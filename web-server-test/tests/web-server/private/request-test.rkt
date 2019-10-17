#lang racket
(require rackunit
         net/url
         web-server/private/connection-manager
         web-server/private/timer
         web-server/http/request
         web-server/http)
(provide request-tests)

(require/expose web-server/http/request
                (read-bindings&post-data/raw))

;; mock connection object for test on post body parsing
(define (make-mock-connection&headers post-body)
  (let* ([b (string->bytes/utf-8 post-body)]
         [headers (list (make-header
                         #"Content-Length"
                         (string->bytes/utf-8
                          (number->string (bytes-length b)))))]
         [ip (open-input-bytes b)]
         [op (open-output-bytes)])
    (values (make-connection 0 (make-timer tm ip +inf.0 (lambda () (void)))
                             ip op (make-custodian) #f)
            headers)))


(define (get-bindings post-data)
  (define-values (conn headers) (make-mock-connection&headers post-data))
  (call-with-values
      (lambda ()
        (read-bindings&post-data/raw (connection-i-port conn) #"POST" (string->url "http://localhost") headers))
    (lambda (f s) f)))

(define (get-post-data/raw post-data)
  (define-values (conn headers) (make-mock-connection&headers post-data))
  (call-with-values 
      (lambda () 
        (read-bindings&post-data/raw (connection-i-port conn) #"POST" (string->url "http://localhost") headers))
    (lambda (f s) s)))

(define tm (start-timer-manager))

(define (test-read-request b)
  (define ip (open-input-bytes b))
  (define op (open-output-bytes))
  (define c
    (make-connection 0 (make-timer tm ip +inf.0 (lambda () (void)))
                     ip op (make-custodian) #f))
  (define-values (req flag)
    (read-request c 80 (λ (_) (values "to" "from"))))
  (list (list 'request
              (map (λ (f) (f req))
                   (list request-method (compose url->string request-uri)
                         request-headers/raw
                         request-bindings/raw request-post-data/raw
                         request-host-ip request-host-port request-client-ip)))
        flag))

(define request-tests
  (test-suite
   "HTTP Requests"

   (test-suite
    "Headers"
    (test-equal? "Simple" (header-value (headers-assq #"key" (list (make-header #"key" #"val")))) #"val")
    (test-false "Not present" (headers-assq #"key" (list)))
    (test-false "Case (not present)" (headers-assq* #"Key" (list)))
    (test-equal? "Case" (header-value (headers-assq* #"Key" (list (make-header #"key" #"val")))) #"val")
    (test-equal? "Case (not first)"
                 (header-value (headers-assq* #"Key" (list (make-header #"key1" #"val") (make-header #"key" #"val")))) #"val"))

   (test-suite
    "Bindings"
    (test-equal? "Simple" (binding:form-value (bindings-assq #"key" (list (make-binding:form #"key" #"val")))) #"val")
    (test-equal? "Simple (File)" (binding:file-content (bindings-assq #"key" (list (make-binding:file #"key" #"name" empty #"val")))) #"val")
    (test-false "Not present" (bindings-assq #"key" (list))))

                                        ; XXX This needs to be really extensive, see what Apache has
   (test-suite
    "Parsing"
    (test-suite
     "URL Query"
     (test-not-exn "Unfinished URL query"
                   (lambda ()
                     (define ip (open-input-string "GET http://127.0.0.1:8080/servlets/examples/hello.rkt?a=1&b: HTTP/1.1"))
                     (read-request
                      (make-connection 0 (make-timer tm ip +inf.0 (lambda () (void)))
                                       ip
                                       (open-output-bytes) (make-custodian) #f)
                      8081
                      (lambda _ (values "s1" "s2")))
                     (void))))

    (test-suite
     "Chunked transfer-encoding"
     (test-equal? "example"
                  (test-read-request
                   #"POST http://127.0.0.1/test HTTP/1.0
Date: Fri, 31 Dec 1999 23:59:59 GMT
Content-Type: text/plain
Content-Length: 42

abcdefghijklmnopqrstuvwxyz1234567890abcdef
")
                  (list
                   (list
                    'request
                    (list
                     #"POST"
                     "http://127.0.0.1/test"
                     (list
                      (header #"Date" #"Fri, 31 Dec 1999 23:59:59 GMT")
                      (header #"Content-Type" #"text/plain")
                      (header #"Content-Length" #"42"))
                     '()
                     #"abcdefghijklmnopqrstuvwxyz1234567890abcdef"
                     "to"
                     80
                     "from"))
                   #t))
     (test-equal? "example"
                  (test-read-request
                   #"POST http://127.0.0.1/test HTTP/1.1
Date: Fri, 31 Dec 1999 23:59:59 GMT
Content-Type: text/plain
Transfer-Encoding: chunked

1a; ignore-stuff-here
abcdefghijklmnopqrstuvwxyz
10
1234567890abcdef
0
some-footer: some-value
another-footer: another-value
")
                  (list
                   (list
                    'request
                    (list
                     #"POST"
                     "http://127.0.0.1/test"
                     (list
                      (header #"Date" #"Fri, 31 Dec 1999 23:59:59 GMT")
                      (header #"Content-Type" #"text/plain")
                      (header #"Transfer-Encoding" #"chunked")
                      (header #"Content-Length" #"42")
                      (header #"some-footer" #"some-value")
                      (header #"another-footer" #"another-value"))
                     '()
                     #"abcdefghijklmnopqrstuvwxyz1234567890abcdef"
                     "to"
                     80
                     "from"))
                   #f)))

    (test-suite
     "Content-Transfer-Encoding"
     (test-equal?
      "filename with quotes"
      (test-read-request
       (regexp-replace*
        #px#"\n"
        #"POST /foo HTTP/1.1
Host: localhost:8022
User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.11; rv:48.0) Gecko/20100101 Firefox/48.0
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Referer: http://localhost:8000/servlets/standalone.rkt
Connection: keep-alive
Upgrade-Insecure-Requests: 1
Content-Type: multipart/form-data; boundary=---------------------------182000195915857045221383087
Content-Length: 254

-----------------------------182000195915857045221383087
Content-Disposition: form-data; name=\"abcz\";
 zokbar = no_quotes_needed;
 filename=\"abc\\\"d\"
Content-Type: application/octet-stream

these lines are
in the file

-----------------------------182000195915857045221383087--
"
        #"\r\n"))
      (list
       (list
        'request
        (list
         #"POST"
         "/foo"
         (list
          (header #"Host" #"localhost:8022")
          (header
           #"User-Agent"
           #"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.11; rv:48.0) Gecko/20100101 Firefox/48.0")
          (header
           #"Accept"
           #"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
          (header #"Accept-Language" #"en-US,en;q=0.5")
          (header #"Accept-Encoding" #"gzip, deflate")
          (header #"Referer" #"http://localhost:8000/servlets/standalone.rkt")
          (header #"Connection" #"keep-alive")
          (header #"Upgrade-Insecure-Requests" #"1")
          (header
           #"Content-Type"
           #"multipart/form-data; boundary=---------------------------182000195915857045221383087")
          (header #"Content-Length" #"254"))
         (list
          (binding:file
           #"abcz"
           #"abc\"d"
           (list
            (header
             #"Content-Disposition"
             #"form-data; name=\"abcz\"; zokbar = no_quotes_needed; filename=\"abc\\\"d\"")
            (header #"Content-Type" #"application/octet-stream"))
           #"these lines are\r\nin the file\r\n"))
         #f
         "to"
         80
         "from"))
       #f)))

    (test-suite
     "POST Bindings"
     (test-equal? "simple test 1"
                  (get-post-data/raw "hello world") #"hello world")
     (test-equal? "simple test 2"
                  (get-post-data/raw "hello=world") #"hello=world")
     (test-equal? "simple test 3"
                  (binding:form-value (bindings-assq #"hello" (force (get-bindings "hello=world"))))
                  #"world")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests request-tests))


