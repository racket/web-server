#lang racket

(require net/url
         rackunit
         web-server/http/request
         web-server/private/timer
         (submod web-server/http/request internal-test)
         web-server/http
         web-server/private/connection-manager)

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

(define (get-bindings post-data [max-body-length 1024])
  (define-values (conn headers)
    (make-mock-connection&headers post-data))
  (call-with-values
   (lambda ()
     (read-bindings&post-data/raw (connection-i-port conn)
                                  #"POST"
                                  (string->url "http://localhost")
                                  (cons (make-header #"content-type"
                                                     #"application/x-www-form-urlencoded")
                                        headers)
                                  max-body-length))
   (lambda (f s) f)))

(define (get-post-data/raw post-data [max-body-length 1024])
  (define-values (conn headers) (make-mock-connection&headers post-data))
  (call-with-values
   (lambda ()
     (read-bindings&post-data/raw (connection-i-port conn) #"POST" (string->url "http://localhost") headers max-body-length))
   (lambda (f s) s)))

(define tm (start-timer-manager))

(define (test-read-request b [read-request read-request])
  (define ip (open-input-bytes b))
  (define op (open-output-bytes))
  (define c
    (make-connection 0 (make-timer tm ip +inf.0 (lambda () (void)))
                     ip op (make-custodian) #f))
  (define-values (req close?)
    (read-request c 80 (λ (_) (values "to" "from"))))
  (list (list 'request
              (map (λ (f) (f req))
                   (list request-method (compose url->string request-uri)
                         request-headers/raw
                         request-bindings/raw request-post-data/raw
                         request-host-ip request-host-port request-client-ip)))
        close?))

(define request-tests
  (test-suite
   "HTTP Requests"

   (test-suite
    "Utilities"

    (test-suite
     "read-http-line/limited"

     (test-equal?
      "empty input"
      (read-http-line/limited (open-input-string ""))
      (read-bytes-line (open-input-string "")))

     (test-equal?
      "input without line endings"
      (read-http-line/limited (open-input-string "hello world"))
      (read-bytes-line (open-input-string "hello world") 'return-linefeed))

     (test-equal?
      "input with other line endings"
      (read-http-line/limited (open-input-string "hello world\n how's it going?"))
      (read-bytes-line (open-input-string "hello world\n how's it going?") 'return-linefeed))

     (test-equal?
      "input with proper line endings"
      (read-http-line/limited (open-input-string "hello world\r\n"))
      (read-bytes-line (open-input-string "hello world\r\n") 'return-linefeed))

     (test-equal?
      "empty line with line endings"
      (read-http-line/limited (open-input-string "\r\n"))
      #"")

     (test-exn
      "input too long"
      (lambda (e)
        (and (exn:fail:network? e)
             (equal? (exn-message e) "read-http-line/limited: line exceeds limit of 5")))
      (lambda _
        (read-http-line/limited (open-input-string "hello world\r\n") 5)))))

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
     "Request Line"

     (test-equal?
      "asterisk path"
      (test-read-request
       #"OPTIONS * HTTP/1.1\r\n\r\n")
      (list (list 'request (list #"OPTIONS" "*" '() '() #f "to" 80 "from")) #f))

     (test-exn
      "request line too short"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match? #rx"line exceeds limit of 5" (exn-message e))))
      (lambda _
        (test-read-request #"OPTIONS /some/path HTTP/1.1\r\n\r\n"
                           (make-read-request #:max-request-line-length 5)))))

    (test-suite
     "URL Query"

     (test-not-exn
      "Unfinished URL query"
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
     "Headers"

     (test-equal?
      "multi-line header values"
      (test-read-request
       #"POST / HTTP/1.1\r
Content-Type: text/plain\r
Content-Length: 42\r
X-Multi-Line: hello\r
 there\r
X-Forty-Two: 42\r
\r
abcdefghijklmnopqrstuvwxyz1234567890abcdef\r
")
      (list
       (list
        'request
        (list
         #"POST"
         "/"
         (list
          (header #"Content-Type" #"text/plain")
          (header #"Content-Length" #"42")
          (header #"X-Multi-Line" #"hello there")
          (header #"X-Forty-Two" #"42"))
         '()
         #"abcdefghijklmnopqrstuvwxyz1234567890abcdef"
         "to"
         80
         "from"))
       #f))

     (test-exn
      "too many headers"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match? #rx"header count exceeds limit of 2" (exn-message e))))
      (lambda _
        (test-read-request
         #"GET / HTTP/1.1\r
A: 1\r
B: 2\r
C: 3\r
\r"
         (make-read-request #:max-request-fields 2))))

     (test-exn
      "header too long"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match? #rx"line exceeds limit of 10" (exn-message e))))
      (lambda _
        (test-read-request
         #"GET / HTTP/1.1\r
A: notlong\r
B: waytoofreakinglong\r
\r"
         (make-read-request #:max-request-field-length 10))))

     (test-exn
      "folded header too long"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match? #rx"line exceeds limit of 10" (exn-message e))))
      (lambda _
        (test-read-request
         #"GET / HTTP/1.1\r
A: notlong\r
B: way\r
 too\r
 freaking\
 long\r
\r"
         (make-read-request #:max-request-field-length 10)))))

    (test-suite
     "Chunked transfer-encoding"

     (test-equal?
      "example"
      (test-read-request
       #"POST http://127.0.0.1/test HTTP/1.0\r
Date: Fri, 31 Dec 1999 23:59:59 GMT\r
Content-Type: text/plain\r
Content-Length: 42\r
\r
abcdefghijklmnopqrstuvwxyz1234567890abcdef\r
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

     (test-equal?
      "example"

      (test-read-request
       #"POST http://127.0.0.1/test HTTP/1.1\r
Date: Fri, 31 Dec 1999 23:59:59 GMT\r
Content-Type: text/plain\r
Transfer-Encoding: chunked\r
\r
1a; ignore-stuff-here\r
abcdefghijklmnopqrstuvwxyz\r
10\r
1234567890abcdef\r
0\r
some-footer: some-value\r
another-footer: another-value\r
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
