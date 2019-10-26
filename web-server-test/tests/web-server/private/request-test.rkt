#lang racket

(require (for-syntax racket/base)
         net/url
         racket/runtime-path
         rackunit
         web-server/private/timer
         (submod web-server/http/request internal-test)
         web-server/http
         web-server/http/request
         web-server/private/connection-manager)

(provide request-tests)

(require/expose web-server/http/request
                (read-bindings&post-data/raw))

(define-runtime-path fixtures
  (build-path "fixtures"))

(define (fixture/ip filename)
  (open-input-file (build-path fixtures filename)))

(define (fixture filename)
  (file->bytes (build-path fixtures filename)))

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
  (define-values (conn headers)
    (make-mock-connection&headers post-data))

  (call-with-values
   (lambda ()
     (read-bindings&post-data/raw (connection-i-port conn)
                                  #"POST"
                                  (string->url "http://localhost")
                                  (cons (make-header #"content-type"
                                                     #"application/x-www-form-urlencoded")
                                        headers)))

   (lambda (fields data)
     fields)))

(define (get-post-data/raw post-data)
  (define-values (conn headers)
    (make-mock-connection&headers post-data))

  (call-with-values
   (lambda ()
     (read-bindings&post-data/raw (connection-i-port conn)
                                  #"POST"
                                  (string->url "http://localhost")
                                  headers))

   (lambda (fields data)
     data)))

(define tm (start-timer-manager))

(define (test-read-request b [read-request read-request])
  (define custodian (make-custodian))
  (parameterize ([current-custodian custodian])
    (define ip (open-input-bytes b))
    (define op (open-output-bytes))
    (define timer (make-timer tm ip +inf.0 void))
    (define conn
      (make-connection 0 timer ip op custodian #f))

    (define-values (req close?)
      (read-request conn 80 (lambda _ (values "to" "from"))))

    (hasheq
     'method (request-method req)
     'uri (string->bytes/utf-8 (url->string (request-uri req)))
     'headers (request-headers/raw req)
     'bindings (request-bindings/raw req)
     'body (request-post-data/raw req)
     'host-ip (request-host-ip req)
     'host-port (request-host-port req)
     'client-ip (request-client-ip req)
     'close? close?)))

(define-syntax-rule (test-request name data e)
  (let* ([e* e]
         [r (for/hasheq ([(k v) (in-hash (test-read-request data))]
                         #:when (hash-has-key? e* k))
              (values k v))])
    (test-equal? name r e*)))

(define-syntax-rule (test-request/fixture filename e)
  (test-request filename (fixture filename) e))

(define-syntax-rule (test-multipart name data boundary ps)
  (let ([r (read-mime-multipart data boundary)])
    (test-equal? name (length r) (length ps))
    (for ([rp (in-list r)]
          [ep (in-list ps)])
      (test-equal? name (mime-part-headers rp) (mime-part-headers ep))
      (test-equal? name
                   (port->bytes (mime-part-contents rp))
                   (port->bytes (mime-part-contents ep))))))

(define-syntax-rule (test-multipart/fixture filename boundary ps)
  (test-multipart filename (fixture/ip filename) boundary ps))

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

     (test-equal?
      "CRLF at boundary between chunks"
      ;; the first chunk should contain #"hello\r" and the second #"\nworld"
      (read-http-line/limited (open-input-string "hello\r\nworld") 100 6)
      #"hello")

     (test-exn
      "input too long"
      (lambda (e)
        (and (exn:fail:network? e)
             (equal? (exn-message e) "read-http-line/limited: line exceeds limit of 5")))
      (lambda _
        (read-http-line/limited (open-input-string "hello world\r\n") 5)))

     (test-exn
      "input too long with minimal bufsize"
      (lambda (e)
        (and (exn:fail:network? e)
             (equal? (exn-message e) "read-http-line/limited: line exceeds limit of 5")))
      (lambda _
        (read-http-line/limited (open-input-string "hello world\r\n") 5 1))))

    (test-suite
     "read-bytes/lazy"

     (test-equal?
      "empty input"
      (read-bytes/lazy 10 (open-input-string ""))
      (read-bytes 10 (open-input-string "")))

     (test-equal?
      "short input"
      (read-bytes/lazy 10 (open-input-string "hi"))
      (read-bytes 10 (open-input-string "hi")))

     (test-equal?
      "short input, short buffer"
      (read-bytes/lazy 10 (open-input-string "hi") 1)
      #"hi")

     (for ([bufsize (in-list '(1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192))])
       (let ([buf (open-input-string "hello there, how are you doing this fine day?")])
         (test-equal?
          "short input, short buffer 1/5"
          (read-bytes/lazy 10 buf bufsize)
          #"hello ther")

         (test-equal?
          "short input, short buffer 2/5"
          (read-bytes/lazy 10 buf bufsize)
          #"e, how are")

         (test-equal?
          "short input, short buffer 3/5"
          (read-bytes/lazy 10 buf bufsize)
          #" you doing")

         (test-equal?
          "short input, short buffer 4/5"
          (read-bytes/lazy 10 buf bufsize)
          #" this fine")

         (test-equal?
          "short input, short buffer 5/5"
          (read-bytes/lazy 10 buf bufsize)
          #" day?")

         (test-not-false
          "short input, short buffer 6/5"
          (eof-object? (read-bytes/lazy 10 buf bufsize))))))

    (test-suite
     "make-spooled-temporary-file"

     (let-values ([(in out) (make-spooled-temporary-file 4096)])
       (display "hello, world!" out)
       (close-output-port out)

       (test-equal?
        "doesn't spill"
        (port->bytes in)
        #"hello, world!"))

     (let-values ([(in out) (make-spooled-temporary-file 5)])
       (display "hello, world!" out)
       (close-output-port out)

       (test-equal?
        "its input port contains all data written to it"
        (port->bytes in)
        #"hello, world!")

       (test-true
        "it creates a temporary file"
        (file-exists? (object-name in)))

       (test-equal?
        "the temporary file contains all the data written to the output port"
        (file->bytes (object-name in))
        #"hello, world!")))

    (test-suite
     "read-request-line"

     (test-exn
      "empty input"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #"http input closed prematurely" (exn-message e))))
      (lambda _
        (read-request-line (open-input-string ""))))

     (test-exn
      "malformed syntax"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #"malformed request" (exn-message e))))
      (lambda _
        (read-request-line (open-input-string "HTTP/1.1 GET /"))))

     (let ([parse-request-uri (lambda (line)
                                (define-values (method uri major minor)
                                  (read-request-line (open-input-string line)))
                                uri)])
       (test-equal?
        "absolute URI"
        (parse-request-uri "GET http://example.com/foo HTTP/1.1")
        (string->url "http://example.com/foo"))

       (test-equal?
        "absolute schemaless URI"
        (parse-request-uri "GET //example.com/foo HTTP/1.1")
        (string->url "//example.com/foo"))

       (test-equal?
        "absolute path"
        (parse-request-uri "GET / HTTP/1.1")
        (string->url "/"))

       (test-equal?
        "asterisk"
        (path->string (url->path (parse-request-uri "GET * HTTP/1.1")))
        "*")))

    (test-suite
     "read-mime-multipart"

     (test-exn
      "multipart-body-empty"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #"malformed header" (exn-message e))))
      (lambda _
        (read-mime-multipart (fixture/ip "multipart-body-empty") #"abc")))

     (test-multipart/fixture
      "multipart-body-fields-only"
      #"abc"
      (list
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"x\""))
                  (open-input-bytes #"42"))
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"y\""))
                  (open-input-bytes #"20"))))

     (test-exn
      "multipart-body-field-with-many-headers"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #"header count exceeds limit of 20" (exn-message e))))
      (lambda _
        (read-mime-multipart (fixture/ip "multipart-body-field-with-many-headers") #"abc")))

     (test-multipart/fixture
      "multipart-body-field-without-name"
      #"abc"
      (list
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data"))
                  (open-input-bytes #"42"))))

     (test-multipart/fixture
      "multipart-body-field-empty"
      #"abc"
      (list
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"a\""))
                  (open-input-bytes #""))))

     (test-exn
      "multipart-body-field-without-data"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #"port closed prematurely" (exn-message e))))
      (lambda _
        (read-mime-multipart (fixture/ip "multipart-body-field-without-data") #"abc")))

     (test-multipart/fixture
      "multipart-body-with-line-breaks"
      #"abc"
      (list
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"f\"; filename=\"a.txt\""))
                  (open-input-bytes #"a\r\nb\r\nc"))))

     (test-multipart/fixture
      "multipart-body-with-long-field"
      #"------------------------4cb1363b48c1c499"
      (list
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"a\""))
                  (open-input-bytes (make-bytes 1000 97)))))

     (test-exn
      "multipart-body-with-long-field, short field limit"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #"field exceeds max length" (exn-message e))))
      (lambda _
        (read-mime-multipart (fixture/ip "multipart-body-with-long-field")
                             #"------------------------4cb1363b48c1c499"
                             #:max-field-length 10)))

     (test-exn
      "multipart-body-with-long-files, small file limit"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #"too many files" (exn-message e))))
      (lambda _
        (read-mime-multipart (fixture/ip "multipart-body-with-long-files")
                             #"------------------------4cb1363b48c1c499"
                             #:max-files 2)))

     (test-not-exn
      "multipart-body-with-long-files, small field limit"
      (lambda _
        (read-mime-multipart (fixture/ip "multipart-body-with-long-files")
                             #"------------------------4cb1363b48c1c499"
                             #:max-fields 2)))

     (test-exn
      "multipart-body-with-long-files, small file length limit"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #"file exceeds max length" (exn-message e))))
      (lambda _
        (read-mime-multipart (fixture/ip "multipart-body-with-long-files")
                             #"------------------------4cb1363b48c1c499"
                             #:max-files 100
                             #:max-file-length 100)))

     (test-multipart/fixture
      "multipart-body-with-multiple-files"
      #"abc"
      (list
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"f\"; filename=\"a.txt\""))
                  (open-input-bytes #"a"))
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"f\"; filename=\"b.txt\""))
                  (open-input-bytes #"b"))
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"f\"; filename=\"c.txt\""))
                  (open-input-bytes #"c"))))

     (test-multipart/fixture
      "multipart-body-without-disposition"
      #"abc"
      (list
       (mime-part (list)
                  (open-input-bytes #"42"))))

     (test-exn
      "multipart-body-without-end-boundary"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #"port closed prematurely" (exn-message e))))
      (lambda _
        (read-mime-multipart (fixture/ip "multipart-body-without-end-boundary") #"abc")))

     (test-multipart/fixture
      "multipart-body-with-mixture-of-fields-and-files"
      #"abc"
      (list
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"f\"; filename=\"a.txt\""))
                  (open-input-bytes #"a"))
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"x\""))
                  (open-input-bytes #"x"))
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"f\"; filename=\"b.txt\""))
                  (open-input-bytes #"b"))
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"y\""))
                  (open-input-bytes #"y"))
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"f\"; filename=\"c.txt\""))
                  (open-input-bytes #"c"))
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"z\""))
                  (open-input-bytes #"z"))))

     (test-not-exn
      "multipart-body-with-mixture-of-fields-and-files, fields within limit"
      (lambda _
        (read-mime-multipart (fixture/ip "multipart-body-with-mixture-of-fields-and-files")
                             #"abc"
                             #:max-fields 3)))

     (test-not-exn
      "multipart-body-with-mixture-of-fields-and-files, files within limit"
      (lambda _
        (read-mime-multipart (fixture/ip "multipart-body-with-mixture-of-fields-and-files")
                             #"abc"
                             #:max-files 3)))

     (test-exn
      "multipart-body-with-mixture-of-fields-and-files, fields above limit"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #"too many fields" (exn-message e))))
      (lambda _
        (read-mime-multipart (fixture/ip "multipart-body-with-mixture-of-fields-and-files")
                             #"abc"
                             #:max-fields 2)))

     (test-exn
      "multipart-body-with-mixture-of-fields-and-files, files above limit"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #"too many files" (exn-message e))))
      (lambda _
        (read-mime-multipart (fixture/ip "multipart-body-with-mixture-of-fields-and-files")
                             #"abc"
                             #:max-files 2)))

     (test-multipart/fixture
      "multipart-body-with-long-preamble"
      #"abc"
      (list
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"x\""))
                  (open-input-bytes #"42"))))))

   (test-suite
    "Headers"

    (test-suite
     "read-headers"

     (test-equal?
      "real-world 1"
      (read-headers (fixture/ip "headers-to-github-dot-com") 100 4096)
      (list
       (header #"Host" #"github.com")
       (header #"User-Agent"
               #"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:69.0) Gecko/20100101 Firefox/69.0")
       (header #"Accept" #"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
       (header #"Accept-Language" #"en-US,en;q=0.5")
       (header #"Accept-Encoding" #"gzip, deflate, br")
       (header #"Connection" #"keep-alive")
       (header #"Cookie" #"logged_in=yes; _octo=xxxxxxxxxxxxxxxxxxxxxxxxxx; user_session=xxxxxx; __Host-user_session_same_site=xxxxxx; dotcom_user=x; _device_id=xxxx; _ga=xxxxxxxxxxxxxxxxxxxxxxxxxx; _gh_sess=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx; ignored_unsupported_browser_notice=false; tz=Europe%2FBucharest")
       (header #"Upgrade-Insecure-Requests" #"1")
       (header #"Pragma" #"no-cache")
       (header #"Cache-Control" #"no-cache")))

     (test-equal?
      "real-world 1"
      (read-headers (fixture/ip "headers-to-reddit-dot-com") 100 4096)
      (list
       (header #"Host" #"www.reddit.com")
       (header #"User-Agent" #"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:69.0) Gecko/20100101 Firefox/69.0")
       (header #"Accept" #"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
       (header #"Accept-Language" #"en-US,en;q=0.5")
       (header #"Accept-Encoding" #"gzip, deflate, br")
       (header #"Connection" #"keep-alive")
       (header #"Cookie" #"loid=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx; eu_cookie_v2=x; edgebucket=xxxxxxxxxxxxxxxxxx; recent_srs=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx; reddaid=xxxxxxxxxxxxxxxx; reddit_session=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx; redesign_optout=xxxx; xxxxxxx_recentclicks2=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx; USER=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx; session=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx; d2_token=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx; session_tracker=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx; pc=xx")
       (header #"Upgrade-Insecure-Requests" #"1")
       (header #"Pragma" #"no-cache")
       (header #"Cache-Control" #"no-cache")
       (header #"TE" #"Trailers"))))

    (test-suite
     "headers-assq"

     (test-equal?
      "simple"
      (header-value (headers-assq #"key" (list (make-header #"key" #"val"))))
      #"val")

     (test-false
      "not present"
      (headers-assq #"key" (list)))

     (test-false
      "case (not present)"
      (headers-assq* #"Key" (list)))

     (test-equal?
      "case"
      (header-value (headers-assq* #"Key" (list (make-header #"key" #"val"))))
      #"val")

     (test-equal?
      "case (not first)"
      (header-value (headers-assq* #"Key" (list (make-header #"key1" #"val") (make-header #"key" #"val"))))
      #"val")))

   (test-suite
    "Bindings"

    (test-suite
     "parse-bindings"

     (test-equal?
      "empty"
      (parse-bindings #"")
      (list))

     (test-equal?
      "basic"
      (parse-bindings #"a=1&b=2")
      (list (make-binding:form #"a" #"1")
            (make-binding:form #"b" #"2")))

     (test-equal?
      "repeated"
      (parse-bindings #"a=1&a=2&b=3")
      (list (make-binding:form #"a" #"1")
            (make-binding:form #"a" #"2")
            (make-binding:form #"b" #"3")))

     (test-equal?
      "value-less"
      (parse-bindings #"a=1&a=2&b=3&c&d=4")
      (list (make-binding:form #"a" #"1")
            (make-binding:form #"a" #"2")
            (make-binding:form #"b" #"3")
            (make-binding:form #"c" #"")
            (make-binding:form #"d" #"4")))

     (test-equal?
      "value-less"
      (parse-bindings #"a=1&a=2&b=3&c=&d=4")
      (list (make-binding:form #"a" #"1")
            (make-binding:form #"a" #"2")
            (make-binding:form #"b" #"3")
            (make-binding:form #"c" #"")
            (make-binding:form #"d" #"4")))

     (test-equal?
      "value-less at the beginning"
      (parse-bindings #"a&b=1&c=2")
      (list (make-binding:form #"a" #"")
            (make-binding:form #"b" #"1")
            (make-binding:form #"c" #"2")))

     (test-equal?
      "value-less at the end"
      (parse-bindings #"a=1&a=2&b=3&c=&d=")
      (list (make-binding:form #"a" #"1")
            (make-binding:form #"a" #"2")
            (make-binding:form #"b" #"3")
            (make-binding:form #"c" #"")
            (make-binding:form #"d" #"")))

     (test-equal?
      "value-less at the end 2"
      (parse-bindings #"a=1&a=2&b=3&c=&d")
      (list (make-binding:form #"a" #"1")
            (make-binding:form #"a" #"2")
            (make-binding:form #"b" #"3")
            (make-binding:form #"c" #"")
            (make-binding:form #"d" #"")))

     (test-equal?
      "with encoded data"
      (parse-bindings #"a=hi%20there&b=what%27s%20up")
      (list (make-binding:form #"a" #"hi there")
            (make-binding:form #"b" #"what's up")))

     (test-equal?
      "with encoded data 2"
      (parse-bindings #"x=%26encoded%3D&y=1")
      (list (make-binding:form #"x" #"&encoded=")
            (make-binding:form #"y" #"1"))))

    (test-suite
     "bindings-assq"

     (test-equal?
      "Simple"
      (let ([binds (list (make-binding:form #"key" #"val"))])
        (binding:form-value (bindings-assq #"key" binds)))
      #"val")

     (test-equal?
      "Simple (File)"
      (let ([binds (list (make-binding:file #"key" #"name" empty #"val"))])
        (binding:file-content (bindings-assq #"key" binds)))
      #"val")

     (test-equal?
      "Simple (File/port)"
      (let ([binds (list (make-binding:file/port #"key" #"name" empty (open-input-bytes #"val")))])
        (binding:file-content (bindings-assq #"key" binds)))
      #"val")

     (test-false
      "Not present"
      (bindings-assq #"key" (list)))))

   ; XXX This needs to be really extensive, see what Apache has
   (test-suite
    "Parsing"

    (test-suite
     "Request Line"

     (test-request
      "asterisk path"
      #"OPTIONS * HTTP/1.1\r\n\r\n"
      (hasheq
       'method #"OPTIONS"
       'uri #"*"))

     (test-exn
      "request line too short"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match? #rx"line exceeds limit of 5" (exn-message e))))
      (lambda _
        (test-read-request #"OPTIONS /some/path HTTP/1.1\r\n\r\n"
                           (make-read-request #:max-request-line-length 5)))))

    (test-suite
     "Headers"

     (test-request
      "line-folded header values"
      (fixture "post-with-multi-line-header")
      (hasheq
       'method #"POST"
       'uri #"/"
       'headers (list (header #"Date" #"Fri, 31 Dec 1999 23:59:59 GMT")
                      (header #"Content-Type" #"text/plain")
                      (header #"Content-Length" #"42")
                      (header #"X-Multi-Line" #"hello there")
                      (header #"X-Forty-Two" #"42"))
       'body #"abcdefghijklmnopqrstuvwxyz1234567890abcdef"))

     (test-request
      "line-folded header values (tabbed)"
      (fixture "post-with-multi-line-header-tabbed")
      (hasheq
       'method #"POST"
       'uri #"/"
       'headers (list (header #"Date" #"Fri, 31 Dec 1999 23:59:59 GMT")
                      (header #"Content-Type" #"text/plain")
                      (header #"Content-Length" #"42")
                      (header #"X-Multi-Line" #"hello	there,	how are you?")
                      (header #"X-Forty-Two" #"42"))
       'body #"abcdefghijklmnopqrstuvwxyz1234567890abcdef"))

     (test-exn
      "too many headers"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match? #rx"header count exceeds limit" (exn-message e))))
      (lambda _
        (test-read-request
         (fixture "get-with-many-headers")
         (make-read-request #:max-request-fields 10))))

     (test-exn
      "header too long"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match? #rx"line exceeds limit of 10" (exn-message e))))
      (lambda _
        (test-read-request
         (fixture "get-with-long-single-line-header")
         (make-read-request #:max-request-field-length 10))))

     (test-exn
      "folded header too long"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match? #rx"header too long" (exn-message e))))
      (lambda _
        (test-read-request
         (fixture "get-with-long-multi-line-header")
         (make-read-request #:max-request-field-length 10)))))

    (test-suite
     "Chunked transfer-encoding"

     (test-request/fixture
      "post-with-chunked-transfer-encoding"
      (hasheq
       'method #"POST"
       'uri #"/test"
       'headers (list (header #"Date" #"Fri, 31 Dec 1999 23:59:59 GMT")
                      (header #"Content-Type" #"text/plain")
                      (header #"Transfer-Encoding" #"chunked")
                      (header #"Content-Length" #"42")
                      (header #"Some-Footer" #"some-value")
                      (header #"Another-Footer" #"another-value"))
       'body #"abcdefghijklmnopqrstuvwxyz1234567890abcdef"))

     (test-exn
      "too many headers after chunked body"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #rx"header count exceeds limit" (exn-message e))))
      (lambda _
        (test-read-request
         (fixture "post-with-chunked-transfer-encoding")
         (make-read-request #:max-request-fields 3))))

     (test-exn
      "chunked request too large"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #rx"chunked content exceeds max body length" (exn-message e))))
      (lambda _
        (test-read-request
         (fixture "post-with-chunked-transfer-encoding")
         (make-read-request #:max-request-body-length 10)))))

    (test-suite
     "JSON data"

     (test-request/fixture
      "post-with-json-body"
      (hasheq
       'method #"POST"
       'uri #"/books"
       'headers (list (header #"Date" #"Fri, 31 Dec 1999 23:59:59 GMT")
                      (header #"Content-Type" #"application/json; charset=utf-8")
                      (header #"Content-Length" #"35"))
       'body #"{\"title\": \"How to Design Programs\"}"))

     (test-request/fixture
      "post-with-json-body-and-query-param"
      (hasheq
       'method #"POST"
       'uri #"/books?upsert=1"
       'headers (list (header #"Date" #"Fri, 31 Dec 1999 23:59:59 GMT")
                      (header #"Content-Type" #"application/json; charset=utf-8")
                      (header #"Content-Length" #"35"))
       'bindings (list (binding:form #"upsert" #"1"))
       'body #"{\"title\": \"How to Design Programs\"}"))

     (test-exn
      "post-with-json-body, exceeding size limit"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #"body length exceeds limit" (exn-message e))))
      (lambda _
        (test-read-request (fixture "post-with-json-body")
                           (make-read-request #:max-request-body-length 10)))))

    (test-suite
     "GET bindings"

     (test-request/fixture
      "get-with-query-params"
      (hasheq 'bindings (list (binding:form #"b" #"1")
                              (binding:form #"c" #"2")
                              (binding:form #"x" #"&encoded=")
                              (binding:form #"y" #"1")))))

    (test-suite
     "POST Bindings"

     (test-equal?
      "simple test 1"
      (get-post-data/raw "hello world")
      #"hello world")

     (test-equal?
      "simple test 2"
      (get-post-data/raw "hello=world")
      #"hello=world")

     (test-equal?
      "simple test 3"
      (binding:form-value (bindings-assq #"hello" (force (get-bindings "hello=world"))))
      #"world")

     (test-request/fixture
      "post-with-body-params-and-query-params"
      (hasheq 'bindings (list (binding:form #"a" #"1")
                              (binding:form #"b" #"2")
                              (binding:form #"c" #"3"))))

     (test-exn
      "post-with-invalid-content-length"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #rx"POST request contained a non-numeric content-length" (exn-message e))))
      (lambda _
        (test-read-request (fixture "post-with-invalid-content-length"))))

     (test-exn
      "post-with-body-shorter-than-content-length"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #rx"port closed prematurely" (exn-message e))))
      (lambda _
        (test-read-request (fixture "post-with-body-shorter-than-content-length")))))

    (test-suite
     "File Uploads"

     (let ([r (test-read-request (fixture "post-with-short-file"))])
       (check-equal? (length (hash-ref r 'bindings)) 1)
       (check-equal? (binding-id (car (hash-ref r 'bindings))) #"file")
       (check-equal? (binding:file-filename (car (hash-ref r 'bindings))) #"blob1.dat")
       (check-equal? (binding:file-content (car (hash-ref r 'bindings))) #"aaaa"))

     (let ([r (test-read-request (fixture "post-with-long-file"))])
       (check-equal? (length (hash-ref r 'bindings)) 1)
       (check-equal? (binding-id (car (hash-ref r 'bindings))) #"somename")
       (check-equal? (binding:file-filename (car (hash-ref r 'bindings))) #"racket-logo.svg")
       (check-equal? (bytes-length (binding:file-content (car (hash-ref r 'bindings)))) 1321))

     (test-exn
      "post-with-multipart-data-without-disposition"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #"Couldn't extract form field name for file upload" (exn-message e))))
      (lambda _
        (test-read-request (fixture "post-with-multipart-data-without-disposition"))))

     (test-exn
      "post-with-multipart-data-without-body"
      (lambda (e)
        (and (exn:fail:network? e)
             (regexp-match #"port closed prematurely" (exn-message e))))
      (lambda _
        (test-read-request (fixture "post-with-multipart-data-without-body"))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests request-tests))
