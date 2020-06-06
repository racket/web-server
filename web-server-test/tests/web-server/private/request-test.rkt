#lang racket

(require rackunit
         net/url
         racket/runtime-path
         web-server/private/timer
         (submod web-server/http/request internal-test)
         web-server/http
         web-server/http/request
         web-server/private/connection-manager
         web-server/safety-limits
         syntax/parse/define
         (for-syntax racket/base))

(provide request-tests)

(module+ test
  (require rackunit/text-ui)
  (run-tests request-tests))

(define-runtime-path fixtures
  "fixtures/")

(define (fixture/ip filename [chunker randomly-chunked-ip])
  (chunker (open-input-file (build-path fixtures filename))))

(define (fixture filename)
  (file->bytes (build-path fixtures filename)))



(define/contract (exn:fail:network/c cf)
  (-> (or/c string? regexp?) (-> any/c boolean?))
  (match-lambda
    [(exn:fail:network msg _)
     (if (string? cf)
         (equal? cf msg)
         (regexp-match? cf msg))]
    [_
     #f]))

(define-check (check-exn:fail:network cf thunk)
  (define pred (exn:fail:network/c cf))
  (with-check-info*
   (list (make-check-expected cf))
   (λ ()
     (check-exn pred thunk))))

(define-syntax-parser test-exn:fail:network
  [(_ name:expr cf:expr thunk:expr)
   (quasisyntax/loc this-syntax
     (test-case name #,(syntax/loc this-syntax
                         (check-exn:fail:network cf thunk))))])



;; helpers to setup tests on post body parsing
(define-values [get-bindings get-post-data/raw]
  (let ()
    (define ((make-get-bindings/post-data transform-hs receive) post-body)
      (define body-bs
        (string->bytes/utf-8 post-body))
      (define hs
        (transform-hs
         (list (make-header
               #"Content-Length"
               (string->bytes/utf-8
                (number->string (bytes-length body-bs)))))))
      (call-with-values
       (λ ()
         (read-bindings&post-data/raw (open-input-bytes body-bs)
                                      #"POST"
                                      (string->url "http://localhost")
                                      hs))
       receive))
    (define get-bindings
      (make-get-bindings/post-data
       (λ (hs)
         (cons (header #"content-type"
                       #"application/x-www-form-urlencoded")
               hs))
       (λ (fields data)
         fields)))
    (define get-post-data/raw
      (make-get-bindings/post-data
       values
       (λ (fields data)
         data)))
    (values get-bindings get-post-data/raw)))




;; Currently the only safety issues addressed by
;; `start-connection-manager` and `new-connection`
;; are for responses, which we don't excercise in this file,
;; so it's ok to keep using `make-connection` and
;; a shared timer manager here.
(define tm (start-timer-manager))

(define (test-read-request b [read-request standard-read-request])
  (define custodian (make-custodian))
  (parameterize ([current-custodian custodian])
    (define ip (open-input-bytes b))
    (define op (open-output-bytes))
    (define timer (make-timer tm +inf.0 void))
    (define conn
      (connection 0 timer ip op custodian #f))

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


(define (do-test-read-request/limits b limits)
  (test-read-request b (make-read-request #:safety-limits limits)))


(define-check (check-request data expected)
  (define actual
    (test-read-request data))
  (define diffs
    ;; Iterating over `expected` ensures all expected keys
    ;; are present, preventing typos from leading to skipped tests.
    (for*/hasheq ([(k v-e) (in-immutable-hash expected)]
                  [v-a (in-value (hash-ref actual k (λ () (string-info "<absent>"))))]
                  #:unless (equal? v-e v-a))
      (define nstd
        (nested-info (list (make-check-actual v-a)
                           (make-check-expected v-e))))
      (values k (check-info k nstd))))
  (unless (hash-empty? diffs)
    (define sames
      (for/hasheq ([(k v-e) (in-immutable-hash expected)]
                   #:unless (hash-has-key? diffs k))
        (values k (check-info k v-e))))
    (define (hash-values/ordered hsh)
      (hash-map hsh (λ (k v) v) #t))
    (with-check-info
        (['problems (nested-info (hash-values/ordered diffs))]
         ['|other fields|
          (if (hash-empty? sames)
              (string-info "none")
              (nested-info (hash-values/ordered sames)))])
      (fail-check))))



(define-syntax-parser test-request
  [(_ name:expr data:expr expected:expr)
   (quasisyntax/loc this-syntax
     (test-case name #,(syntax/loc this-syntax
                         (check-request data expected))))])

(define-syntax-parser test-request/fixture
  [(_ filename:expr expected:expr)
   (syntax/loc this-syntax
     (test-request filename (fixture filename) expected))])




(define-check (check-multipart data boundary expected-parts)
  (define (check msg #:e e #:a a)
    (unless (equal? e a)
      (with-check-info*
       (list (check-info 'problem (string-info msg))
             (make-check-actual a)
             (make-check-expected e))
       fail-check)))
  (define actual-parts
    (read-mime-multipart data boundary #:safety-limits (make-safety-limits)))
  (check #:e (length expected-parts) #:a (length actual-parts)
         "wrong number of parts")
  (for ([n (in-naturals)]
        [e (in-list expected-parts)]
        [a (in-list actual-parts)])
    (with-check-info (['|part index| n])
      (match-define (mime-part e-hs (app port->bytes e-bs)) e)
      (match-define (mime-part a-hs (app port->bytes a-bs)) a)
      (check #:e e-hs #:a a-hs "wrong headers")
      (check #:e e-bs #:a a-bs "wrong contents"))))



(define-syntax-parser test-multipart
  [(_ name:expr data:expr boundary:expr expected:expr)
   (quasisyntax/loc this-syntax
     (test-case name #,(syntax/loc this-syntax
                         (check-multipart data boundary expected))))])

(define-syntax-parser test-multipart/fixture
  [(_ filename:expr boundary:expr expected:expr (~optional chunker:expr))
   (syntax/loc this-syntax
     (test-multipart filename (fixture/ip filename (~? chunker randomly-chunked-ip)) boundary expected))])


(define ((make-ip-chunker sizer) inp)
  (define-values (in out)
    (make-pipe))
  (begin0 in
    (thread
     (lambda ()
       (let loop ()
         (define bs (read-bytes (sizer) inp))
         (cond
           [(eof-object? bs)
            (close-output-port out)]

           [else
            (write-bytes bs out)
            (flush-output out)
            (sync (system-idle-evt))
            (loop)]))))))


;; Wraps `inp' so that only one byte at a time is available from it.
(define drip-ip
  (make-ip-chunker (lambda () 1)))


;; Wraps `inp' so that parts of it are available in random chunk
;; sizes, like they would be under realistic network conditions.
(define randomly-chunked-ip
  (make-ip-chunker (lambda ()
                     (random 1 128))))


;; Creates an input port whose contents are the appended chunks.  The
;; data is available for reading one chunk at a time.
(define (feed-ip chunks)
  (define-values (in out)
    (make-pipe))
  (begin0 in
    (thread
     (lambda ()
       (for ([c (in-list chunks)])
         (write-bytes c out)
         (flush-output out)
         (sync (system-idle-evt)))
       (close-output-port out)))))


;
;
;
;
;   ;;             ;;                  ;; ;;
;   ;;             ;;                     ;;
;  ;;;;; ;;    ;; ;;;;;     ;; ;;; ;;  ;;;;;;; ;;
;   ;;  ;  ; ;;  ; ;;     ;;  ; ;; ;;  ;; ;;  ;  ;
;   ;;  ;  ;  ;    ;;      ;    ;; ;;  ;; ;;  ;  ;
;   ;; ;;;;;;  ;;  ;;  ;;;  ;;  ;; ;;  ;; ;; ;;;;;;
;   ;;  ;        ;;;;         ;;;; ;;  ;; ;;  ;
;    ;  ;    ;   ;  ;     ;   ;  ; ;;  ;;  ;  ;
;    ;;; ;;;  ;;;   ;;;    ;;;   ;;;;  ;;  ;;; ;;;
;
;
;
;

(define request-tests
  (test-suite
   "HTTP Requests"

   (test-suite
    "Utilities"

    (test-suite
     "read-http-line/limited"

     (test-equal?
      "empty input"
      (read-http-line/limited (open-input-string "") #:limit (* 8 1024))
      (read-bytes-line (open-input-string "")))

     (test-equal?
      "input without line endings"
      (read-http-line/limited (open-input-string "hello world") #:limit (* 8 1024))
      (read-bytes-line (open-input-string "hello world") 'return-linefeed))

     (test-equal?
      "input with other line endings"
      (read-http-line/limited (open-input-string "hello world\n how's it going?")
                              #:limit (* 8 1024))
      (read-bytes-line (open-input-string "hello world\n how's it going?") 'return-linefeed))

     (test-equal?
      "input with proper line endings"
      (read-http-line/limited (open-input-string "hello world\r\n") #:limit (* 8 1024))
      (read-bytes-line (open-input-string "hello world\r\n") 'return-linefeed))

     (test-equal?
      "empty line with line endings"
      (read-http-line/limited (open-input-string "\r\n") #:limit (* 8 1024))
      #"")

     (test-equal?
      "CRLF at boundary between chunks"
      ;; the first chunk should contain #"hello\r" and the second #"\nworld"
      (read-http-line/limited (open-input-string "hello\r\nworld") 6 #:limit 100)
      #"hello")

     (test-exn:fail:network
      "input too long"
      "read-http-line/limited: line exceeds limit of 5"
      (lambda ()
        (read-http-line/limited (open-input-string "hello world\r\n") #:limit 5)))

     (test-exn:fail:network
      "input too long with minimal bufsize"
      "read-http-line/limited: line exceeds limit of 5"
      (lambda ()
        (read-http-line/limited (open-input-string "hello world\r\n") 1 #:limit 5))))

    (test-suite
     "read-bytes/lazy"

     (test-equal?
      "empty input"
      (read-bytes/lazy 10 (open-input-string ""))
      (read-bytes 10 (open-input-string "")))

     (test-equal?
      "empty input, read zero bytes"
      (read-bytes/lazy 0 (open-input-string ""))
      (read-bytes 0 (open-input-string "")))

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

     (test-exn:fail:network
      "empty input"
      #rx"http input closed prematurely"
      (lambda ()
        (read-request-line (open-input-string "") 1024)))

     (test-exn:fail:network
      "malformed syntax"
      #rx"malformed request"
      (lambda ()
        (read-request-line (open-input-string "HTTP/1.1 GET /") 1024)))

     (let ()
       (define (parse-request-uri line)
         (define-values (method uri major minor)
           (read-request-line (open-input-string line) 1024))
         uri)

       (test-equal?
        "absolute URI"
        (parse-request-uri "GET http://example.com/foo HTTP/1.1")
        (string->url "http://example.com/foo"))

       (test-equal?
        "absolute schemaless URI"
        (parse-request-uri "GET //example.com/foo HTTP/1.1")
        (url #f #f "" #f #t (list (path/param "" '()) (path/param
                                                       "example.com" '()) (path/param "foo" '())) '() #f))

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

     (test-exn:fail:network
      "multipart boundary too long"
      #rx"boundary too long"
      (lambda ()
        (read-mime-multipart (open-input-bytes #"") (make-bytes 300 65))))

     (test-exn:fail:network
      "multipart-body-empty"
      #rx"malformed header"
      (lambda ()
        (read-mime-multipart (fixture/ip "multipart-body-empty") #"abc")))

     (test-multipart/fixture
      "multipart-body-fields-only"
      #"abc"
      (list
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"x\""))
                  (open-input-bytes #"42"))
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"y\""))
                  (open-input-bytes #"20"))))

     (test-exn:fail:network
      "multipart-body-field-with-many-headers"
      #rx"header count exceeds limit of 20"
      (lambda ()
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

     (test-exn:fail:network
      "multipart-body-field-without-data"
      #rx"part without data"
      (lambda ()
        (read-mime-multipart (fixture/ip "multipart-body-field-without-data") #"abc")))

     (test-exn:fail:network
      "multipart-body-field-without-data (dripped)"
      #rx"part without data"
      (lambda ()
        (read-mime-multipart (fixture/ip "multipart-body-field-without-data" drip-ip) #"abc")))

     (test-exn:fail:network
      "multipart-body-fields-without-data"
      #rx"part without data"
      (lambda ()
        (read-mime-multipart (fixture/ip "multipart-body-fields-without-data") #"abc")))

     (test-exn:fail:network
      "multipart-body-fields-without-data (dripped)"
      #rx"part without data"
      (lambda ()
        (read-mime-multipart (fixture/ip "multipart-body-fields-without-data" drip-ip) #"abc")))

     (test-exn:fail:network
      "multipart body field without data (full boundary)"
      #rx"part without data"
      (lambda ()
        (read-mime-multipart
         (feed-ip
          '(#"--abc\r\n"
            #"cont"
            #"ent-disposition: "
            #"multipart/form-data; name=\"a\""
            #"\r\n"
            #"\r\n--abc--"
            #"\r\n"))
         #"abc")))

     (test-exn:fail:network
      "multipart body field without data (split at CR)"
      #rx"part without data"
      (lambda ()
        (read-mime-multipart
         (feed-ip
          '(#"--abc\r\n"
            #"cont"
            #"ent-disposition: "
            #"multipart/form-data"
            #"; name=\"a\"\r"
            #"\n\r"
            #"\n--ab"
            #"c--\r\n"))
         #"abc")))

     (test-exn:fail:network
      "multipart body field without data (split boundary)"
      #rx"part without data"
      (lambda ()
        (read-mime-multipart
         (feed-ip
          '(#"--abc\r\n"
            #"cont"
            #"ent-disposition: "
            #"multipart/form-data; name=\"a\""
            #"\r\n\r\n"
            #"--ab"
            #"c--\r\n"))
         #"abc")))

     (test-multipart
      "multipart body (split at CR)"
      (feed-ip
       '(#"--ab"
         #"c\r\n"
         #"Content-Disposition: "
         #"multipart/form-data"
         #"; name=\"f\"\r"
         #"\n\r"
         #"\nhello\r"
         #"world\n\r"
         #"\n--"
         #"ab"
         #"c--\r"
         #"\n"))
      #"abc"
      (list
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"f\""))
                  (open-input-bytes #"hello\rworld\n"))))

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

     (test-exn:fail:network
      "multipart-body-with-long-field, short field limit"
      #rx"field exceeds max length"
      (lambda ()
        (read-mime-multipart (fixture/ip "multipart-body-with-long-field")
                             #"------------------------4cb1363b48c1c499"
                             #:safety-limits (make-safety-limits
                                              #:max-form-data-field-length 10))))

     (test-exn:fail:network
      "multipart-body-with-long-files, small file limit"
      #rx"too many files"
      (lambda ()
        (read-mime-multipart (fixture/ip "multipart-body-with-long-files")
                             #"------------------------4cb1363b48c1c499"
                             #:safety-limits (make-safety-limits
                                              #:max-form-data-files 2))))

     (test-not-exn
      "multipart-body-with-long-files, small field limit"
      (lambda ()
        (read-mime-multipart (fixture/ip "multipart-body-with-long-files")
                             #"------------------------4cb1363b48c1c499"
                             #:safety-limits (make-safety-limits
                                              #:max-form-data-fields 2))))

     (test-exn:fail:network
      "multipart-body-with-long-files, small file length limit"
      #rx"file exceeds max length"
      (lambda ()
        (read-mime-multipart (fixture/ip "multipart-body-with-long-files")
                             #"------------------------4cb1363b48c1c499"
                             #:safety-limits (make-safety-limits
                                              #:max-form-data-files 100
                                              #:max-form-data-file-length 100))))

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

     (test-exn:fail:network
      "multipart-body-without-end-boundary"
      #rx"port closed prematurely"
      (lambda ()
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
      (lambda ()
        (read-mime-multipart (fixture/ip "multipart-body-with-mixture-of-fields-and-files")
                             #"abc"
                             #:safety-limits (make-safety-limits
                                              #:max-form-data-fields 3))))

     (test-not-exn
      "multipart-body-with-mixture-of-fields-and-files, files within limit"
      (lambda ()
        (read-mime-multipart (fixture/ip "multipart-body-with-mixture-of-fields-and-files")
                             #"abc"
                             #:safety-limits (make-safety-limits
                                              #:max-form-data-files 3))))

     (test-exn:fail:network
      "multipart-body-with-mixture-of-fields-and-files, fields above limit"
      #rx"too many fields"
      (lambda ()
        (read-mime-multipart (fixture/ip "multipart-body-with-mixture-of-fields-and-files")
                             #"abc"
                             #:safety-limits (make-safety-limits
                                              #:max-form-data-fields 2))))

     (test-exn:fail:network
      "multipart-body-with-mixture-of-fields-and-files, files above limit"
      #rx"too many files"
      (lambda ()
        (read-mime-multipart (fixture/ip "multipart-body-with-mixture-of-fields-and-files")
                             #"abc"
                             #:safety-limits (make-safety-limits
                                              #:max-form-data-files 2))))

     (test-multipart/fixture
      "multipart-body-with-long-preamble"
      #"abc"
      (list
       (mime-part (list (header #"Content-Disposition" #"multipart/form-data; name=\"x\""))
                  (open-input-bytes #"42"))))

     (test-exn:fail:network
      "multipart-body-with-too-long-preamble, preamble too long"
      #rx"too many \"preamble\" lines"
      (λ ()
        (read-mime-multipart (fixture/ip "multipart-body-with-too-long-preamble")
                             #"abc"
                             #:safety-limits (make-safety-limits))))

     (test-multipart/fixture
      "multipart-body-realistic"
      #"---------------------------200748463811934395181707906709"
      (list
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"customer-email\""))
                  (open-input-bytes #"REDACTED@yahoo.com"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"customer-locale\""))
                  (open-input-bytes #"ro_ro.UTF-8"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"discount\""))
                  (open-input-bytes #"0"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"discount-vat-amount\""))
                  (open-input-bytes #"0"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"discount-label\""))
                  (open-input-bytes #""))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"shipping-cost\""))
                  (open-input-bytes #"1260"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"shipping-vat-amount\""))
                  (open-input-bytes #"239"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"shipping-vat-rate\""))
                  (open-input-bytes #"standard"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"payment-method\""))
                  (open-input-bytes #"credit-card"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"shipping-address.first-name\""))
                  (open-input-bytes #"REDACTED"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"shipping-address.last-name\""))
                  (open-input-bytes #"REDACTED"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"shipping-address.phone\""))
                  (open-input-bytes #"0000000000"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"shipping-address.company\""))
                  (open-input-bytes #""))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"shipping-address.line-1\""))
                  (open-input-bytes #"REDACTED"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"shipping-address.line-2\""))
                  (open-input-bytes #""))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"shipping-address.country\""))
                  (open-input-bytes #"Romania"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"shipping-address.state\""))
                  (open-input-bytes #"REDACTED"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"shipping-address.city\""))
                  (open-input-bytes #"REDACTED"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"shipping-address.zip\""))
                  (open-input-bytes #"000000"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"billing-address.first-name\""))
                  (open-input-bytes #"REDACTED"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"billing-address.last-name\""))
                  (open-input-bytes #"REDACTED"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"billing-address.phone\""))
                  (open-input-bytes #"0000000000"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"billing-address.company\""))
                  (open-input-bytes #""))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"billing-address.line-1\""))
                  (open-input-bytes #"REDACTED"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"billing-address.line-2\""))
                  (open-input-bytes #""))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"billing-address.country\""))
                  (open-input-bytes #"Romania"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"billing-address.state\""))
                  (open-input-bytes #"REDACTED"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"billing-address.city\""))
                  (open-input-bytes #"REDACTED"))
       (mime-part (list (header #"Content-Disposition" #"form-data; name=\"billing-address.zip\""))
                  (open-input-bytes #"000000"))))))

   (test-suite
    "Headers"

    (test-suite
     "read-headers"

     (test-equal?
      "real-world 1"
      (read-headers (fixture/ip "headers-to-github-dot-com")
                    #:safety-limits
                    (make-safety-limits #:max-request-headers 100
                                        #:max-request-header-length 4096))
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
      (read-headers (fixture/ip "headers-to-reddit-dot-com")
                    #:safety-limits
                    (make-safety-limits #:max-request-headers 100
                                        #:max-request-header-length 4096))
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

     (test-exn:fail:network
      "request line too short"
      #rx"line exceeds limit of 5"
      (lambda ()
        (do-test-read-request/limits #"OPTIONS /some/path HTTP/1.1\r\n\r\n"
                                     (make-safety-limits #:max-request-line-length 5)))))

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

     (test-exn:fail:network
      "too many headers"
      #rx"header count exceeds limit"
      (lambda ()
        (do-test-read-request/limits
         (fixture "get-with-many-headers")
         (make-safety-limits #:max-request-headers 10))))

     (test-exn:fail:network
      "header too long"
      #rx"line exceeds limit of 10"
      (lambda ()
        (do-test-read-request/limits
         (fixture "get-with-long-single-line-header")
         (make-safety-limits #:max-request-header-length 10))))

     (test-exn:fail:network
      "folded header too long"
      #rx"header too long"
      (lambda ()
        (do-test-read-request/limits
         (fixture "get-with-long-multi-line-header")
         (make-safety-limits #:max-request-header-length 10)))))

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

     (test-exn:fail:network
      "too many headers after chunked body"
      #rx"header count exceeds limit"
      (lambda ()
        (do-test-read-request/limits
         (fixture "post-with-chunked-transfer-encoding")
         (make-safety-limits #:max-request-headers 3))))

     (test-exn:fail:network
      "chunked request too large"
      #rx"chunked content exceeds max body length"
      (lambda ()
        (do-test-read-request/limits
         (fixture "post-with-chunked-transfer-encoding")
         (make-safety-limits #:max-request-body-length 10)))))

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

     (test-exn:fail:network
      "post-with-json-body, exceeding size limit"
      #rx"body length exceeds limit"
      (lambda ()
        (do-test-read-request/limits (fixture "post-with-json-body")
                                     (make-safety-limits #:max-request-body-length 10)))))

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

     (test-request/fixture
      "post-with-empty-body"
      (hasheq 'bindings null))

     (test-exn:fail:network
      "post-with-invalid-content-length"
      #rx"POST request contained a non-numeric content-length"
      (lambda ()
        (test-read-request (fixture "post-with-invalid-content-length"))))

     (test-exn:fail:network
      "post-with-body-shorter-than-content-length"
      #rx"port closed prematurely"
      (lambda ()
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

     (test-request/fixture
      "post-with-empty-multipart-body"
      (hasheq 'bindings null))

     (test-exn:fail:network
      "post-with-multipart-data-without-disposition"
      #rx"Couldn't extract form field name for file upload"
      (lambda ()
        (test-read-request (fixture "post-with-multipart-data-without-disposition"))))

     (test-exn:fail:network
      "post-with-multipart-data-without-body"
      #rx"port closed prematurely"
      (lambda ()
        (test-read-request (fixture "post-with-multipart-data-without-body"))))))))
