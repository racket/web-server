#lang racket/base
(require rackunit
         racket/promise
         racket/list
         racket/match
         racket/runtime-path
         (for-syntax racket/base)
         net/url
         net/cookies/common
         (except-in net/cookies/server
                    make-cookie)
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/http/cookie
         web-server/http/id-cookie
         web-server/http/cookie-parse)
(provide cookies-tests)

(define (header-equal? h1 h2)
  (and (bytes=? (header-field h1)
                (header-field h2))
       (bytes=? (header-value h1)
                (header-value h2))))

(define (set-header->read-header h)
  (make-header #"Cookie" (header-value h)))

(define-runtime-path tmp-secret-salt-path
  "tmp-secret-salt-path")

(define-check (check-equal?/list-no-order actual expected)
  (or (and (list? actual)
           (list? expected)
           (= (length actual)
              (length expected))
           (let loop ([actual-to-go actual]
                      [expected-to-go expected])
             (match expected-to-go
               ['() (null? actual-to-go)]
               [(cons this-expected more-expected)
                (and (member this-expected actual-to-go)
                     (loop (remove this-expected actual-to-go)
                           more-expected))])))
      (with-check-info (['actual actual]
                        ['expected expected])
        (fail-check))))

(define-syntax (test-equal?/list-no-order stx)
  (syntax-case stx ()
    [(_ msg actual expected)
     (with-syntax ([expr (syntax/loc stx (check-equal?/list-no-order actual expected))])
       (syntax/loc stx
         (test-case msg expr)))]))

(define cookies-tests
  (test-suite
   "Cookies"

   (test-suite
    "cookie.rkt"

    (test-suite
     "cookie->header and make-cookie"
     (test-check "Simple" header-equal?
                 (cookie->header (make-cookie "name" "value"))
                 (make-header #"Set-Cookie" #"name=value"))

     (test-equal? "Comment" 
                  (header-value (cookie->header (make-cookie "name" "value" #:comment "comment")))
                  #"name=value") ;comment is now ignored

     (test-equal? "Domain"
                  (header-value (cookie->header (make-cookie "name" "value" #:domain "host.domain")))
                  #"name=value; Domain=host.domain")

     (test-equal? "max-age"
                  (header-value (cookie->header (make-cookie "name" "value" #:max-age 24)))
                  #"name=value; Max-Age=24")

     (test-equal? "path"
                  (header-value (cookie->header (make-cookie "name" "value" #:path "path")))
                  #"name=value; Path=path")

     (test-equal? "secure? #t"
                  (header-value (cookie->header (make-cookie "name" "value" #:secure? #t)))
                  #"name=value; Secure")

     (test-equal? "secure? #f"
                  (header-value (cookie->header (make-cookie "name" "value" #:secure? #f)))
                  #"name=value")))

   (let ()
     (define (reqcs hs)
       (request-cookies
        (make-request
         #"GET" (string->url "http://test.com/foo")
         hs (delay empty) #f
         "host" 80 "client")))
     (define (reqc h)
       (reqcs (list (make-header #"Cookie" h))))

     (test-suite
      "cookie-parse.rkt"

      ;RFC 6265 no longer gives special meaning to "$Version" "$Path" or "$Domain"
      
      (test-equal? "None"
                   (reqcs empty)
                   empty)

      (test-equal?/list-no-order "Simple"
                                 (reqc #"$Version=\"1\"; name=\"value\"") 
                                 (list (make-client-cookie "$Version" "1" #f #f)
                                       (make-client-cookie "name" "value" #f #f)))

      (test-equal?/list-no-order "Path"
                                 (reqc #"$Version=\"1\"; name=\"value\"; $Path=\"/acme\"")
                                 (list (make-client-cookie "$Version" "1" #f #f)
                                       (make-client-cookie "$Path" "/acme" #f #f)
                                       (make-client-cookie "name" "value" #f #f))) ;new version of request-cookies never populates path or domain

      (test-equal?/list-no-order "Domain"
                                 (reqc #"$Version=\"1\"; name=\"value\"; $Domain=\"host.acme\"")
                                 (list (make-client-cookie "$Version" "1" #f #f)
                                       (make-client-cookie "$Domain" "host.acme" #f #f)
                                       (make-client-cookie "name" "value" #f #f))) ;new version of request-cookies never populates path or domain

      (test-equal?/list-no-order "Multiple"
                                 (reqc #"$Version=\"1\"; key1=\"value1\"; key2=\"value2\"")
                                 (list (make-client-cookie "$Version" "1" #f #f)
                                       (make-client-cookie "key1" "value1" #f #f)
                                       (make-client-cookie "key2" "value2" #f #f)))

      (test-equal?/list-no-order "Multiple w/ paths & domains"
                                 (reqc #"$Version=\"1\"; key1=\"value1\"; $Path=\"/acme\"; key2=\"value2\"; $Domain=\"host.acme\"")
                                 (list (make-client-cookie "$Version" "1" #f #f)
                                       (make-client-cookie "$Domain" "host.acme" #f #f)
                                       (make-client-cookie "$Path" "/acme" #f #f)
                                       (make-client-cookie "key1" "value1" #f #f) ;new version of request-cookies never populates path or domain
                                       (make-client-cookie "key2" "value2" #f #f)))

      (test-equal?/list-no-order "phpBB. PR10689"
                                 (reqc #"style_cookie=null; phpbb3_e1p9b_u=54; phpbb3_e1p9b_k=; phpbb3_e1p9b_sid=3fa8d7a7b65fbabcbe9b345861dc079a")
                                 (list (make-client-cookie "style_cookie" "null" #f #f)
                                       (make-client-cookie "phpbb3_e1p9b_u" "54" #f #f)
                                       (make-client-cookie "phpbb3_e1p9b_k" "" #f #f)
                                       (make-client-cookie "phpbb3_e1p9b_sid" "3fa8d7a7b65fbabcbe9b345861dc079a" #f #f)))

      (test-equal?/list-no-order "Google" 
                                 (reqc ;this is rejected if there is a \n between the cookies or if there is a trailing \r\n
                                  (bytes-append #"teaching-order=course; "
                                                #"__utmz=165257760.1272597702.1.1.utmcsr=(direct)"
                                                #"|utmccn=(direct)|utmcmd=(none)"))
                                 (list (make-client-cookie "teaching-order" "course" #f #f)
                                       (make-client-cookie "__utmz" "165257760.1272597702.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none)" #f #f)))

      #;(let ()
          (define in "hell\"w\"o") ;<--- this is not a cookie-value?
          (define out #"id=\"hell\\\"w\\\"o\"")
          (test-check "quotes (pr14194)" header-equal?
                      (cookie->header (make-cookie "id" in))
                      (make-header #"Set-Cookie" out))
          (test-equal? "quotes (pr14194)"
                       (reqc out)
                       (list (make-client-cookie "id" in #f #f))))))
   (test-suite
    "RFC 6265 modifications"

    (let ([dt (date* 26 42 0 9 3 2017 4 67 #f 0 0 "UTC")])
      (test-equal? "#:expires as string"
                   (cookie-expires (make-cookie "my-cookie"
                                                "my-value"
                                                #:expires "Thu, 09 Mar 2017 00:42:26 GMT"))
                   dt)
      (define c
        (make-cookie "my-cookie"
                     "my-value"
                     #:comment "This is ignored"
                     #:domain "example.com"
                     #:max-age 42
                     #:path "/some-path"
                     #:expires dt
                     #:secure? 'yes
                     #:http-only? 'yes
                     #:extension "ext"))
      (test-suite
       "extra arguments to make-cookie"
       (check-match c
                    (cookie "my-cookie"
                            "my-value"
                            (? (λ (x) (equal? x dt)))
                            42
                            "example.com"
                            "/some-path"
                            #t
                            #t
                            "ext"))
       (check-match (cookie->header c)
                    (header
                     #"Set-Cookie"
                     (app (λ (val) (regexp-split #rx"; " val))
                          (list-no-order #"my-cookie=my-value"
                                         #"Expires=Thu, 09 Mar 2017 00:42:26 GMT"
                                         #"Max-Age=42"
                                         #"Domain=example.com"
                                         #"Path=/some-path"
                                         #"Secure"
                                         #"HttpOnly"
                                         #"ext"))))
       )))

   (test-suite
    "id-cookie.rkt"

    (test-suite
     "make-secret-salt/file"
     (let ([delete-salt-file
            (λ ()
              (when (file-exists? tmp-secret-salt-path)
                (delete-file tmp-secret-salt-path)))])
       (dynamic-wind delete-salt-file
                     (λ ()
                       (test-equal? "should only initialize once"
                                    (make-secret-salt/file tmp-secret-salt-path)
                                    (make-secret-salt/file tmp-secret-salt-path)))
                     delete-salt-file)))
       
    (let ()
      (define test-secret-salt
        (bytes-append #"U;\256\0.\203Iu\3663\367\262d\220\276t\207\17^_0\240\2U\341"
                      #"\240E\20\322\36\213\210\224\35ey\365:\332\"\e\211\262\v@y\n"
                      #"\377\32561\364\277R\363\334Q\273\270\36\223\242\202\272\206"
                      #"\2\355\335\343\327\211\22\24\365\377\353\340\332\e\21\312\217"
                      #"\220\344\203\322\320\322\341\2731\e\236\230\307\246\23i\352>3,"
                      #"\260*\2,\375DK\302S\270Q\2433v\327\272\1\16\361y\213\4\16X\345H"))

      (test-suite
       "make-id-cookie and valid-id-cookie?"
       (test-false "reject forged"
                   (valid-id-cookie? (client-cookie "my-id-cookie"
                                                    "my-id-cookie=YmFLLOIDULjpLQOu1+cvMBM+m&1489023629&forged-value"
                                                    #f #f)
                                     #:name "my-id-cookie"
                                     #:key test-secret-salt))
       (let ([dt (date* 26 42 0 9 3 2017 4 67 #f 0 0 "UTC")])
         (define kw-c
           (make-id-cookie "my-id-cookie"
                           "my-signed-value"
                           #:key test-secret-salt
                           #:domain "example.com"
                           #:max-age 42
                           #:path "/some-path"
                           #:expires dt
                           #:secure? 'yes
                           #:http-only? 'yes
                           #:extension "ext"))
         (define by-pos-c
           (make-id-cookie "my-id-cookie"
                           test-secret-salt
                           "my-signed-value"
                           #:domain "example.com"
                           #:max-age 42
                           #:path "/some-path"
                           #:expires dt
                           #:secure? 'yes
                           #:http-only? 'yes
                           #:extension "ext"))
         (for ([c (list kw-c by-pos-c)]
               [convention '(keyword by-position)])
           (with-check-info (['cookie c]
                             ['|make-id-cookie calling convention| convention])
             (test-not-false "infinite timeout"
                             (valid-id-cookie? c
                                               #:name "my-id-cookie"
                                               #:key test-secret-salt))
             (test-not-false "finite timeout"
                             (valid-id-cookie? c
                                               #:name "my-id-cookie"
                                               #:key test-secret-salt
                                               #:timeout (current-seconds)))
             (test-false "reject expired"
                         (valid-id-cookie? c
                                           #:name "my-id-cookie"
                                           #:key test-secret-salt
                                           #:timeout (- (current-seconds)
                                                        86400)))
             ))))
      (test-suite
       "request-id-cookie"
       (let ()
         (define req
           (make-request
            #"GET" (string->url "http://test.com/foo")
            (list (header #"Cookie"
                          #"my-id-cookie=YmFLLOIDULjpLQOu1+cvMBM+m&1489023629&my-signed-value"))
            (delay empty) #f "host" 80 "client"))
         (test-not-false "infinite timeout & shelf life"
                         (request-id-cookie req
                                            #:name "my-id-cookie"
                                            #:key test-secret-salt))
         (test-not-false "finite timeout"
                     (request-id-cookie req
                                        #:name "my-id-cookie"
                                        #:key test-secret-salt
                                        #:timeout (current-seconds)))
         (test-not-false "finite timeout / by position"
                     (request-id-cookie "my-id-cookie"
                                        test-secret-salt
                                        req
                                        #:timeout (current-seconds)))
         (test-false "timeout / reject expired"
                     (request-id-cookie req
                                        #:name "my-id-cookie"
                                        #:key test-secret-salt
                                        #:timeout 1089023629))
         (test-equal? "long finite shelf-life / fresh cookie"
                      (valid-id-cookie? (make-id-cookie "fresh-id-cookie"
                                                  "test-value"
                                                  #:key #"test-key")
                                  #:name "fresh-id-cookie"
                                  #:key #"test-key"
                                  #:shelf-life 500
                                  )
                      "test-value")
         (test-equal? "long finite shelf-life"
                      (request-id-cookie req
                                         #:name "my-id-cookie"
                                         #:key test-secret-salt
                                         #:shelf-life (+ 10
                                                         (- (current-seconds)
                                                            1489023629)))
                      "my-signed-value")
         (test-false "shelf-life / reject expired"
                     (request-id-cookie req
                                         #:name "my-id-cookie"
                                         #:key test-secret-salt
                                         #:shelf-life -10))
         ))))))
    

  (module+ test
    (require rackunit/text-ui)
    (run-tests cookies-tests))

