#lang racket/base

(require net/base64
         net/cookies/common
         net/cookies/server
         racket/match
         racket/file
         racket/random
         racket/contract
         (except-in web-server/http
                    make-cookie)
         web-server/stuffers/hmac-sha1
         web-server/private/util)

(provide
 (contract-out
  [make-secret-salt/file
   (-> path-string?
       bytes?)]
  [logout-id-cookie
   (->* [(and/c string? cookie-name?)]
        [#:path (or/c path/extension-value? #f)
         #:domain (or/c domain-value? #f)]
        cookie?)]
  [valid-id-cookie?
   (->* [any/c
         #:name (and/c string? cookie-name?)
         #:key bytes?]
        [#:timeout real?
         #:shelf-life real?]
        (or/c #f (and/c string? cookie-value?)))]
  [request-id-cookie
   (->i ([name-or-req {kw-name}
                      (if (unsupplied-arg? kw-name)
                          (and/c string? cookie-name?)
                          request?)])
        ([maybe-key bytes?]
         [maybe-req request?]
         #:name [kw-name (and/c string? cookie-name?)]
         #:key [kw-key bytes?]
         #:timeout [timeout number?]
         #:shelf-life [shelf-life real?])
        #:pre/desc {maybe-key maybe-req kw-name kw-key}
        (let ([maybe-key/un (unsupplied-arg? maybe-key)]
              [maybe-req/un (unsupplied-arg? maybe-req)]
              [kw-name/un (unsupplied-arg? kw-name)]
              [kw-key/un (unsupplied-arg? kw-key)])
          (or (and (if maybe-key/un maybe-req/un (not maybe-req/un))
                   (if kw-name/un kw-key/un (not kw-key/un))
                   (not (and maybe-key/un kw-key/un)))
              `("expected: either three by-position arguments or one by-position argument and arguments with keywords #:name and #:key"
                "given: something else")))
        [_ (or/c #f (and/c string? cookie-value?))])]
  [make-id-cookie
   (->i ([name (and/c string? cookie-name?)]
         [data-or-key {maybe-key}
                      (if (unsupplied-arg? maybe-key)
                          bytes?
                          (and/c string? cookie-value?))])
        ([maybe-data {maybe-key}
                     (if (unsupplied-arg? maybe-key)
                         (and/c string? cookie-value?)
                         none/c)]
         #:key [maybe-key bytes?]
         #:path [path (or/c path/extension-value? #f)]
         #:expires [expires (or/c date? #f)]	 	 	 
         #:max-age [max-age
                    (or/c (and/c integer? positive?) #f)]	 	 	 
         #:domain [domain (or/c domain-value? #f)]	 
         #:secure? [secure? any/c]	 	 	 
         #:http-only? [http-only? any/c]	 	 
         #:extension [extension
                      (or/c path/extension-value? #f)])
        #:pre {maybe-data maybe-key}
        (not (and (unsupplied-arg? maybe-data)
                  (unsupplied-arg? maybe-key)))
        [_ cookie?])]
  ))

(define (substring* s st en)
  (substring s st (+ (string-length s) en)))

(define (mac key v)
  (substring*
   (bytes->string/utf-8
    (base64-encode (HMAC-SHA1 key (write/bytes v)) #""))
   0 -3))

(define (make-secret-salt/file secret-salt-path)
  (unless (file-exists? secret-salt-path)
    (with-output-to-file secret-salt-path
      (λ ()
        (write-bytes (crypto-random-bytes 128)))))
  (file->bytes secret-salt-path))

(define (id-cookie? name c)
  (or (and (client-cookie? c)
           (string=? (client-cookie-name c) name))
      (and (cookie? c)
           (equal? (cookie-name c) name))))

(define (make-id-cookie name
                        data-or-key
                        [maybe-data #f]
                        #:key [maybe-key #f]
                        #:path [path #f]
                        #:expires [exp-date #f]	 	 	 	 
                        #:max-age [max-age #f]	 	 	 
                        #:domain [domain #f]	 	 	 
                        #:secure? [secure? #f] ;default ok?	 	 
                        #:http-only? [http-only? #t] ;default ok?	 	 
                        #:extension [extension #f]
                        )
  (define-values {data key}
    (if maybe-key
        (values data-or-key maybe-key)
        (values maybe-data data-or-key)))
  (define authored (current-seconds))
  (define digest
    (mac key (list authored data)))
  (make-cookie name
               (format "~a&~a&~a"
                       digest authored data)
               #:path path
               #:expires exp-date	 	 	 	 
               #:max-age max-age	 	 	 	 
               #:domain domain	 	 	 	 
               #:secure? (not (not secure?))	 	 	 
               #:http-only? (not (not http-only?))	 	 	 
               #:extension extension
               ))

(define (valid-id-cookie? c
                          #:name name
                          #:key key
                          #:timeout [timeout +inf.0]
                          #:shelf-life [shelf-life +inf.0])
  (and (id-cookie? name c)
       (with-handlers ([exn:fail? (lambda (x) #f)])
         (match (if (client-cookie? c)
                    (client-cookie-value c)
                    (cookie-value c))
           [(pregexp #px"^(.+)&(\\d+)&(.*)$"
                     (list _
                           digest
                           (app string->number authored)
                           data))
            (and [authored . <= . timeout]
                 [shelf-life . >= . (- (current-seconds)
                                       authored)]
                 (let ([re-digest (mac key (list authored data))])
                   (string=? digest re-digest))
                 data)]
           [cv
            #f]))))

(define (request-id-cookie name-or-req
                           [maybe-key #f]
                           [maybe-req #f]
                           #:name [kw-name #f]
                           #:key [kw-key #f]
                           #:timeout [timeout +inf.0]
                           #:shelf-life [shelf-life +inf.0])
  (let ([name (or kw-name name-or-req)]
        [key (or kw-key maybe-key)]
        [req (or maybe-req name-or-req)])
    (for/or ([c (in-list (request-cookies req))])
      (valid-id-cookie? c
                        #:name name
                        #:key key
                        #:timeout timeout
                        #:shelf-life shelf-life))))

(define (logout-id-cookie name
                          #:path [path #f]
                          #:domain [domain #f])
  ;net/cookies implements clear-cookie-header by wrapping
  ;this in cookie->set-cookie-header
  (make-cookie name ""
               #:expires
               (seconds->date 1420070400 ; midnight UTC on 1/1/15
                              #f)
               #:path path
               #:domain domain))
