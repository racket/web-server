#lang racket/base

(require net/base64
         net/cookies/common
         net/cookies/server
         racket/match
         racket/file
         racket/random
         racket/contract
         racket/bool
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
   (->* [cookie-name?]
        [#:path (or/c path/extension-value? #f)
         #:domain (or/c domain-value? #f)]
        cookie?)]
  [valid-id-cookie?
   (->* [any/c
         #:name cookie-name?
         #:key bytes?]
        [#:timeout real?
         #:shelf-life real?]
        (or/c #f (and/c string? cookie-value?)))]
  [request-id-cookie
   (->i #:chaperone
        ([name-or-req {kw-name}
                      (if (unsupplied-arg? kw-name)
                          cookie-name?
                          request?)])
        ([pos-key bytes?]
         [pos-req request?]
         #:name [kw-name cookie-name?]
         #:key [kw-key bytes?]
         #:timeout [timeout real?]
         #:shelf-life [shelf-life real?])
        #:pre/desc {pos-key pos-req kw-name kw-key}
        (let ([pos-key-unsupplied (unsupplied-arg? pos-key)]
              [pos-req-unsupplied (unsupplied-arg? pos-req)]
              [kw-name-unsupplied (unsupplied-arg? kw-name)]
              [kw-key-unsupplied (unsupplied-arg? kw-key)])
          (if (if pos-key-unsupplied
                  (and pos-req-unsupplied
                       (nor kw-name-unsupplied kw-key-unsupplied))
                  (and (not pos-req-unsupplied)
                       (and kw-name-unsupplied kw-key-unsupplied)))
              #true
              (string-append
               "expected:\n  "
               " either three by-position arguments,\n  "
               " or both one by-position argument and #:name and #:key arguments\n  "
               "given: something else")))
        [_ (or/c #f (and/c string? cookie-value?))])]
  [make-id-cookie
   (->i #:chaperone
        ([name cookie-name?]
         [data-or-key {maybe-key}
                      (if (unsupplied-arg? maybe-key)
                          bytes?
                          cookie-value?)])
        ([maybe-data cookie-value?]
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
        #:pre/desc {maybe-data maybe-key}
        (if (xor (unsupplied-arg? maybe-data)
                 (unsupplied-arg? maybe-key))
            #true
            (string-append
             "expected:\n  "
             " either three by-position arguments,\n  "
             " or both two by-position arguments and a #:key argument\n  "
             "given: something else"))
        [_ cookie?])]
  ))

;; stringify : (-> (or/c string? bytes?) bytes?)
(define (stringify str-or-bs)
  (if (bytes? str-or-bs)
      (bytes->string/utf-8 str-or-bs)
      str-or-bs))

;; mac : (-> bytes? exact-integer? string? string?)
(define (mac key authored data)
  (let* ([v (list authored data)]
         ;; 28 bytes including 1 byte of padding
         [bs (base64-encode (HMAC-SHA1 key (write/bytes v)) #"")]
         ;; drop the padding to get 27 bytes
         [bs (subbytes bs 0 27)])
    (bytes->string/utf-8 bs)))

(define (make-secret-salt/file secret-salt-path)
  (unless (file-exists? secret-salt-path)
    (call-with-output-file* secret-salt-path
      (λ (out)
        (write-bytes (crypto-random-bytes 128) out))))
  (file->bytes secret-salt-path))

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
                        #:extension [extension #f])
  (define-values {data* key}
    (if maybe-key
        (values data-or-key maybe-key)
        (values maybe-data data-or-key)))
  (define data (stringify data*))
  (define authored (current-seconds))
  (define authored-str (number->string authored))
  (define digest
    (mac key authored data))
  (make-cookie name
               (string-append digest "&" authored-str "&" data)
               #:path path
               #:expires exp-date	 	 	 	 
               #:max-age max-age	 	 	 	 
               #:domain domain	 	 	 	 
               #:secure? (and secure? #t)
               #:http-only? (and http-only? #t)
               #:extension extension))


(define (valid-id-cookie? c
                          #:name expected-name
                          #:key key
                          #:timeout [timeout +inf.0]
                          #:shelf-life [shelf-life +inf.0])
  (define-values [actual-name value]
    (if (client-cookie? c)
        (values (client-cookie-name c) (client-cookie-value c))
        (values (cookie-name c) (cookie-value c))))
  (and (string=? (stringify expected-name) actual-name)
       (match value
         ;; digest is 27 base64 digits
         [(pregexp #px"^([[:alnum:]\\+/]{27})&(\\d+)&(.*)$"
                   (list _
                         digest
                         (app string->number authored) ;; exact-integer? due to regexp
                         data))
          (and (<= (- (current-seconds) shelf-life) authored timeout)
               (let ([re-digest (mac key authored data)])
                 (string=? digest re-digest))
               data)]
         [_
          #f])))


(define (request-id-cookie name-or-req
                           [maybe-key #f]
                           [maybe-req #f]
                           #:name [kw-name #f]
                           #:key [kw-key #f]
                           #:timeout [timeout +inf.0]
                           #:shelf-life [shelf-life +inf.0])
  (let-values ([{name* key req}
                (if kw-name ;; contract says can't mix old & new conventions
                    (values kw-name kw-key name-or-req)
                    (values name-or-req maybe-key maybe-req))])
    ;; valid-id-cookie? would stringify,
    ;; but we don't want to (potentially) allocate inside the loop.
    (define name (stringify name*))
    (for/or ([c (in-list (request-cookies req))])
      (valid-id-cookie? c
                        #:name name
                        #:key key
                        #:timeout timeout
                        #:shelf-life shelf-life))))


(define (logout-id-cookie name
                          #:path [path #f]
                          #:domain [domain #f])
  ;; net/cookies implements clear-cookie-header by wrapping
  ;; this in cookie->set-cookie-header
  (make-cookie name ""
               #:expires
               (seconds->date 1420070400 ;; midnight UTC on 2015-01-01
                              #f)
               #:path path
               #:domain domain))
