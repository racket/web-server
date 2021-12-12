#lang racket/base
(require net/url
         racket/match
         racket/list
         racket/contract
         racket/bool)

(require web-server/dispatchers/dispatch
         web-server/private/util
         web-server/http
         web-server/http/response
         web-server/dispatchers/filesystem-map)

(provide/contract
 [interface-version dispatcher-interface-version/c]
 [read-range-header (-> (listof header?) (or/c (listof pair?) false/c))]
 [make
  (->* (#:url->path url->path/c)
       (#:path->mime-type (path-string? . -> . (or/c false/c bytes?))
        #:indices (listof path-string?)
        #:cache-max-age (or/c false/c (and/c exact-integer? positive?))
        #:cache-smaxage (or/c false/c (and/c exact-integer? positive?))
        #:cache-stale-while-revalidate (or/c false/c (and/c exact-integer? positive?))
        #:cache-stale-if-error (or/c false/c (and/c exact-integer? positive?))
        #:cache-no-cache boolean?
        #:cache-no-store boolean?
        #:cache-no-transform boolean?
        #:cache-must-revalidate boolean?
        #:cache-proxy-revalidate boolean?
        #:cache-must-understand boolean?
        #:cache-private boolean?
        #:cache-public boolean?
        #:cache-immutable boolean?)
       dispatcher/c)])

(module+ test
  (require rackunit))

;; looks-like-directory : str -> bool
;; to determine if is url style path looks like it refers to a directory
(define (looks-like-directory? path)
  (and (regexp-match #rx"/$" path)
       #t))

; path? -> boolean?
(define (can-emit-file? path)
  (and (file-exists? path)
       (member 'read (file-or-directory-permissions path))
       #t))

(define interface-version 'v1)

(define/contract (cache-policy-pair->header-value pair)
  (-> (cons/c symbol? (or/c boolean? integer?))
      string?)
  (cond [(integer? (cdr pair))
         (string-append (symbol->string (car pair))
                        "="
                        (number->string (cdr pair)))]
        [else
         (symbol->string (car pair))]))

; (listof (pairof symbol? (or boolean? integer?))) -> (listof (pairof symbol? (or #t integer?)))
(define/contract (remove-false-policies policies)
  (-> (listof (cons/c symbol? (or/c boolean? integer?)))
      (listof (cons/c symbol? (or/c #t integer?))))
  (define (ok? pair)
    (or (integer? (cdr pair))
        (eq? #t (cdr pair))))
  (filter ok? policies))

(define/contract (cache-policy-alist->header policy)
  (-> (listof (cons/c symbol? (or/c boolean? integer?)))
      (or/c false/c header?))
  (define trimmed-policy (remove-false-policies policy))
  (cond [(null? trimmed-policy)
         #f]
        [else
         (define policy-parts (map cache-policy-pair->header-value
                                   trimmed-policy))
         (define rendered-policy-parts (apply string-append
                                              (add-between policy-parts ", ")))
         (make-header #"Cache-Control"
                      (string->bytes/utf-8 rendered-policy-parts))]))

(define (make #:url->path url->path
              #:path->mime-type [path->mime-type (lambda (path) #f)]
              #:indices [indices (list "index.html" "index.htm")]
              #:cache-max-age [cache-max-age #f]
              #:cache-smaxage [cache-smaxage #f]
              #:cache-no-cache [cache-no-cache #f]
              #:cache-no-store [cache-no-store #f]
              #:cache-no-transform [cache-no-transform #f]
              #:cache-must-revalidate [cache-must-revalidate #f]
              #:cache-proxy-revalidate [cache-proxy-revalidate #f]
              #:cache-must-understand [cache-must-understand #f]
              #:cache-private [cache-private #f]
              #:cache-public [cache-public #f]
              #:cache-immutable [cache-immutable #f]
              #:cache-stale-while-revalidate [cache-stale-while-revalidate #f]
              #:cache-stale-if-error [cache-stale-if-error #f])
  (define cache-policy/header
    (cache-policy-alist->header
     (list (cons 'immutable cache-immutable)
           (cons 'max-age cache-max-age)
           (cons 'must-revalidate cache-must-revalidate)
           (cons 'must-understand cache-must-understand)
           (cons 'no-cache cache-no-cache)
           (cons 'no-store cache-no-store)
           (cons 'no-transform cache-no-transform)
           (cons 'private cache-private)
           (cons 'proxy-revalidate cache-proxy-revalidate)
           (cons 'public cache-public)
           (cons 's-maxage cache-smaxage)
           (cons 'stale-if-error cache-stale-if-error)
           (cons 'stale-while-revalidate cache-stale-while-revalidate))))
  (define headers (cond [(header? cache-policy/header)
                         (list cache-policy/header)]
                        [else (list)]))
  (lambda (conn req)
    (define uri (request-uri req))
    (define method (request-method req))
    (define-values (path _) (url->path uri))
    (define is-head? (bytes-ci=? method #"HEAD"))
    (define is-get? (bytes-ci=? method #"GET"))
    (define (emit-file-response path)
      (output-file conn
                   path
                   method
                   (path->mime-type path)
                   (read-range-header (request-headers/raw req))
                   headers))
    (define path/string (url-path->string (url-path uri)))
    (define (emit-index-if-possible dir)
      (let/ec esc
        (for-each (lambda (dir-default)
                    (define full-name (build-path dir dir-default))
                    (when (can-emit-file? full-name)
                      (esc (emit-file-response full-name))))
                  indices)
        (next-dispatcher)))
    (cond [(nor is-head? is-get?)
           (next-dispatcher)]
          [(can-emit-file? path)
           (emit-file-response path)]
          [(directory-exists? path)
           (if (looks-like-directory? path/string)
               (emit-index-if-possible path)
               (output-response
                conn
                (redirect-to (string-append path/string "/"))))]
          [else (next-dispatcher)])))

;; read-range-header : (listof header) -> (U (alist-of (U integer #f) (U integer #f)) #f)
;;
;; Returns a list of pairs of the byte offsets specified in an HTTP Range
;; header, or #f if the header is missing or malformed.
;;
;; The HTTP spec for the Range header can be found here:
;;
;;     http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35
;;
;; More specifically, the (non-#f form of the) result is:
;;
;;     (list-of (U byte-range-spec suffix-byte-range-spec))
;;
;; where:
;;
;;     byte-range-spec : (cons integer (U integer #f))
;;     suffix-byte-range-spec : (cons #f integer)
;;
;; All offsets are inclusive: the integers are precisely those that appear
;; in the header.
(define read-range-header
  (let ([range-header-regexp    #px#"^bytes=(.*)$"]
        [range-delimiter-regexp #px#","]
        [range-regexp           #px#"^([0-9]*)-([0-9]*)$"]
        [range-error            (lambda (header)
                                  (display (format "Bad Range header: ~s. File a Racket problem report!\n"
                                                   (header-value header))
                                           (current-error-port))
                                  #f)])
    (lambda (headers)
      (let ([header (headers-assq* #"Range" headers)])
        (if header
            (let/ec escape
              (match (regexp-match range-header-regexp (header-value header))
                [(list _ ranges-string)
                 (let ([ranges (regexp-split range-delimiter-regexp ranges-string)])
                   (map (lambda (range-string)
                          (match (regexp-match range-regexp range-string)
                            [(list _ start-offset end-offset)
                             (cons (string->number (bytes->string/utf-8 start-offset))
                                   (string->number (bytes->string/utf-8 end-offset)))]
                            [#f (escape (range-error header))]))
                        ranges))]
                [#f (escape (range-error header))]))
            #f)))))
