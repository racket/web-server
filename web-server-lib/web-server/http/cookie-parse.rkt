#lang racket/base

(require web-server/http/request-structs
         net/cookies/common
         net/cookies/server
         web-server/private/util
         racket/match
         racket/contract)

(provide (contract-out
          [struct client-cookie 
            ([name (and/c string? cookie-name?)]
             [value (and/c string? cookie-value?)]
             [domain (or/c #f domain-value?)]
             [path (or/c #f path/extension-value?)])]
          [request-cookies (-> request?
                               (listof client-cookie?))]
          ))

(define-struct client-cookie 
  (name value domain path)
  #:prefab)

(define handle-quoted-value
  (match-lambda
    [(regexp #rx"^\"(.*)\"$" (list _ inner))
     inner]
    [val val]))

(define (request-cookies req)
  (for/fold ([cookies-so-far null])
            ([this-header (in-list (request-headers/raw req))]
             #:when (bytes-ci=? #"Cookie"
                                (header-field this-header)))
    (append cookies-so-far
            (for/list ([pr (in-list (cookie-header->alist
                                     (header-value this-header)))])
              (client-cookie (bytes->string/utf-8 (car pr))
                             (handle-quoted-value (bytes->string/utf-8 (cdr pr)))
                             #f
                             #f)))))
