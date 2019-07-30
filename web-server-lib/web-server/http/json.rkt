#lang racket/base

(require racket/list
         racket/contract
         json
         (except-in net/cookies/server
                    make-cookie)
         (file "request-structs.rkt")
         (file "response-structs.rkt")
         (file "cookie.rkt")
         (file "status-code.rkt"))

(module+ test
  (require rackunit))

(define (response/jsexpr
         jsexpr
         #:code [code 200]
         #:message [message #f]
         #:seconds [seconds (current-seconds)]
         #:mime-type [mime-type APPLICATION/JSON-MIME-TYPE]
         #:cookies [cooks empty]
         #:headers [hdrs empty])
  (define js-null (json-null))
  (response
   code (infer-response-message code message) seconds mime-type
   ; rfc2109 also recommends some cache-control stuff here for cookies
   (append hdrs (map cookie->header cooks))
   (λ (out)
     (write-json jsexpr out #:null js-null))))

(provide/contract
 [response/jsexpr
  ((jsexpr?)
   (#:code response-code/c
    #:message (or/c #f bytes?)
    #:seconds real?
    #:mime-type (or/c #f bytes?)
    #:cookies (listof cookie?)
    #:headers (listof header?))
   . ->* . response?)])
