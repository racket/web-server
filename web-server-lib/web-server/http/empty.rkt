#lang racket/base

(require racket/contract
         (except-in net/cookies/server
                    make-cookie)
         (file "request-structs.rkt")
         (file "response-structs.rkt")
         (file "cookie.rkt")
         (file "status-code.rkt"))

(module+ test
  (require rackunit))

(define (response/empty
         #:code [code 204]
         #:message [message #f]
         #:seconds [seconds (current-seconds)]
         #:cookies [cooks '()]
         #:headers [hdrs '()])
  (response
   code (infer-response-message code message) seconds #f
   ; rfc2109 also recommends some cache-control stuff here for cookies
   (cons (make-header #"Content-Length" #"0")
         (append hdrs (map cookie->header cooks)))
   (λ (out) (write-bytes #"" out))))

(provide/contract
 [response/empty
  (()
   (#:code response-code/c
    #:message (or/c #f bytes?)
    #:seconds real?
    #:cookies (listof cookie?)
    #:headers (listof header?))
   . ->* . response?)])
