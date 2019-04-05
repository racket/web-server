#lang racket/base
(require racket/contract
         racket/list
         racket/match
         xml
         web-server/private/xexpr
         (except-in net/cookies/server
                    make-cookie)
         "request-structs.rkt"
         "cookie.rkt"
         "response-structs.rkt"
         "status-code.rkt")

(module+ test
  (require rackunit))

(define (response/xexpr
         xexpr
         #:code [code 200]
         #:message [message #f]
         #:seconds [seconds (current-seconds)]
         #:mime-type [mime-type TEXT/HTML-MIME-TYPE]
         #:cookies [cooks empty]
         #:headers [hdrs empty]
         #:preamble [preamble #""])
  (response
   code (infer-response-message code message) seconds mime-type
   ; rfc2109 also recommends some cache-control stuff here for cookies
   (append hdrs (map cookie->header cooks))
   (λ (out)
     (write-bytes preamble out)
     (write-xexpr xexpr out))))

(module+ test
  ;; sanity check: we get a response
  (check-true (response? (response/xexpr '(foo))))
  (let ([resp (response/xexpr '(html))])
    ;; no code, no message ==> 200 "OK"
    (check-equal? 200 (response-code resp))
    (check-equal? #"OK" (response-message resp)))
  ;; code present, message absent
  (let ([resp (response/xexpr '(html) #:code 201)])
    (check-equal? 201 (response-code resp))
    (check-equal? #"Created" (response-message resp)))
  ;; unknown status code used, no message
  (let ([resp (response/xexpr '(html) #:code 256)])
    (check-equal? 256 (response-code resp))
    (check-equal? #"OK" (response-message resp)))
  ;; known code used, #f used as message
  (let ([resp (response/xexpr '(html) #:code 204 #:message #f)])
    (check-equal? 204 (response-code resp))
    (check-equal? #"No Content" (response-message resp)))
  ;; known code used, message lookup overridden
  (let ([resp (response/xexpr '(html) #:code 204 #:message #"Cowabunga")])
    (check-equal? 204 (response-code resp))
    (check-equal? #"Cowabunga" (response-message resp)))
  ;; code absent, message #f ==> 200 "OK"
  (let ([resp (response/xexpr '(html) #:message #f)])
    (check-equal? 200 (response-code resp))
    (check-equal? #"OK" (response-message resp)))
  ;; code absent, message present ==> 200 <message>
  (let ([resp (response/xexpr '(html) #:message #"Say Cheese")])
    (check-equal? 200 (response-code resp))
    (check-equal? #"Say Cheese" (response-message resp))))

(provide/contract
 [response/xexpr
  ((pretty-xexpr/c)
   (#:code response-code/c #:message (or/c #f bytes?) #:seconds real? #:mime-type (or/c #f bytes?) #:cookies (listof cookie?) #:headers (listof header?) #:preamble bytes?)
   . ->* . response?)])
