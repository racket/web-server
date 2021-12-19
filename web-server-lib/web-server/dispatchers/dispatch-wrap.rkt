#lang racket/base

(require racket/contract
         web-server/dispatchers/dispatch
         web-server/http/request-structs
         web-server/http/response-structs
         (prefix-in lift: web-server/dispatchers/dispatch-lift))

(provide/contract
 [interface-version dispatcher-interface-version/c]
 [make (-> (-> request? response?)
           (-> request? request?)
           (-> response? response?)
           dispatcher/c)])

(define interface-version 'v1)

(define (make servlet req-trans res-trans)
  (lift:make (λ (req) (res-trans (servlet (req-trans req))))))
