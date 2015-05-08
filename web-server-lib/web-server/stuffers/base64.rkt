#lang racket/base
(require racket/contract
         web-server/stuffers/stuffer
         net/base64)

(define base64-stuffer
  (make-stuffer (λ (x) (base64-encode x #"")) base64-decode))

(provide/contract
 [base64-stuffer (stuffer/c bytes? bytes?)])
