#lang racket/base
(require racket/unit
         web-server/private/dispatch-server-sig)

(provide raw:dispatch-server-connect@)

(define-unit raw:dispatch-server-connect@
  (import) (export dispatch-server-connect^)
  (define (port->real-ports ip op)
    (values ip op)))
