#lang racket/base

(require racket/async-channel
         web-server/servlet
         web-server/servlet-dispatch
         web-server/web-server)

(provide start)

(define (echo req)
  (define msg
    (cond
      [(bindings-assq #"message" (request-bindings/raw req)) => binding:form-value]
      [else #"nothing"]))
  (response/output
   (lambda (out)
     (display msg out))))

(define (start)
  (define confirmation-ch
    (make-async-channel))
  (define stop
    (serve
     #:port 0
     #:dispatch (dispatch/servlet echo)
     #:confirmation-channel confirmation-ch))
  (define port-or-exn
    (sync confirmation-ch))
  (when (exn:fail? port-or-exn)
    (raise port-or-exn))
  (values stop port-or-exn))
