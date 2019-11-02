#lang racket/base

(require web-server/servlet
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

(define (start port)
  (serve
   #:port port
   #:dispatch (dispatch/servlet echo)))
