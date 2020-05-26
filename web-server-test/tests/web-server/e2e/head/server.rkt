#lang racket/base

(require web-server/servlet
         web-server/servlet-dispatch
         web-server/web-server)

(provide start)

(define (start port)
  (serve
   #:port port
   #:dispatch (dispatch/servlet
               (lambda (_req)
                 (response/output
                  #:headers (list (make-header #"X-Example" #"Found"))
                  (lambda (out)
                    (displayln "hello" out)))))))
