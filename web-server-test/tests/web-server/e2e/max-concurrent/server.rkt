#lang racket/base

(require web-server/safety-limits
         web-server/servlet
         web-server/servlet-dispatch
         web-server/web-server)

(provide start)

(define (app _req)
  (response/output
   (lambda (out)
     (display "ok" out))))

(define (start port)
  (serve
   #:port port
   #:dispatch (dispatch/servlet app)
   #:safety-limits (make-safety-limits
                    #:max-concurrent 1
                    #:max-waiting 10)))
