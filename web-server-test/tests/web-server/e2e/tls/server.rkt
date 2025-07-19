#lang racket/base

(require racket/async-channel
         racket/port
         web-server/servlet
         web-server/servlet-dispatch
         web-server/web-server)

(provide start)

(define here
  (simplify-path
   (build-path (syntax-source #'here) 'up)))

(define (hello _req)
  (response/output
   (lambda (out)
     (display "success!" out))))

(define (start)
  (parameterize ([current-error-port (open-output-nowhere)])
    (define confirmation-ch
      (make-async-channel))
    (define stop
      (serve
       #:port 0
       #:dispatch (dispatch/servlet hello)
       #:dispatch-server-connect@
       (make-ssl-connect@
        (build-path here "cert.pem")
        (build-path here "key.pem"))
       #:confirmation-channel confirmation-ch))
    (define port-or-exn
      (sync confirmation-ch))
    (when (exn:fail? port-or-exn)
      (raise port-or-exn))
    (values stop port-or-exn)))
