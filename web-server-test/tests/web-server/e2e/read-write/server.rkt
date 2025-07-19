#lang racket/base

(require racket/async-channel
         racket/port
         web-server/safety-limits
         web-server/servlet
         web-server/servlet-dispatch
         web-server/web-server)

(provide start)

(define (read-write req)
  (response/output
   (lambda (out)
     (display (request-post-data/raw req) out))))

(define (start)
  (parameterize ([current-error-port (open-output-nowhere)])
    (define confirmation-ch
      (make-async-channel))
    (define stop
      (serve
       #:port 0
       #:dispatch (dispatch/servlet read-write)
       #:confirmation-channel confirmation-ch
       #:safety-limits (make-safety-limits
                        #:request-read-timeout 1
                        #:response-send-timeout 1)))
    (define port-or-exn
      (sync confirmation-ch))
    (when (exn:fail? port-or-exn)
      (raise port-or-exn))
    (values stop port-or-exn)))
