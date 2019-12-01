#lang racket/base

(require racket/port
         web-server/servlet
         web-server/servlet-dispatch
         web-server/safety-limits
         web-server/web-server)

(provide start)

(define (read-write req)
  (response/output
   (lambda (out)
     (display (request-post-data/raw req) out))))

(define (start port)
  (parameterize ([current-error-port (open-output-nowhere)])
    (serve
      #:port port
      #:dispatch (dispatch/servlet read-write)
      #:safety-limits (make-safety-limits
                       #:request-read-timeout 1
                       #:response-send-timeout 1))))
