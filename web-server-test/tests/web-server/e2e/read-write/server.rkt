#lang racket/base

(require racket/port
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
    (serve
     #:port 9114
     #:dispatch (dispatch/servlet read-write)
     #:request-read-timeout 1
     #:response-send-timeout 1)))

(module+ main
  (start))
