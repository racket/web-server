#lang racket

(provide write-response)

(require web-server/private/timer
         web-server/private/connection-manager
         web-server/http/response
         (file "../util.rkt"))

(define (write-response tm r [redact? #t])
  (define-values (i-port o-port) (make-pipe))
  (define conn
    (connection 0 (start-timer tm +inf.0 void)
                i-port o-port (make-custodian) #t))
  (output-response conn r)
  (close-output-port o-port)
  (define bs (port->bytes i-port))
  (bytes-sort (if redact? (redact bs) bs)))
