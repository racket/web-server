#lang racket/base

(require net/http-client
         racket/port
         rackunit)

(provide make-tests)

(define (make-tests get-port get-stop)
  (test-suite
   "graceful-shutdown"

   (test-case "waits for in-progress connections on stop"
     (define hc (http-conn-open "127.0.0.1" #:port (get-port)))
     (define-values (status _headers in)
       (http-conn-sendrecv! hc "/"))
     ((get-stop))
     (check-equal? status #"HTTP/1.1 200 OK")
     (check-equal? (port->bytes in) #"0\n1\n2\n3\n4\n"))))
