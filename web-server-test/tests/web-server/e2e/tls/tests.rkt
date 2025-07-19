#lang racket/base

(require net/url
         racket/port
         rackunit)

(provide make-tests)

(define (make-tests get-port _get-stop)
  (test-suite
   "tls"

   (test-equal?
    "can get data"
    (port->string (get-pure-port (string->url (format "https://127.0.0.1:~a" (get-port)))))
    "success!")))
