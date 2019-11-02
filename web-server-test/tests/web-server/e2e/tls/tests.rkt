#lang racket/base

(require net/url
         racket/port
         rackunit)

(provide make-tests)

(define (make-tests port)
  (test-suite
   "tls"

   (test-equal?
    "can get data"
    (port->string (get-pure-port (string->url (format "https://127.0.0.1:~a" port))))
    "success!")))
