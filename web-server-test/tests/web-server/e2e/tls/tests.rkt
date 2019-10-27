#lang racket/base

(require net/url
         racket/port
         rackunit)

(provide tests)

(define tests
  (test-suite
   "tls"

   (test-equal?
    "can get data"
    (port->string (get-pure-port (string->url "https://127.0.0.1:9111")))
    "success!")))
