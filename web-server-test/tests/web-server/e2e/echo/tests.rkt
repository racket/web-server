#lang racket/base

(require net/url
         racket/port
         rackunit)

(provide tests)

(define get-response
  (compose1 port->string get-pure-port string->url))

(define tests
  (test-suite
   "echo"

   (test-equal?
    "no query param"
    (get-response "http://127.0.0.1:9111")
    "nothing")

   (test-equal?
    "with empty query param"
    (get-response "http://127.0.0.1:9111?message=")
    "")

   (test-equal?
    "with query param"
    (get-response "http://127.0.0.1:9111?message=hello")
    "hello")

   (test-equal?
    "with encoded param"
    (get-response "http://127.0.0.1:9111?message=hello%3D%26")
    "hello=&")))
