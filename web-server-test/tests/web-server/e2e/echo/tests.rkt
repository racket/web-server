#lang racket/base

(require net/url
         racket/port
         rackunit)

(provide make-tests)

(define get-response
  (compose1 port->string get-pure-port string->url))

(define (make-tests get-port _get-stop)
  (define (make-uri [path "/"])
    (format "http://127.0.0.1:~a/~a" (get-port) path))

  (test-suite
   "echo"

   (test-equal?
    "no query param"
    (get-response (make-uri))
    "nothing")

   (test-equal?
    "with empty query param"
    (get-response (make-uri "?message="))
    "")

   (test-equal?
    "with query param"
    (get-response (make-uri "?message=hello"))
    "hello")

   (test-equal?
    "with encoded param"
    (get-response (make-uri "?message=hello%3D%26"))
    "hello=&")))
