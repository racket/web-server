#lang racket/base
(require web-server/test
         rackunit
         net/url
         web-server/http
         racket/promise)

(define (url->request u)
  (make-request #"GET" (string->url u) '()
                (delay '()) #f "127.0.0.1" 8080 "127.0.0.1"))

(define (index request)
  (response
   200 #"OK"
   (current-seconds)
   #f '()
   (lambda (out)
     (write-bytes #"<html>bad\r\n\r\nbody</html>" out))))

(check-not-exn
 (lambda ()
   (let [(tester (make-servlet-tester index))]
     (tester (url->request "")))))
