#lang racket/base

(require rackunit
         racket/promise
         racket/function
         net/url
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/dispatchers/dispatch
         web-server/private/connection-manager
         (prefix-in wrap: web-server/dispatchers/dispatch-wrap)
         "../util.rkt")

(provide dispatch-wrap-tests)

; request? -> response?
(define (count-request-headers req)
  (response 200
            #"OK"
            (current-seconds)
            #f
            (list (make-header #"Request-Header-Count"
                               (string->bytes/utf-8
                                (number->string
                                 (length (request-headers/raw req))))))
            (lambda (p) (write-bytes #"hi" p))))

(define (inject-request-header req)
  (define h (make-header #"Joe" #"Blow"))
  (struct-copy request
               req
               [headers/raw (cons h (request-headers/raw req))]))

(define (replace-response-body res)
  (struct-copy response
               res
               [output (lambda (p)
                         (write-bytes #"Intercepted!" p))]))

(define dummy-request
  (request #"POST"
           (string->url "whatever")
           (list (make-header #"Vary" #"Lisp"))
           (delay (list))
           #f
           "localhost"
           80
           "whatever"))

(define dispatch-wrap-tests
  (test-suite
   "Wrap"

   (test-case
       "Transform neither request nor response"
     (let ([d (wrap:make count-request-headers identity identity)]
           [c (fake-connection-for-bytes #"")])
       (d c dummy-request)
       (define bs (bytes->string/utf-8 (get-output-bytes (connection-o-port c))))
       (check-regexp-match #px"\r\nRequest-Header-Count: 1\r\n" bs bs)
       (check-regexp-match #px"\r\n\r\nhi$" bs bs)))

   (test-case
       "Transform request, do nothing to response"
     (let ([d (wrap:make count-request-headers inject-request-header identity)]
           [c (fake-connection-for-bytes #"")])
       (d c dummy-request)
       (define bs (bytes->string/utf-8 (get-output-bytes (connection-o-port c))))
       (check-regexp-match #px"\r\nRequest-Header-Count: 2\r\n" bs bs)
       (check-regexp-match #px"\r\n\r\nhi$" bs bs)))

   (test-case
       "Transform response, do nothing to request"
     (let ([d (wrap:make count-request-headers identity replace-response-body)]
           [c (fake-connection-for-bytes #"")])
       (d c dummy-request)
       (define bs (bytes->string/utf-8 (get-output-bytes (connection-o-port c))))
       (check-regexp-match #px"\r\nRequest-Header-Count: 1\r\n" bs bs)
       (check-regexp-match #px"\r\n\r\nIntercepted!$" bs bs)))

   (test-case
       "Transform both request and response"
     (let ([d (wrap:make count-request-headers inject-request-header replace-response-body)]
           [c (fake-connection-for-bytes #"")])
       (d c dummy-request)
       (define bs (bytes->string/utf-8 (get-output-bytes (connection-o-port c))))
       (check-regexp-match #px"\r\nRequest-Header-Count: 2\r\n" bs bs)
       (check-regexp-match #px"\r\n\r\nIntercepted!$" bs bs)))))
