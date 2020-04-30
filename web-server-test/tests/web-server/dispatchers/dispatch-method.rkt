#lang racket/base

(require rackunit
         racket/promise
         racket/match
         net/url
         web-server/http/request-structs
         web-server/dispatchers/dispatch
         (prefix-in method: web-server/dispatchers/dispatch-method)
         "../util.rkt")

; connection? request? -> symbol?
(define (do-nothing-dispatcher conn req)
  'do-nothing)

; bytes? -> request?
(define (fake-request-for-method method)
  (request method
           (string->url "whatever")
           (list)
           (delay (list))
           #f
           "localhost"
           80
           "whatever"))

; method matches
(let ([d (method:make '(get) do-nothing-dispatcher)]
      [r (fake-request-for-method #"GET")]
      [c (fake-connection-for-bytes #"")])
  (check-equal? 'do-nothing
                (d c r)))

; method (single symbol, uppercase name)
(let ([d (method:make 'GET do-nothing-dispatcher)]
      [r (fake-request-for-method #"gEt")]
      [c (fake-connection-for-bytes #"")])
  (check-equal? 'do-nothing
                (d c r)))

; method does not match multiple options
(let ([d (method:make '(get head) do-nothing-dispatcher)]
      [r (fake-request-for-method #"OPTIONS")]
      [c (fake-connection-for-bytes #"")])
  (check-exn exn:dispatcher?
             (lambda () (d c r))))

; no match, excception
(let ([d (method:make 'POST do-nothing-dispatcher)]
      [r (fake-request-for-method #"GET")]
      [c (fake-connection-for-bytes #"")])
  (check-exn exn:dispatcher?
             (lambda () (d c r))))
