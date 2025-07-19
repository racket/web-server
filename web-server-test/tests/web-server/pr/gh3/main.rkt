#lang racket/base

(require net/http-client
         racket/async-channel
         racket/port
         racket/runtime-path
         version/utils
         web-server/http
         web-server/servlet-dispatch
         web-server/web-server)

(define-runtime-path private-key.pem
  "private-key.pem")
(define-runtime-path server-cert.pem
  "server-cert.pem")

(module+ test
  (require rackunit)

  ;; Old versions of Racket don't work well with recent versions of
  ;; OpenSSL found in CI. So, skip this test for older Rackets.
  (unless (version<? (version) "8.16")
    (define ssl-connect@
      (make-ssl-connect@ server-cert.pem private-key.pem))
    (define confirmation-ch
      (make-async-channel))
    (define stop
      (serve
       #:dispatch
       (dispatch/servlet
        (lambda (_req)
          (response/xexpr `(html (body (h1 "Hello"))))))
       #:port 0
       #:confirmation-channel confirmation-ch
       #:dispatch-server-connect@ ssl-connect@))
    (define port
      (sync confirmation-ch))
    (when (exn:fail? port)
      (raise port))
    (test-case "regression test for GH issue #3"
      (define-values (status _headers body)
        (http-sendrecv "localhost" "/" #:port port #:ssl? #t))
      (check-equal? status #"HTTP/1.1 200 OK")
      (check-equal? (port->bytes body) #"<html><body><h1>Hello</h1></body></html>"))
    (stop)))
