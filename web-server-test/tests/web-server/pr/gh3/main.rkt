#lang racket/base
(require racket/runtime-path
         racket/port
         racket/list
         web-server/servlet
         net/http-client
         web-server/servlet-env)

(define-runtime-path here ".")

(module+ test
  (require rackunit)
  
  (define-values (pipe-i pipe-o) (make-pipe))

  (define server-t
    (parameterize ([current-output-port pipe-o])
      (thread
       (λ ()
         (serve/servlet (lambda (req) (response/xexpr `(html (body (h1 "Hello")))))
                        #:launch-browser? #f
                        #:port 0
                        #:listen-ip #f
                        #:ssl? #t
                        #:ssl-cert (build-path here "server-cert.pem")
                        #:ssl-key (build-path here "private-key.pem")
                        #:servlet-regexp #rx"")))))

  (define the-port
    (string->number
     (second
      (regexp-match #rx"localhost:([0-9]+).$" (read-line pipe-i)))))
  
  (define-values (status headers body)
    (http-sendrecv "localhost" "/" #:port the-port #:ssl? #t))
  
  (check-equal? status #"HTTP/1.1 200 OK") 
  (check-equal? (port->bytes body) #"<html><body><h1>Hello</h1></body></html>"))
