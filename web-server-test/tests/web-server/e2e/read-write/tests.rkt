#lang racket/base

(require net/url
         racket/port
         racket/tcp
         rackunit)

(provide tests)

(define (broken-pipe? e)
  (and (exn:fail:network? e)
       (equal? (exn:fail:network:errno-errno e) '(32 . posix))))

(define tests
  (test-suite
   "read-write"

   (test-case "sending and receiving data"
     (check-regexp-match
      "hello world"
      (port->string
       (post-pure-port
        (string->url "http://127.0.0.1:9111")
        #"hello world"))))

   (test-exn
    "sending data too slowly"
    broken-pipe?
    (lambda _
      (define-values (in out)
        (tcp-connect "127.0.0.1" 9111))

      (parameterize ([current-output-port out])
        (for ([c (in-string "POST / HTTP/1.1\r\n")])
          (display c)
          (flush-output)
          (sleep 0.25)))))))
