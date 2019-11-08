#lang racket/base

(require net/url
         racket/port
         racket/tcp
         rackunit)

(provide make-tests)

(define (broken-pipe? e)
  (and (exn:fail:network? e)
       (equal? (exn:fail:network:errno-errno e) '(32 . posix))))

(define (make-tests port)
  (test-suite
   "read-write"

   (test-case "sending and receiving data"
     (check-regexp-match
      "hello world"
      (port->string
       (post-pure-port
        (string->url (format "http://127.0.0.1:~a" port))
        #"hello world"))))

   (test-exn
    "sending data too slowly"
    broken-pipe?
    (lambda _
      ;; On Racket CS 7.4 the default plumber writes an error to
      ;; standard out when it tries to close the socket. Creating a
      ;; custom plumber seems to fix that problem.
      (parameterize ([current-plumber (make-plumber)])
        (define-values (in out)
          (tcp-connect "127.0.0.1" port))

        (parameterize ([current-output-port out])
          (for ([c (in-string "POST / HTTP/1.1\r\n")])
            (display c)
            (flush-output)
            (sleep 0.25))))))))
