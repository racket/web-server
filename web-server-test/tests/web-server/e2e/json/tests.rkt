#lang racket/base

(require json
         net/url
         racket/port
         rackunit)

(provide tests)

(define (get-books)
  (read-json (get-pure-port (string->url "http://127.0.0.1:9113/books"))))

(define (post-book e)
  (post-impure-port
   (string->url "http://127.0.0.1:9113/books")
   (call-with-output-bytes
    (lambda (out)
      (write-json e out)))))

(define tests
  (test-suite
   "json"

   (test-equal?
    "no books"
    (get-books)
    null)

   (test-case "adding a valid book"
     (check-regexp-match "200 OK"
                         (port->string
                          (post-book (hasheq 'title "Mechanica"
                                             'author "Euler"))))
     (check-equal?
      (get-books)
      (list (hasheq 'title "Mechanica"
                    'author "Euler"))))

   (test-case "adding an invalid book"
     (check-regexp-match "400 Bad Request"
                         (port->string
                          (post-book (hasheq 'title "Mechanica"))))
     (check-equal?
      (get-books)
      (list (hasheq 'title "Mechanica"
                    'author "Euler"))))

   (test-case "sending invalid JSON"
     (check-regexp-match "500 Internal Server Error"
                         (port->string
                          (post-impure-port
                           (string->url "http://127.0.0.1:9113/books")
                           #"invalid"))))

   (test-exn
    "sending too much data"
    (lambda (e)
      (and (exn:fail? e)
           (regexp-match #rx"Connection ended early" (exn-message e))))
    (lambda _
      (post-impure-port
       (string->url "http://127.0.0.1:9113/books")
       (make-bytes 512))))))
