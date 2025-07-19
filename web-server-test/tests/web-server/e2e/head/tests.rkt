#lang racket/base

(require racket/tcp
         rackunit)

(provide make-tests)

(define (make-tests get-port _get-stop)
  ;; net/url and net/http-client both ignore the body of HEAD requests
  ;; if present so we can't use them to test this.
  (define (request method path)
    (define-values (in out)
      (tcp-connect "127.0.0.1" (get-port)))

    (display (format "~a ~a HTTP/1.1\r\n" method path) out)
    (display "\r\n" out)
    (close-output-port out)

    (define status (read-line in 'return-linefeed))
    (define-values (headers body)
      (for/fold ([headers null]
                 [headers-done? #f]
                 [body null]
                 #:result (values
                           (reverse headers)
                           (reverse body)))
                ([line (in-lines in 'return-linefeed)])
        (if headers-done?
            (values headers headers-done? (cons line body))
            (if (string=? line "")
                (values headers #t body)
                (values (cons line headers) #f body)))))

    (values status headers body))

  (test-suite
   "head"

   (test-case "HEAD requests must not have a body"
     (define (x-example-header? h)
       (regexp-match? #px"X-Example" h))

     (define-values (get-status get-headers get-body)
       (request "GET" "/"))
     (check-equal? get-status "HTTP/1.1 200 OK")
     (check-not-false (findf x-example-header? get-headers))
     (check-equal? get-body '("6" "hello\n" "0" ""))

     (define-values (head-status head-headers head-body)
       (request "HEAD" "/"))
     (check-equal? head-status "HTTP/1.1 200 OK")
     (check-not-false (findf x-example-header? head-headers))
     (check-equal? head-body '()))))
