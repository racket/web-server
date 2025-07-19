#lang racket/base

(require net/http-client
         racket/match
         racket/port
         rackunit)

(provide make-tests)

(define (make-tests get-port _get-stop)
  (define-check (check-concurrent-requests n min-successes min-failures)
    (let ([sema (make-semaphore)])
      (define result-ch (make-channel))
      (define thds
        (for/list ([_ (in-range n)])
          (thread
           (lambda ()
             (with-handlers ([(λ (_) #t)
                              (λ (e) (channel-put result-ch e))])
               (define conn (http-conn-open "127.0.0.1" #:port (get-port)))
               (semaphore-wait sema)
               (define-values (line _headers in)
                 (http-conn-sendrecv! conn "/"))
               (channel-put result-ch (list line (port->string in)))
               (http-conn-abandon! conn))))))
      (sync (system-idle-evt))
      (for ([_ (in-list thds)])
        (semaphore-post sema))
      (define successes
        (for/sum ([_ (in-list thds)])
          (match (sync result-ch)
            [(? exn?) 0]
            ['(#"HTTP/1.1 200 OK" "ok") 1]
            [_ 0])))
      (when (< successes min-successes)
        (fail (format "expected at least ~a successes but got ~a" min-successes successes)))
      (define failures (- n successes))
      (when (< failures min-failures)
        (fail (format "expected at least ~a failures but got ~a" min-failures failures)))))

  (test-suite
   "max-concurrent"

   (test-suite
    "sequential requests"
    (for ([_ (in-range 10)])
      (define-values (_line _headers in)
        (http-sendrecv "127.0.0.1" #:port (get-port) "/"))
      (check-equal? (port->string in) "ok")))

   (test-suite
    "concurrent requests"

    (check-concurrent-requests 10 10 0)
    ;(check-concurrent-requests 500 10 1)  times out in GitHub Actions
    )))
