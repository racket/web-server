#lang racket/base

(require rackunit
         web-server/http
         web-server/http/response
         web-server/private/connection-manager)
(provide response-tests)

;; Adds buffering on top of a pair of pipes so that OS-level buffering
;; of sockets may be simulated.
(define (make-buffered-pipe #:bufsize [bufsize 4096])
  (define-values [in out] (make-pipe))

  (define position 0)
  (define buffer (make-bytes bufsize))

  (define (flush!)
    (write-bytes-avail buffer out 0 position)
    (set! position 0)
    0)

  (define (chomp! bs start-pos end-pos non-blocking? enable-breaks?)
    (define input-size (- end-pos start-pos))

    (define (go current-pos)
      (define capacity (- bufsize position))
      (define chunk-size (min capacity (- end-pos current-pos)))

      (cond
        [(= capacity 0)
         (flush!)
         (go current-pos)]

        [(< current-pos end-pos)
         (bytes-copy! buffer position bs current-pos (+ current-pos chunk-size))
         (set! position (+ position chunk-size))
         (go (+ current-pos chunk-size))]

        [else input-size]))

    (if (> input-size 0)
        (go start-pos)
        (flush!)))

  (define (close)
    (flush!)
    (close-output-port out))

  (values (make-input-port 'in in in (lambda () (close-input-port in)))
          (make-output-port 'out out chomp! close)))

(define (call-with-test-client+server resp f)
  (define-values [in out] (make-buffered-pipe))
  (define chunks (make-channel))
  (define client
    (thread
     (lambda ()
       (define chunk (make-bytes 4096))
       (let loop ()
         (define n-read-or-eof (read-bytes-avail! chunk in))
         (unless (or (eof-object? n-read-or-eof) (port-closed? in))
           (channel-put chunks (subbytes chunk 0 n-read-or-eof))
           (loop))))))

  (define server
    (thread
     (lambda ()
       (define-values [conn-i conn-o] (make-pipe))
       (define connection
         (new-connection (start-connection-manager) 120 conn-i out (current-custodian) #f))
       (output-response connection resp))))

  (dynamic-wind
    void
    (lambda ()
      (f in out chunks))
    (lambda ()
      (kill-thread client)
      (kill-thread server))))


(define response-tests
  (test-suite
   "Response"

   (test-suite
    "response.rkt"

    (test-suite
     "output-response-body/chunked"

     (test-case "the request and each chunk after it is flushed immediately"
       (define write-ready (make-semaphore))
       (define resp
         (response/output
          (lambda (out)
            (semaphore-wait write-ready)
            (write-bytes #"a" out)
            (semaphore-wait write-ready)
            (write-bytes #"b" out)
            (semaphore-wait write-ready))))

       (call-with-test-client+server resp
         (lambda (in out chunks)
           (define headers-chunk (sync/timeout 1 chunks))
           (check-equal? (subbytes headers-chunk 0 8)
                         #"HTTP/1.1")
           (check-equal? (subbytes headers-chunk (- (bytes-length headers-chunk) 4) (bytes-length headers-chunk))
                         #"\r\n\r\n")

           (semaphore-post write-ready)
           (check-equal? (sync/timeout 1 chunks) #"1\r\na\r\n")

           (semaphore-post write-ready)
           (check-equal? (sync/timeout 1 chunks) #"1\r\nb\r\n")

           (semaphore-post write-ready)
           (check-equal? (sync/timeout 1 chunks) #"0\r\n\r\n"))))

     (test-case "responders exit cleanly when the client disappears"
       (define responder-thread #f)
       (define write-ready (make-semaphore))
       (define resp
         (response/output
          (lambda (out)
            (set! responder-thread (current-thread))
            (let loop ()
              (semaphore-wait write-ready)
              (write-bytes #"a" out)
              (loop)))))

       (call-with-test-client+server resp
         (lambda (in out chunks)
           (check-equal? (subbytes (sync/timeout 1 chunks) 0 8) #"HTTP/1.1")

           (semaphore-post write-ready)
           (check-equal? (sync/timeout 1 chunks) #"1\r\na\r\n")
           (close-output-port out)

           (semaphore-post write-ready)
           (check-equal? (sync/timeout 1 chunks) #f)
           (check-true (thread-dead? responder-thread)))))))))


(module+ test
  (require rackunit/text-ui)
  (run-tests response-tests))
