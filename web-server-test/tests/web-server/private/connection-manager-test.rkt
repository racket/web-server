#lang racket/base
(require rackunit
         web-server/private/connection-manager
         web-server/private/timer)
(provide connection-manager-tests)

(module+ test
  (require rackunit/text-ui)
  (run-tests connection-manager-tests))

(define cm (start-connection-manager))

(define (make-connection&evt ttl ib ob [cust (make-custodian)] [close? #t])
  (define conn
    (new-connection cm ttl ib ob cust close?))

  ;; Modify the timer s.t. it fires an event when its action completes.
  ;; This means we don't have to rely on static sleeps to test this
  ;; code, although it does couple the tests to the implementation.
  (define chan (make-channel))
  (define timer (connection-timer conn))
  (define action (timer-action timer))
  (revise-timer!
   timer
   (max 0 (- (timer-deadline timer)
             (current-inexact-milliseconds)))
   (lambda ()
     (dynamic-wind
       void
       action
       (lambda ()
         (channel-put chan 'done)))))

  ;; Ensure the connection object is retained for longer than the
  ;; timer, otherwise things will flake out.
  (define evt (handle-evt chan (lambda _ conn)))
  (values conn evt))

(define connection-manager-tests
  (test-suite
   "Connection Manager"

   (test-case "Input closed"
     (define ib (open-input-bytes #""))
     (define ob (open-output-bytes))
     (define-values (_conn evt)
       (make-connection&evt 1 ib ob))

     (sync/timeout 10 evt)
     (check-true
      (with-handlers ([exn? (lambda _ #t)])
        (begin0 #f
          (read ib)))))

   (test-case "Output closed"
     (define ib (open-input-bytes #""))
     (define ob (open-output-bytes))
     (define-values (_conn evt)
       (make-connection&evt 1 ib ob))

     (sync/timeout 10 evt)
     (check-true
      (with-handlers ([exn? (lambda _ #t)])
        (begin0 #f
          (write 1 ob)))))

   (test-case "Early kill"
     (define ib (open-input-bytes #""))
     (define ob (open-output-bytes))
     (define-values (conn evt)
       (make-connection&evt 1 ib ob))

     (kill-connection! conn)
     (check-true
      (and (with-handlers ([exn? (lambda _ #t)])
             (begin0 #f
               (read ib)))
           (with-handlers ([exn? (lambda _ #t)])
             (begin0 #f
               (write 1 ob))))))

   (test-case "Extend timer"
     (define ib (open-input-bytes #""))
     (define ob (open-output-bytes))
     (define st (current-inexact-milliseconds))
     (define-values (conn evt)
       (make-connection&evt 1 ib ob))

     (adjust-connection-timeout! conn 1)
     (sync/timeout 20 evt)
     (check-true
      (and (with-handlers ([exn? (lambda _ #t)])
             (begin0 #f
               (read ib)))
           (with-handlers ([exn? (lambda _ #t)])
             (begin0 #f
               (write 1 ob)))
           (>= (- (current-inexact-milliseconds) st) 2000))))))
