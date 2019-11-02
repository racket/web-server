#lang racket/base

(require racket/path
         racket/port
         racket/tcp
         rackunit)

(provide all-e2e-tests)

(define here
  (simplify-path
   (build-path (syntax-source #'here) 'up)))

(define (wait-for-local-port port)
  (let loop ([attempts 1])
    (sync (system-idle-evt))
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (if (> attempts 99)
                           (raise e)
                           (loop (add1 attempts))))])
      (define-values (in out)
        (tcp-connect "127.0.0.1" port))
      (close-output-port out)
      (close-input-port in))))

(define all-e2e-tests
  (make-test-suite
   "e2e"

   (for/list ([test-path (in-list (directory-list here))]
              [port (in-naturals 9111)]
              #:when (directory-exists? test-path)
              #:unless (equal? #"compiled" (path->bytes test-path)))
     (define server-mod-path (build-path test-path "server.rkt"))
     (define tests-mod-path (build-path test-path "tests.rkt"))
     (define stopper #f)
     (make-test-suite
      (path->string (file-name-from-path test-path))
      #:before
      (lambda _
        (define start
          (dynamic-require server-mod-path 'start))

        (set! stopper (start port))
        (wait-for-local-port port))
      #:after
      (lambda _
        (stopper))

      (let ([make-tests (dynamic-require tests-mod-path 'make-tests)])
        (list (make-tests port)))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests all-e2e-tests))
