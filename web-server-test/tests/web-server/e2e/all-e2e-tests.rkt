#lang racket/base

(require racket/path
         racket/port
         rackunit)

(provide all-e2e-tests)

(define here
  (simplify-path
   (build-path (syntax-source #'here) 'up)))

(define all-e2e-tests
  (make-test-suite
   "e2e"

   (for/list ([test-path (in-list (directory-list here))]
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

        (set! stopper (start))
        (sync (system-idle-evt)))
      #:after
      (lambda _
        (stopper))

      (list
       (dynamic-require tests-mod-path 'tests))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests all-e2e-tests))
