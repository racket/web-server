#lang racket/base

(require racket/path
         rackunit)

(provide all-e2e-tests)

(define here (path-only (syntax-source #'here)))

(define all-e2e-tests
  (make-test-suite
   "e2e"

   (for/list ([test-path (in-list (directory-list here))]
              #:when (directory-exists? test-path)
              #:unless (equal? #"compiled" (path->bytes test-path)))
     (define server-mod-path (build-path test-path "server.rkt"))
     (define tests-mod-path (build-path test-path "tests.rkt"))
     (define stop-box (box void))
     (define port-box (box #f))
     (make-test-suite
      (path->string (file-name-from-path test-path))
      #:before
      (lambda ()
        (define start (dynamic-require server-mod-path 'start))
        (let-values ([(stop port) (start)])
          (set-box! stop-box stop)
          (set-box! port-box port)))
      #:after
      (lambda ()
        ((unbox stop-box)))
      (let ([make-tests (dynamic-require tests-mod-path 'make-tests)])
        (list
         (make-tests
          (λ () (unbox port-box))
          (λ () (unbox stop-box)))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests all-e2e-tests))
