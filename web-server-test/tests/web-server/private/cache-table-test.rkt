#lang racket/base
(require rackunit
         web-server/private/cache-table)
(provide cache-table-tests)

(define cache-table-tests
  (test-suite
   "Cache Table"
   
   (test-case
    "Can construct cache-table"
    (check-not-false (make-cache-table)))
   
   (test-case
    "make-cache-table returns cache-table?"
    (check-true (cache-table? (make-cache-table))))
   
   (test-case
    "cache-table? does not recognize hash-tables"
    (check-false (cache-table? (make-hash))))
   
   (test-case
    "cache-table-lookup: simple"
    (check-true (cache-table-lookup! (make-cache-table) 'foo (lambda () #t))))
   
   (test-case
    "cache-table-lookup: complicated"
    (check-true (let ([ct (make-cache-table)])
                  (cache-table-lookup! ct 'foo (lambda () #t))
                  (cache-table-lookup! ct 'foo (lambda () #f)))))

   (test-case
    "cache-table-clear! is effective"
    (check-false (let ([ct (make-cache-table)])
                   (cache-table-lookup! ct 'foo (lambda () #t))
                   (cache-table-clear! ct)
                   (cache-table-lookup! ct 'foo (lambda () #f)))))

   (test-case
    "cache-table-clear! is selective (1)"
    (check-true (let ([ct (make-cache-table)])
                  (cache-table-lookup! ct 'foo (lambda () #t))
                  (cache-table-lookup! ct 'bar (lambda () #t))
                  (cache-table-clear! ct (list 'bar))
                  (cache-table-lookup! ct 'foo (lambda () #f)))))

   (test-case
    "cache-table-clear! is selective (2)"
    (check-false (let ([ct (make-cache-table)])
                   (cache-table-lookup! ct 'foo (lambda () #t))
                   (cache-table-lookup! ct 'bar (lambda () #t))
                   (cache-table-clear! ct (list 'bar))
                   (cache-table-lookup! ct 'bar (lambda () #f)))))

   (test-case
    "cache-table-clear! is robust"
    (check-true (let ([ct (make-cache-table)])
                  (cache-table-lookup! ct 'foo (lambda () #t))
                  (cache-table-clear! ct (list 'bar 'baz))
                  (cache-table-lookup! ct 'foo (lambda () #f)))))

   (test-case "cache-table-clear! finalizer applies to all"
     (check-true (let ([ct (make-cache-table)]
                       [ks '(a b c d e)]
                       [vs '(42 35 10 12 1830)])
                   (for ([k (in-list ks)] [v (in-list vs)])
                     (cache-table-lookup! ct k (lambda () v)))
                   (cache-table-clear! ct #f (λ (v) (set! vs (remv v vs))))
                   (null? vs))))

   (test-case "cache-table-clear! finalizer applies to all given"
     (check-true (let ([ct (make-cache-table)]
                       [ks '(a b c d e)]
                       [vs '(42 35 10 12 1830)])
                   (for ([k (in-list ks)] [v (in-list vs)])
                     (cache-table-lookup! ct k (lambda () v)))
                   (cache-table-clear! ct '(b d) (λ (v) (set! vs (remv v vs))))
                   (and (andmap (lambda (v) (not (memv v vs))) '(35 12)) #t))))

   (test-case "cache-table-clear! finalizer applies only to given"
     (check-true (let ([ct (make-cache-table)]
                       [ks '(a b c d e)]
                       [vs '(42 35 10 12 1830)])
                   (for ([k (in-list ks)] [v (in-list vs)])
                     (cache-table-lookup! ct k (lambda () v)))
                   (cache-table-clear! ct '(b d) (λ (v) (set! vs (remv v vs))))
                   (and (andmap (lambda (v) (memv v vs)) '(42 10 1830)) #t))))))
