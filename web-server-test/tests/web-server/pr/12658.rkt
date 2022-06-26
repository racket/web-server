#lang racket/base
(require rackunit
         "12658-mod.rkt")

(check-exn
 (lambda (e)
   (and (exn:fail:contract:arity? e)
        (regexp-match? #rx"go: arity mismatch;\n the expected number of arguments does not match the given number\n  expected: 0\n  given: 1"
                       (exn-message e))))
 (lambda () (go 42)))
