#lang racket/base
(require rackunit
         "12658-mod.rkt")

(check-exn
 exn:fail:contract:arity?
 (lambda () (go 42))
 #rx"go: arity mismatch;\n the expected number of arguments does not match the given number\n  expected: 0\n  given: 1")


