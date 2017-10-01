#lang web-server/base

(define-syntax-rule (require-provide mod ...)
  (begin (require mod ...) (provide (all-from-out mod) ...)))

(require-provide "unsafe/lib.rkt"
                 "unsafe/input.rkt"
                 "unsafe/syntax.rkt"
                 "unsafe/dyn-syntax.rkt"
                 "unsafe/embed.rkt"
                 "unsafe/stateless-send.rkt")
