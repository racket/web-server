#lang web-server/base

(define-syntax-rule (require-provide mod ...)
  (begin (require mod ...) (provide (all-from-out mod) ...)))

(require-provide "lib.rkt"
                 "input.rkt"
                 "syntax.rkt"
                 "dyn-syntax.rkt"
                 "embed.rkt")

(require racket/contract
         web-server/private/xexpr
         "unsafe/stateless-send.rkt")

(provide (contract-out
          [send/formlet (->* (formlet*/c)
                             (#:method (or/c "GET" "POST" "get" "post")
                              #:wrap (-> pretty-xexpr/c pretty-xexpr/c))
                             any)]))
