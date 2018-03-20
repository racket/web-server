#lang racket/base

(require racket/contract
         web-server/http
         web-server/private/xexpr
         "unsafe/lib.rkt"
         racket/function
         racket/serialize
         syntax/location
         setup/collects
         (for-syntax racket/base
                     syntax/parse))

(provide formlet/c ;macro
         (contract-out
          [xexpr-forest/c contract?]
          [formlet*/c contract?]
          [pure (-> alpha
                    (serial-formlet/c alpha))]
          [cross (-> (formlet/c procedure?)
                     formlet*/c
                     serial-formlet*/c)]
          [cross* (-> (formlet/c (unconstrained-domain-> beta))
                      (formlet/c alpha) ...
                      (serial-formlet/c beta))]
          [xml-forest (-> xexpr-forest/c
                          (serial-formlet/c procedure?))]
          [xml (-> pretty-xexpr/c
                   (serial-formlet/c procedure?))] 
          [text (-> string?
                    (serial-formlet/c procedure?))]
          [tag-xexpr (-> symbol?
                         (listof (list/c symbol? string?))
                         (formlet/c alpha)
                         (serial-formlet/c alpha))]
          [formlet-display (-> (formlet/c alpha)
                               xexpr-forest/c)]
          [formlet-process (-> formlet*/c request?
                               any)]))

(module+ private
  (provide serial-formlet*/c
           serial-formlet/c))

(define alpha any/c)
(define beta any/c)


; Contracts
(define xexpr-forest/c
  (listof pretty-xexpr/c))

(define (formlet/c** processing-proc/c)
  (-> integer? 
      (values xexpr-forest/c
              processing-proc/c
              integer?)))
(define listof-binding
  (listof binding?))
(define-syntax-rule (formlet/c* range/c)
  ;must be macro to allow any for formlet*/c
  (formlet/c** (-> listof-binding range/c)))
(define formlet*/c (formlet/c* any))
(define dynamic-formlet/c
  (case-lambda
    [(single)
     (formlet/c* (coerce-contract 'formlet/c single))]
    [contracts     
     (formlet/c**
      (dynamic->* #:mandatory-domain-contracts (list listof-binding)
                  #:range-contracts (map (curry coerce-contract 'formlet/c)
                                         contracts)))]))
(define quote-this-module-path
  (path->collects-relative (quote-module-path)))
(define-syntax formlet/c
  (syntax-parser
    [(_ range ...)
     #:declare range (expr/c #'contract?
                             #:name "range contract argument")
     #'(formlet/c** (-> listof-binding
                        (values (coerce-contract 'formlet/c range.c) ...)))]
    [name:id
     #`(contract
        (-> contract? (... ...) contract?)
        dynamic-formlet/c
        quote-this-module-path
        (path->collects-relative (quote-module-path))
        "formlet/c"
        #'name)]))

(define serial-formlet*/c
  (and/c serializable? formlet*/c))

(define-syntax-rule (serial-formlet/c sub ...)
  (and/c serializable? (formlet/c sub ...)))
