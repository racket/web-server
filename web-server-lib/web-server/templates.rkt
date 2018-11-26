#lang racket/base
(require xml
         scribble/text
         (for-syntax racket/base
                     racket/list
                     syntax/parse)
         racket/port)

(define-syntax (include-template stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:command-char command-char:expr)) p:expr)
     (quasisyntax/loc stx
       (with-output-to-string
         (lambda ()
           (output (include/text #,@(if (attribute command-char)
                                      (list #'#:command-char #'command-char)
                                      empty)
                                 p)))))]))

(define-syntax include-template/xml
  (syntax-rules ()
    [(_ . p)
     (cdata #f #f (include-template . p))]))

(define-syntax in
  (syntax-rules ()
    [(_ x xs e ...)
     (for/list ([x xs])
       (begin/text e ...))]))

(provide include-template
         include-template/xml
         in)
