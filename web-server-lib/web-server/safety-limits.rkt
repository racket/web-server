#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/contract
         racket/match
         syntax/parse/define)

;; Also, define-safety-limits/private-submodule generates
;; a private submodule providing accessor functions and a match expander.
(provide timeout/c
         nonnegative-length/c
         positive-count/c
         safety-limits?
         (contract-out
          [make-safety-limits make-safety-limits/c]
          [make-unlimited-safety-limits make-safety-limits/c]))

(define/final-prop timeout/c
  ;; requires a nonnegative real that is not +nan.0
  (>=/c 0))

(define/final-prop nonnegative-length/c
  (or/c exact-nonnegative-integer? +inf.0))

(define/final-prop positive-count/c
  (or/c exact-positive-integer? +inf.0))

(define-syntax-parser define-safety-limits/private-submodule
  [(_ (~seq field:id contract:expr default:expr (~optional (~seq #:unlimited unlimited:expr)))
      ...)
   #:with (kw:keyword ...) (map (compose1 string->keyword symbol->string)
                                (syntax->datum #'(field ...)))
   #:with (pattern-desc ...)
   (for/list ([kw (in-list (syntax->datum #'(kw ...)))])
     #`'#,(datum-intern-literal (format "~s clause" kw)))
   #:with (safety-limits
           make-safety-limits
           make-unlimited-safety-limits
           make-safety-limits/c)
   (syntax-local-introduce #'(safety-limits
                              make-safety-limits
                              make-unlimited-safety-limits
                              make-safety-limits/c))
   #:with (accessor ...)
   (for/list ([id (in-list (syntax->list #'(field ...)))])
     (format-id id "~a-~a" #'safety-limits id #:subs? #t))
   #'(begin
       (struct safety-limits (field ...)
         #:name -safety-limits
         #:constructor-name -safety-limits)
       (define/final-prop make-safety-limits/c
         (->* [] [(~@ kw contract) ...] safety-limits?))
       (define (make-safety-limits (~@ kw [field default]) ...)
         (-safety-limits field ...))
       (define (make-unlimited-safety-limits (~@ kw [field (~? unlimited +inf.0)]) ...)
         (-safety-limits field ...))
       (define-match-expander safety-limits
         (syntax-parser
           [(_ (~alt (~optional (~seq kw (~var field expr))
                                #:name pattern-desc
                                #:defaults ([field #'_]))
                     ...)
               (... ...))
            #'(-safety-limits field ...)]))
       (module+ private
         (provide safety-limits accessor ...)))])


(define-safety-limits/private-submodule
  max-concurrent positive-count/c 10000 #:unlimited +inf.0
  max-waiting exact-nonnegative-integer? 511 #:unlimited 511 ;; contract from tcp-listen
  request-read-timeout timeout/c 60
  max-request-line-length nonnegative-length/c (* 8 1024)
  max-request-headers nonnegative-length/c 100
  max-request-header-length nonnegative-length/c (* 8 1024)
  max-request-body-length nonnegative-length/c (* 1 1024 1024)
  max-form-data-fields nonnegative-length/c 100
  max-form-data-field-length nonnegative-length/c (* 8 1024)
  max-form-data-files nonnegative-length/c 100
  max-form-data-file-length nonnegative-length/c (* 10 1024 1024)
  form-data-file-memory-threshold nonnegative-length/c (* 1 1024 1024)
  max-form-data-parts nonnegative-length/c (+ max-form-data-fields max-form-data-files)
  max-form-data-header-length nonnegative-length/c (* 8 1024)
  response-timeout timeout/c 60
  response-send-timeout timeout/c 60
  shutdown-grace-period (or/c #f timeout/c) #f)
