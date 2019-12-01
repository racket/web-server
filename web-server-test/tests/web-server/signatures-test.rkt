#lang racket

(require rackunit
         syntax/macro-testing
         web-server/private/dispatch-server-sig
         web-server/web-server-sig
         web-server/web-config-sig
         web-server/safety-limits
         syntax/parse/define)

(provide signatures-tests)

;; These tests are mostly a defense against accidentaly
;; making breaking changes to signatures, like adding additional elements.
;; We use `convert-syntax-error` to delay compile-time errors:
;; hopefully the programmer will see the failing test and reconsider the change,
;; rather than seeing that this file doesn't compile and "fixing" it.

(module+ test
  (require rackunit/text-ui)
  (run-tests signatures-tests))

(define-syntax-parser test-unit
  [(_ name:str body:expr)
   (quasisyntax/loc this-syntax
     (test-case name #,(syntax/loc this-syntax
                         (check-not-exn
                          (λ ()
                            (invoke-unit (convert-syntax-error body)))
                          "don't make breaking changes to signatures"))))])

(define signatures-tests
  (test-suite
   "Signatures"
   (test-unit
    "dispatch-server-connect^"
    (unit
      (import)
      (export dispatch-server-connect^)
      (define port->real-ports void)))
   (test-unit
    "dispatch-server^"
    (unit
      (import)
      (export dispatch-server^)
      (define (serve #:confirmation-channel [ach #f])
        void)
      (define serve-ports void)))
   (test-unit
    "web-server^"
    (unit
      (import)
      (export web-server^)
      (define (serve #:confirmation-channel [ach #f])
        void)
      (define serve-ports void)))
   (test-unit
    "dispatch-server-config*^"
    (unit
      (import)
      (export dispatch-server-config*^)
      (define port 80)
      (define listen-ip #f)
      (define dispatch void)
      (define (read-request conn port-n get-addresses)
        (values #t #t))
      (define safety-limits (make-safety-limits))))
   (test-unit
    "dispatch-server-config^"
    (unit
      (import)
      (export dispatch-server-config^)
      (define port 80)
      (define listen-ip #f)
      (define dispatch void)
      (define (read-request conn port-n get-addresses)
        (values #t #t))
      (define max-waiting 5)
      (define initial-connection-timeout 5)))
   (test-unit
    "web-config*^"
    (unit
      (import)
      (export web-config*^)
      (define virtual-hosts void)
      (define port 80)
      (define listen-ip #f)
      (define (make-servlet-namespace #:additional-specs [s null])
        (make-empty-namespace))
      (define safety-limits (make-safety-limits))))
   (test-unit
    "web-config^"
    (unit
      (import)
      (export web-config^)
      (define virtual-hosts void)
      (define port 80)
      (define listen-ip #f)
      (define (make-servlet-namespace #:additional-specs [s null])
        (make-empty-namespace))
      (define max-waiting 5)
      (define initial-connection-timeout 5)))))

