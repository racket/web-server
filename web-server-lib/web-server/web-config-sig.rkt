#lang racket/base

(require racket/unit
         racket/contract
         web-server/private/util
         web-server/safety-limits
         web-server/configuration/namespace
         web-server/configuration/configuration-table-structs)

(provide web-config*^
         web-config^)

(define-signature web-config*^
  ((contracted
    [safety-limits safety-limits?]
    [virtual-hosts (string? . -> . host?)]
    [port port-number?]
    [listen-ip (or/c #f string?)]
    [make-servlet-namespace make-servlet-namespace/c])))


(define-signature web-config^
  extends web-config*^
  ((contracted
    [max-waiting timeout/c]
    [initial-connection-timeout timeout/c])
   (define-values-for-export [safety-limits]
     (make-safety-limits
      #:max-waiting max-waiting
      #:request-read-timeout initial-connection-timeout))))
