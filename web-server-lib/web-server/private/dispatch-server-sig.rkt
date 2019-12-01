#lang racket/base

(require racket/unit
         racket/contract
         racket/async-channel
         "../safety-limits.rkt"
         web-server/private/util
         web-server/private/connection-manager)

(define-signature dispatch-server^
  ((contracted
    [serve (->* []
                [#:confirmation-channel (or/c #f (async-channel/c (or/c exn? port-number?)))]
                (-> any))]
    [serve-ports (input-port? output-port? . -> . any)])))

(define-signature dispatch-server-connect^
  ((contracted
    [port->real-ports
     (-> input-port? output-port?
         (values input-port? output-port?))])))

(define-signature dispatch-server-config*^
  ((contracted
    [port listen-port-number?]
    [listen-ip (or/c string? #f)]
    [safety-limits safety-limits?]
    [read-request
     (connection?
      listen-port-number?
      (input-port? . -> . (values string? string?))
      . -> .
      (values any/c boolean?))]
    [dispatch
     (-> connection? any/c any)])))

(define-signature dispatch-server-config^
  extends dispatch-server-config*^
  ((contracted
    [max-waiting timeout/c]
    [initial-connection-timeout timeout/c])
   (define-values-for-export [safety-limits]
     (make-safety-limits
      #:max-waiting max-waiting
      #:request-read-timeout initial-connection-timeout))))

(provide
 dispatch-server^
 dispatch-server-connect^
 dispatch-server-config*^
 dispatch-server-config^)
