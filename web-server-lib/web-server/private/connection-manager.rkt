#lang racket/base
(require racket/contract
         racket/match
         "../safety-limits.rkt"
         (submod "../safety-limits.rkt" private)
         "timer.rkt")

(provide
 connection-manager?
 (contract-out
  [struct connection
    ([id integer?]
     [timer timer?]
     [i-port input-port?]
     [o-port output-port?]
     [custodian custodian?]
     [close? boolean?])]
  [start-connection-manager
   (->* [] [#:safety-limits safety-limits?] connection-manager?)]
  [new-connection
   (case->
    (-> connection-manager? input-port? output-port? custodian? boolean?
        connection?)
    (-> connection-manager? number? input-port? output-port? custodian? boolean?
        connection?))]
  [kill-connection!
   (connection? . -> . any)]
  [adjust-connection-timeout!
   (connection? number? . -> . any)]
  [reset-connection-timeout!
   (connection? number? . -> . any)]))

(module+ private
  (provide (contract-out
            [reset-connection-response-send-timeout!
             (-> connection? any)])))

(struct connection-manager (i tm initial-timeout response-send-timeout))

;; The connection type is public.
;; We actually use the private connection* subtype,
;; which cooperates with start-connection-manager and new-connection
;; to keep safety limits around for reset-connection-response-send-timeout!.
(define-struct connection (id timer i-port o-port custodian close?)
  #:mutable)
(struct connection* connection (response-send-timeout))

;; start-connection-manager: custodian [#:safety-limits safety-limits?] -> connection-manager
;; calls the timer manager
(define (start-connection-manager #:safety-limits [limits (make-unlimited-safety-limits)])
  (connection-manager (box 0)
                      (start-timer-manager)
                      (safety-limits-request-read-timeout limits)
                      (safety-limits-response-send-timeout limits)))

;; new-connection: connection-manager [number] i-port o-port custodian -> connection
;; ask the connection manager for a new connection
(define new-connection
  (case-lambda
    [(cm i-port o-port cust close?)
     (new-connection cm #f i-port o-port cust close?)]
    [(cm time-to-live i-port o-port cust close?)
     (match-define (connection-manager i tm initial-timeout response-send-timeout)
       cm)
     (define conn
       (connection*
        ;; The id is just for debugging and isn't normally useful
        0 ;; (begin0 (unbox i) (set-box! i (add1 (unbox i))))
        #f i-port o-port cust close?
        response-send-timeout))
     (define conn-wb (make-weak-box conn))
     (set-connection-timer!
      conn
      (start-timer tm
                   (or time-to-live initial-timeout)
                   (lambda ()
                     (define conn (weak-box-value conn-wb))
                     (when conn
                       (kill-connection-w/o-timer! conn)))))
     conn]))

;; kill-connection!: connection -> void
;; kill this connection
(define (kill-connection! conn)
  #;(printf "K: ~a\n" (connection-id conn))
  (cancel-timer! (connection-timer conn))
  (kill-connection-w/o-timer! conn))

(define (kill-connection-w/o-timer! conn)
  (with-handlers ([exn:fail:network? void])
    (close-output-port (connection-o-port conn)))
  (with-handlers ([exn:fail:network? void])
    (close-input-port (connection-i-port conn)))
  (custodian-shutdown-all (connection-custodian conn)))

;; adjust-connection-timeout!: connection number -> void
;; change the expiration time for this connection
(define (adjust-connection-timeout! conn time)
  (increment-timer! (connection-timer conn) time))

;; reset-connection-timeout!: connection number -> void
;; reset the expiration time for this connection
(define (reset-connection-timeout! conn time)
  (reset-timer! (connection-timer conn) time))

;; reset-connection-response-send-timeout! : connection? -> any
;; (Private for web-server/http/response)
;; Signal that a chunk of data has been written to the client,
;; so we are entitled to more time.
(define (reset-connection-response-send-timeout! conn)
  (reset-timer! (connection-timer conn)
                (if (connection*? conn)
                    (connection*-response-send-timeout conn)
                    +inf.0)))
