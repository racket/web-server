#lang racket/unit

(require mzlib/thread
         net/tcp-sig
         racket/async-channel
         racket/match
         "../safety-limits.rkt"
         (submod "../safety-limits.rkt" private)
         "connection-manager.rkt"
         "dispatch-server-sig.rkt"
         "web-server-structs.rkt")

;; ****************************************
(import tcp^
        (prefix dispatch-server-connect: dispatch-server-connect^)
        (prefix config: dispatch-server-config*^))

(export dispatch-server^)

(match-define (safety-limits
               #:max-waiting config:max-waiting
               #:response-timeout config:response-timeout)
  config:safety-limits)

(define (async-channel-put* ac v)
  (when ac
    (async-channel-put ac v)))

;; serve: -> -> void
;; start the server and return a thunk to shut it down
(define (serve #:confirmation-channel [confirmation-channel #f])
  (define the-server-custodian (make-custodian))
  (parameterize ([current-custodian the-server-custodian]
                 [current-server-custodian the-server-custodian]
                 #;[current-thread-initial-stack-size 3])
    (define cm (start-connection-manager #:safety-limits config:safety-limits))
    (thread
     (lambda ()
       (run-server
        ;; This is the port argument, but because we specialize listen, it is ignored.
        1
        (handle-connection/cm cm)
        #f
        (lambda (exn)
          ((error-display-handler)
           (format "Connection error: ~a" (exn-message exn))
           exn))
        (lambda (_ mw re)
          (with-handlers ([exn?
                           (λ (x)
                             (async-channel-put* confirmation-channel x)
                             (raise x))])
            (define listener
              (tcp-listen config:port config:max-waiting #t config:listen-ip))
            (let-values
                ([(local-addr local-port end-addr end-port)
                  (tcp-addresses listener #t)])
              (async-channel-put* confirmation-channel local-port))
            listener))
        tcp-close
        tcp-accept
        tcp-accept/enable-break))))
  (lambda ()
    (custodian-shutdown-all the-server-custodian)))

;; serve-ports : input-port output-port -> void
;; returns immediately, spawning a thread to handle
;; the connection
;; NOTE: (GregP) should allow the user to pass in a connection-custodian
(define (serve-ports ip op)
  (define server-cust (make-custodian))
  (parameterize ([current-custodian server-cust]
                 [current-server-custodian server-cust])
    (define connection-cust (make-custodian))
    (define cm (start-connection-manager #:safety-limits config:safety-limits))
    (define handle-connection (handle-connection/cm cm))
    (parameterize ([current-custodian connection-cust])
      (thread
       (lambda ()
         (handle-connection ip op
                            #:port-addresses
                            (lambda (ip)
                              (values "127.0.0.1"
                                      "127.0.0.1"))))))))

;; handle-connection : connection-manager input-port output-port (input-port -> string string) -> void
(define ((handle-connection/cm cm)
         i-ip i-op
         #:port-addresses [real-port-addresses tcp-addresses])
  (define-values (ip op)
    (dispatch-server-connect:port->real-ports i-ip i-op))

  (define (port-addresses some-ip)
    (if (eq? ip some-ip)
        (real-port-addresses i-ip)
        (real-port-addresses ip)))

  (define conn
    (new-connection cm ip op (current-custodian) #f))

  (with-handlers ([exn-expected?
                   (lambda (_)
                     (kill-connection! conn))])
    (let connection-loop ()
      (define-values (req close?)
        (config:read-request conn config:port port-addresses))
      (reset-connection-timeout! conn config:response-timeout)
      (set-connection-close?! conn close?)
      (config:dispatch conn req)
      (if (connection-close? conn)
          (kill-connection! conn)
          (connection-loop)))))

(define/match (exn-expected? _)
  [((or
     ;; This error is "Connection reset by peer" and doesn't really
     ;; indicate a problem with the server. It occurs when our end
     ;; doesn't "realize" that the connection was interrupted (for
     ;; whatever reason) and it attempts to send a packet to the other
     ;; end, to which the other end replies with an RST packet because
     ;; it wasn't expecting anything from our end.
     (exn:fail:network:errno _ _ (cons (or 54 104) 'posix))
     ;; This error is "Broken pipe" and it occurs when our end attempts
     ;; to write to the other end over a closed socket. It can happen
     ;; when a browser suddenly closes the socket while we're sending
     ;; it data (eg. because the user closed a tab).
     (exn:fail:network:errno _ _ (cons 32 'posix))
     ;; This is error is not useful because it just means the other
     ;; side closed the connection early during writing, which we can't
     ;; do anything about.
     (exn:fail "fprintf: output port is closed" _)
     ;; The connection may get timed out while the request is being
     ;; read, when that happens we need to gracefully kill the
     ;; connection.
     (exn:fail (regexp #rx"input port is closed") _)
     ;; There is no explicit connection closing under HTTP/1.1 so we
     ;; attempt to read a a new request off of a connection as soon as
     ;; the previous one was handled.  If the client goes a way, then
     ;; this error gets triggered and we can safely ignore it.
     (exn:fail (regexp #rx"http input closed prematurely") _)))
   #t]

  [(_)
   #f])
