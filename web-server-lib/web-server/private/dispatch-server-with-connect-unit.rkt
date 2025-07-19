#lang racket/unit

(require net/tcp-sig
         racket/async-channel
         racket/match
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
                 [current-server-custodian the-server-custodian])
    (define cm (start-connection-manager #:safety-limits config:safety-limits))
    (define handler (handle-connection/cm cm))
    (define can-break? (break-enabled))
    (define listener-thd
      (thread
       (lambda ()
         (define listener
           (with-handlers ([exn?
                            (lambda (e)
                              (async-channel-put* confirmation-channel e)
                              (raise e))])
             (tcp-listen config:port config:max-waiting #t config:listen-ip)))
         (define-values (_local-addr local-port _remote-addr _remote-port)
           (tcp-addresses listener #t))
         (async-channel-put* confirmation-channel local-port)
         (dynamic-wind
           void
           (lambda ()
             (define paramz (current-parameterization))
             (define-values (do-sync do-accept)
               (if can-break?
                   (values sync/enable-break tcp-accept/enable-break)
                   (values sync tcp-accept)))
             ;; The tcp^ signature does not enforce that the result of
             ;; `tcp-listen' is a synchronizable event, so there could
             ;; exist implementations where it isn't.  For backwards
             ;; compatibility, use `always-evt' when the listener is
             ;; not synchronizable.
             (define listener-evt (if (evt? listener) listener (handle-evt always-evt (λ (_) listener))))
             (define max-concurrent (safety-limits-max-concurrent config:safety-limits))
             (let loop ([in-progress 0]
                        [stopped? #f])
               (define accepting?
                 (and (not stopped?)
                      (in-progress . < . max-concurrent)))
               (define-values (in-progress* stopped?*)
                 (with-handlers ([exn:fail:network?
                                  (lambda (e)
                                    ((error-display-handler)
                                     (format "Connection error: ~a" (exn-message e))
                                     e)
                                    (values in-progress stopped?))])
                   (do-sync
                    (handle-evt
                     (thread-receive-evt)
                     (lambda (_)
                       (match (thread-receive)
                         ['done (values (sub1 in-progress) stopped?)]
                         ['stop (values in-progress #t)])))
                    (handle-evt
                     (if accepting? listener-evt never-evt)
                     (lambda (l)
                       (define custodian (make-custodian))
                       (parameterize ([current-custodian custodian])
                         (parameterize-break #f
                           (define-values (in out)
                             (do-accept l))
                           (define handler-thd
                             (thread
                              (lambda ()
                                (call-with-parameterization
                                  paramz
                                  (lambda ()
                                    (when can-break? (break-enabled #t))
                                    (parameterize ([current-custodian (make-custodian custodian)])
                                      (handler in out)))))))
                           (thread
                            (lambda ()
                              (thread-wait handler-thd)
                              (thread-send listener-thd 'done)
                              (custodian-shutdown-all custodian)))
                           (values (add1 in-progress) stopped?))))))))
               (unless (and stopped?* (zero? in-progress*))
                 (loop in-progress* stopped?*))))
           (lambda ()
             (tcp-close listener))))))
    ;; When there is a grace period, calling stop the first time causes the server to stop accepting
    ;; new connections and waits for in-progress connections to finish. Calling it a second time
    ;; immediately kills the server. This can come in handy when implementing dev tooling where stop
    ;; can be called after a break to begin shutdown, and it can be called again after another break
    ;; to kill the server (eg. if the developer doesn't want to wait for requests in flight at that
    ;; particular moment).
    (let ([stopping? #f])
      (lambda ()
        (cond
          [(and (not stopping?)
                (safety-limits-shutdown-grace-period config:safety-limits))
           => (lambda (timeout)
                (set! stopping? #t)
                (thread-send listener-thd 'stop)
                (sync/timeout timeout listener-thd)
                (custodian-shutdown-all the-server-custodian))]
          [else
           (custodian-shutdown-all the-server-custodian)])))))

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

  (with-handlers ([exn:fail?
                   (lambda (e)
                     (unless (exn-expected? e)
                       ((error-display-handler)
                        (format "Connection error: ~a" (exn-message e))
                        e))
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
     ;; (Can this happen to a unix socket?  Better safe than sorry)
     (exn:fail:filesystem:errno _ _ (cons (or 54 104) 'posix))
     ;; This error is "Broken pipe" and it occurs when our end attempts
     ;; to write to the other end over a closed socket. It can happen
     ;; when a browser suddenly closes the socket while we're sending
     ;; it data (eg. because the user closed a tab).
     (exn:fail:network:errno _ _ (cons 32 'posix))
     (exn:fail:filesystem:errno _ _ (cons 32 'posix))
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
