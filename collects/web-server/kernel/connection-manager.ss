(module connection-manager mzscheme
  (require (lib "contract.ss")
           "server-resource-manager.ss")

  ;; TODO: Need some kind of adjust timeout.

  (define-struct (connection server-resource) (i-port o-port custodian close?)
    (make-inspector))

  (provide/contract
   [struct (connection server-resource)
           ([id number?] [expiration-time number?]
            [i-port input-port?] [o-port output-port?] [custodian custodian?]
            [close? boolean?])]
   [start-connection-manager (custodian? . -> . void)]
   [new-connection (number? input-port? output-port? custodian? boolean? . -> . connection?)]
   [kill-connection! (connection? . -> . void)]
   [adjust-connection-timeout! (connection? number? . -> . void)])

  (define the-connection-manager #f)

  ;; start-connection-manager: custodian -> void
  ;; start the connection manager
  (define (start-connection-manager top-cust)
    (set! the-connection-manager
          (start-server-resource-manager
           make-connection
           (lambda (conn-demned)
             (custodian-shutdown-all (connection-custodian conn-demned)))
           top-cust)))

  ;; new-connection: number i-port o-port custodian -> connection
  ;; ask the connection manager for a new connection
  (define (new-connection time-to-live i-port o-port cust close?)
    (new-server-resource the-connection-manager time-to-live i-port o-port cust
                         close?))

  ;; kill-connection!: connection -> void
  ;; kill this connection
  (define (kill-connection! conn-demned)
    (kill-server-resource! the-connection-manager conn-demned))

  ;; adjust-connection-timeout!: connection number -> void
  ;; change the expiration time for this connection
  (define (adjust-connection-timeout! conn time)
    (error "adjust-connection-timeout is not implemented"))
  )
