;; Default choice for writing module servlets
(module servlet mzscheme
  (require (lib "contract.ss")
           (all-except "request-parsing.ss" request-bindings)
           "servlet-tables.ss"
           "response.ss"
           "servlet-helpers.ss"
           "xexpr-callback.ss")

  (provide/contract
    (send/back (response? . -> . any))
    (send/finish (response? . -> . any))
    (send/suspend ((string? . -> . response?) . -> . request?))
    (send/forward ((string? . -> . response?) . -> . request?))
    (send/suspend/callback (xexpr/callback? . -> . any))
    )

  (provide
    (all-from "servlet-helpers.ss")
    (all-from "xexpr-callback.ss")
    )


  ;; ************************************************************
  ;; EXPORTS

  ;; send/back: response -> void
  ;; send a response and don't clear the continuation table
  (define (send/back resp)
    (output-response
     (servlet-context-connection (thread-cell-ref current-servlet-context))
     resp)
    ((servlet-context-suspend (thread-cell-ref current-servlet-context))))

  ;; send/finish: response -> void
  ;; send a response and clear the continuation table
  (define (send/finish resp)
    (clear-continuations!)
    (send/back resp))

  ;; send/suspend: (url -> response) -> request
  ;; send a response and apply the continuation to the next request
  (define (send/suspend response-generator)
    (let/cc k
      (output-response
       (servlet-context-connection (thread-cell-ref current-servlet-context))
       (response-generator (store-continuation! k)))
      ((servlet-context-suspend (thread-cell-ref current-servlet-context)))))

  ;; send/forward: (url -> response) -> request
  ;; clear the continuation table, then behave like send/suspend
  (define (send/forward response-generator)
    (clear-continuations!)
    (send/suspend response-generator))

  ;; send/suspend/callback : xexpr/callback? -> void
  ;; send/back a response with callbacks in it; send/suspend those callbacks.
  (define (send/suspend/callback p-exp)
    (let/cc k0
      (send/back
        (replace-procedures
          p-exp (lambda (proc)
                  (let/cc k1 (k0 (proc (send/suspend k1)))))))))

  
  ;; ************************************************************
  ;; HELPERS

  ;; store-continuation!: continuation -> url-string
  ;; store a continuation in the k-table for the current servlet-instance
  (define (store-continuation! k)
    (let* ([ctxt  (thread-cell-ref current-servlet-context)]
           [inst (servlet-context-instance ctxt)]
           [next-k-id (servlet-instance-next-k-id inst)]
           [k-table (servlet-instance-k-table inst)])
      (set-servlet-instance-next-k-id! inst (add1 next-k-id))
      (hash-table-put! k-table next-k-id k)
      (embed-ids
       (servlet-instance-invoke-id inst) next-k-id
       (request-uri (servlet-context-request ctxt)))))

  ;; clear-continuations! -> void
  ;; replace the k-table for the current servlet-instance
  (define (clear-continuations!)
    (set-servlet-instance-k-table!
     (servlet-context-instance
      (thread-cell-ref current-servlet-context))
     (make-hash-table)))

  ;; replace-procedures : xexpr/callbacks? (xexpr/callbacks? -> xexpr?) -> xexpr?
  ;; Change procedures to the send/suspend of a k-url
  (define (replace-procedures p-exp p->a)
    (cond
      ((list? p-exp) (map (lambda (p-e) (replace-procedures p-e p->a))
                          p-exp))
      ((procedure? p-exp) (p->a p-exp))
      (else p-exp)))

)
