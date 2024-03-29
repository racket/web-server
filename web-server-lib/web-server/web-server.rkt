#lang racket/base

(require racket/match
         net/tcp-sig
         (prefix-in raw: net/tcp-unit)
         racket/unit
         racket/async-channel
         racket/contract
         (only-in racket/tcp listen-port-number?)
         openssl
         web-server/dispatchers/dispatch
         web-server/safety-limits
         (submod web-server/safety-limits private)
         web-server/private/dispatch-server-sig
         web-server/private/dispatch-server-unit
         web-server/private/raw-dispatch-server-connect-unit
         web-server/web-config-sig
         web-server/web-server-sig
         web-server/web-server-unit
         (prefix-in http: web-server/http/request)
         (prefix-in http/private: (submod web-server/http/request private))
         (prefix-in http: web-server/http/response))

(provide/contract
 [serve
  (->* (#:dispatch dispatcher/c)
       (#:confirmation-channel (or/c #f async-channel?)
        #:connection-close? boolean?
        #:dispatch-server-connect@ (unit/c (import) (export dispatch-server-connect^))
        #:tcp@ (unit/c (import) (export tcp^))
        #:port listen-port-number?
        #:listen-ip (or/c #f string?)
        #:max-waiting timeout/c
        #:initial-connection-timeout timeout/c
        #:safety-limits safety-limits?)
       (-> any/c))]
 [serve/ports
  (->* (#:dispatch dispatcher/c)
       (#:confirmation-channel (or/c #f async-channel?)
        #:connection-close? boolean?
        #:dispatch-server-connect@ (unit/c (import) (export dispatch-server-connect^))
        #:tcp@ (unit/c (import) (export tcp^))
        #:ports (listof listen-port-number?)
        #:listen-ip (or/c #f string?)
        #:max-waiting timeout/c
        #:initial-connection-timeout timeout/c
        #:safety-limits safety-limits?)
       (-> any/c))]
 [serve/ips+ports
  (->* (#:dispatch dispatcher/c)
       (#:confirmation-channel (or/c #f async-channel?)
        #:connection-close? boolean?
        #:dispatch-server-connect@ (unit/c (import) (export dispatch-server-connect^))
        #:tcp@ (unit/c (import) (export tcp^))
        #:ips+ports (listof (cons/c (or/c #f string?)
                                    (listof listen-port-number?)))
        #:max-waiting timeout/c
        #:initial-connection-timeout timeout/c
        #:safety-limits safety-limits?)
       (-> any/c))]
 [raw:dispatch-server-connect@ (unit/c (import) (export dispatch-server-connect^))]
 [make-ssl-connect@
  (-> path-string? path-string?
      (unit/c (import) (export dispatch-server-connect^)))]
 [do-not-return (-> none/c)]
 [serve/web-config@
  (->*
   ((unit/c (import) (export web-config*^)))
   (#:dispatch-server-connect@ (unit/c (import) (export dispatch-server-connect^))
    #:tcp@ (unit/c (import) (export tcp^)))
   (-> any/c))])

(define (make-ssl-connect@ server-cert-file server-key-file)
  (define the-ctxt
    (ssl-make-server-context))
  (ssl-load-certificate-chain! the-ctxt server-cert-file)
  (ssl-load-private-key! the-ctxt server-key-file)
  (define-unit ssl:dispatch-server-connect@
    (import) (export dispatch-server-connect^)
    (define (port->real-ports ip op)
      (ports->ssl-ports	ip op
                        #:mode 'accept
                        #:context the-ctxt)))
  ssl:dispatch-server-connect@)

(define (do-not-return)
  (sync never-evt))

(define (serve
         #:dispatch dispatch
         #:confirmation-channel [confirmation-channel #f]
         #:connection-close? [connection-close? #f]
         #:dispatch-server-connect@ [dispatch-server-connect@ raw:dispatch-server-connect@]
         #:tcp@ [tcp@ raw:tcp@]
         #:port [port 80]
         #:listen-ip [listen-ip #f]
         #:max-waiting [_max-waiting 511]
         #:initial-connection-timeout [_timeout 60]
         #:safety-limits [safety-limits (make-safety-limits
                                         #:max-waiting _max-waiting
                                         #:request-read-timeout _timeout)])
  (define read-request
    (http/private:make-read-request
     #:connection-close? connection-close?
     #:safety-limits safety-limits))
  (define-unit-binding a-dispatch-server-connect@
    dispatch-server-connect@ (import) (export dispatch-server-connect^))
  (define-unit-binding a-tcp@
    tcp@ (import) (export tcp^))
  (define-compound-unit/infer dispatch-server@/tcp@
    (import dispatch-server-config*^)
    (link a-dispatch-server-connect@ a-tcp@ dispatch-server-with-connect@)
    (export dispatch-server^))
  (define-values/invoke-unit
    dispatch-server@/tcp@
    (import dispatch-server-config*^)
    (export dispatch-server^))
  (serve #:confirmation-channel confirmation-channel))

(define (serve/ports
         #:dispatch dispatch
         #:confirmation-channel [confirmation-channel #f]
         #:connection-close? [connection-close? #f]
         #:dispatch-server-connect@ [dispatch-server-connect@ raw:dispatch-server-connect@]
         #:tcp@ [tcp@ raw:tcp@]
         #:ports [ports (list 80)]
         #:listen-ip [listen-ip #f]
         #:max-waiting [_max-waiting 511]
         #:initial-connection-timeout [_timeout 60]
         #:safety-limits [limits (make-safety-limits
                                  #:max-waiting _max-waiting
                                  #:request-read-timeout _timeout)])
  (define shutdowns
    (map (lambda (port)
           (serve
            #:dispatch dispatch
            #:confirmation-channel confirmation-channel
            #:connection-close? connection-close?
            #:dispatch-server-connect@ dispatch-server-connect@
            #:tcp@ tcp@
            #:port port
            #:listen-ip listen-ip
            #:safety-limits limits))
         ports))
  (lambda ()
    (for-each apply shutdowns)))

(define (serve/ips+ports
         #:dispatch dispatch
         #:confirmation-channel [confirmation-channel #f]
         #:connection-close? [connection-close? #f]
         #:dispatch-server-connect@ [dispatch-server-connect@ raw:dispatch-server-connect@]
         #:tcp@ [tcp@ raw:tcp@]
         #:ips+ports [ips+ports (list (cons #f (list 80)))]
         #:max-waiting [_max-waiting 511]
         #:initial-connection-timeout [_timeout 60]
         #:safety-limits [limits (make-safety-limits
                                  #:max-waiting _max-waiting
                                  #:request-read-timeout _timeout)])
  (define shutdowns
    (map (match-lambda
           [(list-rest listen-ip ports)
            (serve/ports
             #:dispatch dispatch
             #:confirmation-channel confirmation-channel
             #:connection-close? connection-close?
             #:dispatch-server-connect@ dispatch-server-connect@
             #:tcp@ tcp@
             #:ports ports
             #:listen-ip listen-ip
             #:safety-limits limits)])
         ips+ports))
  (lambda ()
    (for-each apply shutdowns)))

; serve/config@ : configuration -> (-> void)
(define (serve/web-config@
         config@
         #:dispatch-server-connect@
         [dispatch-server-connect@ raw:dispatch-server-connect@]
         #:tcp@ [tcp@ raw:tcp@])
  (define-unit-binding a-dispatch-server-connect@
    dispatch-server-connect@ (import) (export dispatch-server-connect^))
  (define-unit-binding a-tcp@
    tcp@ (import) (export tcp^))
  (define-unit m@ (import web-server^) (export)
    (init-depend web-server^)
    (serve))
  (define-unit-binding c@ config@ (import) (export web-config*^))
  (invoke-unit
   (compound-unit/infer
    (import)
    (link a-dispatch-server-connect@ a-tcp@ c@ web-server-with-connect@ m@)
    (export))))
