#lang racket/base

(require racket/unit
         net/tcp-sig
         web-server/web-server-sig
         web-server/web-config-sig
         web-server/safety-limits
         (submod web-server/safety-limits private)
         web-server/private/dispatch-server-with-connect-unit
         web-server/private/dispatch-server-sig
         web-server/private/web-server-structs
         web-server/private/mime-types
         web-server/configuration/configuration-table-structs
         web-server/private/cache-table
         web-server/private/raw-dispatch-server-connect-unit
         (prefix-in http: web-server/http/request)
         web-server/dispatchers/dispatch
         web-server/servlet/setup
         (prefix-in fsmap: web-server/dispatchers/filesystem-map)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in timeout: web-server/dispatchers/dispatch-timeout)
         (prefix-in passwords: web-server/dispatchers/dispatch-passwords)
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in servlets: web-server/dispatchers/dispatch-servlets)
         (prefix-in path-procedure: web-server/dispatchers/dispatch-pathprocedure)
         (prefix-in log: web-server/dispatchers/dispatch-logresp)
         (prefix-in host: web-server/dispatchers/dispatch-host)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in lift: web-server/dispatchers/dispatch-lift))

(provide web-server-with-connect@
         web-server@)

(define-unit web-config*@->dispatch-server-config*@
  (import (prefix config: web-config*^))
  (export dispatch-server-config*^)
  (init-depend web-config*^)

  (define safety-limits config:safety-limits)
  
  (define read-request
    (http:make-read-request #:safety-limits safety-limits))

  (define port config:port)
  (define listen-ip config:listen-ip)

  ;; dispatch : connection request -> void
  (define dispatch-cache (make-cache-table))
  (define dispatch
    (host:make
     (lambda (host)
       (cache-table-lookup!
        dispatch-cache host
        (lambda ()
          (parameterize ([current-custodian (current-server-custodian)])
            (host-info->dispatcher
             (config:virtual-hosts (symbol->string host)))))))))

  ;; host-info->dispatcher : host-info -> conn request -> void
  (define (host-info->dispatcher host-info)
    (define core-dispatcher (host-info->dispatcher* host-info))
    (sequencer:make
     (timeout:make (safety-limits-request-read-timeout safety-limits)) ;; ????
     (if (and (host-log-format host-info)
              (host-log-path host-info))
         (log:make #:format (log:log-format->format (host-log-format host-info))
                   #:log-path (host-log-path host-info)
                   core-dispatcher)
         core-dispatcher)))

  (define (host-info->dispatcher* host-info)
    (sequencer:make
     (if (host-passwords host-info)
         (let-values ([(update-password-cache! password-check)
                       (passwords:password-file->authorized? (host-passwords host-info))])
           (sequencer:make
            (timeout:make (timeouts-password (host-timeouts host-info)))
            (passwords:make
             (passwords:make-basic-denied?/path
              password-check)
             #:authentication-responder (responders-authentication (host-responders host-info)))
            (path-procedure:make "/conf/refresh-passwords"
                                 (lambda _
                                   (update-password-cache!)
                                   ((responders-passwords-refreshed (host-responders host-info)))))))
         (lambda (conn req) (next-dispatcher)))
     (path-procedure:make "/conf/collect-garbage"
                          (lambda _
                            (collect-garbage)
                            ((responders-collect-garbage (host-responders host-info)))))
     (let-values ([(clear-cache! url->servlet)
                   (servlets:make-cached-url->servlet
                    (fsmap:filter-url->path
                     #rx"\\.(ss|scm|rkt|rktd)$"
                     (fsmap:make-url->valid-path
                      (fsmap:make-url->path (paths-servlet (host-paths host-info)))))
                    (make-default-path->servlet
                     #:make-servlet-namespace config:make-servlet-namespace
                     #:timeouts-default-servlet (timeouts-default-servlet (host-timeouts host-info))))])
       (sequencer:make
        (path-procedure:make "/conf/refresh-servlets"
                             (lambda _
                               (clear-cache!)
                               ((responders-servlets-refreshed (host-responders host-info)))))
        (sequencer:make
         (timeout:make (timeouts-servlet-connection (host-timeouts host-info)))
         (servlets:make url->servlet
                        #:responders-servlet-loading (responders-servlet-loading (host-responders host-info))
                        #:responders-servlet (responders-servlet (host-responders host-info))))))
     (files:make #:url->path (fsmap:make-url->path (paths-htdocs (host-paths host-info)))
                 #:path->mime-type (make-path->mime-type (paths-mime-types (host-paths host-info)))
                 #:indices (host-indices host-info))
     (lift:make (responders-file-not-found (host-responders host-info))))))



(define-compound-unit/infer web-server-with-connect@
  (import tcp^ dispatch-server-connect^ web-config*^)
  (export web-server^)
  (link web-config*@->dispatch-server-config*@ dispatch-server-with-connect@))


(define-compound-unit/infer web-server@
  (import tcp^ web-config*^)
  (export web-server^)
  (link [([ws : web-server^]) web-server-with-connect@]
        [([dsp : dispatch-server-connect^]) raw:dispatch-server-connect@]))
