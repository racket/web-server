(module web-server-unit mzscheme
  (provide web-server@)
  (require "sig.ss"
           "internal-structs.ss"
           "configuration-structures.ss"
           "request-parsing.ss"
           "response-encoding.ss"
           "dispatcher.ss"
           "servlet-helpers.ss"
           "servlet-tables.ss"
           "servlet-sig.ss"
           "servlet.ss"
           "util.ss"
           "channel.ss"
           "session.ss"
           (lib "unitsig.ss")
           (lib "thread.ss") ;for copy-port
           (lib "xml.ss" "xml")
           (lib "tcp-sig.ss" "net")
           (lib "url.ss" "net")
           )
  
  (define web-server@
    (unit/sig web-server^
      (import net:tcp^ (config : web-config^))
   
      ;; session collections
      ;; for timeout model:
      (define-values (create-connection-session kill-all-connections!)
        (make-session-collection))
      (define-values (create-instance-session kill-all-instances!)
        (make-session-collection))
      
      ;; for session-queue model:
      ;; just a prototype
      ;      (define the-memory-threshold 45000000)
      ;      (define-values (create-connection-session kill-all-connections!)
      ;        (make-session-collection the-memory-threshold))
      ;      (define-values (create-instance-session kill-all-instances!)
      ;        (make-session-collection the-memory-threshold))



      
      ; -------------------------------------------------------------------------------
      ; The Server
      
      ; serve : -> -> Void
      ; to start the server and return a thunk to shut it down
      ; If tcp-listen fails, the exception will be raised in the caller's thread.
      (define (serve)
        (let ([server-custodian (make-custodian)])
          (parameterize ([current-custodian server-custodian])
            (let ([get-ports
                   (let ([listener (tcp-listen config:port config:max-waiting #t config:listen-ip)])
                     (lambda () (tcp-accept listener)))])
              (thread
               (lambda ()
                 (server-loop server-custodian get-ports
                              
                              ; more here - log the dropped connection?
                              void)))))
          (lambda () (custodian-shutdown-all server-custodian))))
      
      
      ; -------------------------------------------------------------------------------
      ; The Server Loop
      
      ; server-loop : custodian (-> iport oport) (-> void) -> void
      ; note - connection-lost is used by the development environment
      (define (server-loop top-custodian listener connection-lost)
        (let exception-loop ()
          (with-handlers ([exn:i/o:tcp? (lambda (exn) (exception-loop))]
                          [void (lambda (exn)
                                  (fprintf (current-error-port) "server-loop exn: ~a" exn)
                                  (exception-loop))])
            (let listener-loop ()
              (let ([connection-cust (make-custodian)])
                (parameterize ([current-custodian connection-cust])
                  (let-values ([(ip op) (listener)])
                    (let* ([shutdown
                            (lambda ()
                              (close-input-port ip) ; DEBUG - pipes
                              (close-output-port op) ; DEBUG - pipes
                              (custodian-shutdown-all connection-cust))]
                           [ses (create-connection-session
                                 config:initial-connection-timeout
                                 (make-connection ip op #f) shutdown)]
                           [kill-this-session (lambda () (kill-session! ses))])
                      (if ses
                          (thread (lambda ()
                                    (with-handlers ([exn:i/o:port:closed?
                                                     (lambda (exn)
                                                       (connection-lost)
                                                       (kill-this-session))]
                                                    [exn?
                                                     (lambda (exn)
                                                       (kill-this-session)
                                                       )])
                                      (serve-connection top-custodian ses)
                                      (kill-this-session))))
                          (shutdown))))))
              (listener-loop)))))
      
      ; serve-connection : custodian session -> Void
      ; to respond to all the requests on an http connection
      ; (Currently only the first request is answered.)
      (define (serve-connection top-custodian ses)
        (let* ([conn (session->resource ses)]
               [ip (connection-i-port conn)])
          (let connection-loop ()
            (let-values ([(method uri-string major-version minor-version)
                          (read-request-line ip)])
              (let* ([headers (read-headers ip)]
                     [uri     (string->url
                               (bytes->string/utf-8 uri-string))]
                     [host    (get-host uri headers)]
                     [host-conf (config:virtual-hosts host)])
                ; more here - don't extract host-ip and client-ip twice (leakage)
                (let-values ([(host-ip client-ip) (tcp-addresses ip)])
                  ((host-log-message host-conf) host-ip client-ip method uri host)
                  (let ([close (close-connection? headers
                                                  (string->number (bytes->string/utf-8 major-version))
                                                  (string->number (bytes->string/utf-8 minor-version))
                                                  client-ip host-ip)])
                    (set-connection-close?! conn close)
                    (dispatch top-custodian method host-conf uri headers ses)
                    (renew-session! ses config:initial-connection-timeout)
                    (unless close (connection-loop)))))))))
      
      ; --------------------------------------------------------------------------
      ; DISPATCH
      
      ; host-table = str -> host
      ; access-table = (hashtable-of base:sym -> (path:str name:sym password:str -> (+ false str)))
      ; instance-table = (hashtable-of sym Cgi-instance)
      ; script-table : (hashtable-of sym script)
      ; script = (unit servlet^ -> response)
      
      ; dispatch : custodian Method Host URL x-table session -> Void
      ; to respond to an HTTP request
      (define (dispatch top-custodian method host-info uri headers ses)
        (let ([path (url-path uri)]
              [conn (session->resource ses)])
          (cond
            [(access-denied? method uri headers host-info config:access)
             => (lambda (realm)
                  (renew-session! ses (timeouts-password (host-timeouts host-info)))
                  (request-authentication conn method uri host-info realm))]
            
            [((host-dispatcher host-info) path)
             => (lambda (a-resource)
                  (cond
                    [(static-resource? a-resource)
                     (file-content-producer a-resource method uri headers ses host-info)]
                    [(dynamic-resource? a-resource)
                     (renew-session! ses (timeouts-servlet-connection (host-timeouts host-info)))
                     ; more here - make timeout proportional to size of bindings
                     (servlet-content-producer a-resource top-custodian method uri headers conn host-info)]))]
            
            [(conf-prefix? path)
             (cond
               [(string=? "/conf/refresh-servlets" path)
                ; more here - this is broken - only out of date or specifically mentioned
                ; scripts should be flushed.  This destroys persistent state!
                (set-box! config:scripts (make-hash-table 'equal))
                (kill-all-instances!)
                (report-error conn method ((responders-servlets-refreshed (host-responders host-info))))]
               [(string=? "/conf/refresh-passwords" path)
                ; more here - send a nice error page
                (hash-table-put! config:access host-info (read-passwords host-info))
                (report-error conn method ((responders-passwords-refreshed (host-responders host-info))))]
               [else (report-error conn method (responders-file-not-found uri))])]
            
            [else
             (report-error conn method (responders-file-not-found uri))])))

      ;; conf-prefix?: string -> (union (listof string) #f)
      ;; does the path string have "/conf/" as a prefix?
      (define conf-prefix?
        (let ([conf-re (regexp "^/conf/.*")])
          (lambda (str)
            (regexp-match conf-re str))))
      
      ; --------------------------------------------------------------------------
      ; SERVING SERVLETS:
      
      (define FILE-FORM-REGEXP (regexp "multipart/form-data; *boundary=(.*)"))
      
      ; servlet-content-producer : resource custodian Method URL Bindings connection host -> Void
      ; to find and run a servlet program, wait for the result, and output the page
      (define (servlet-content-producer a-resource custodian meth uri headers conn host-info)
        (if (eq? meth 'head)
            (output-headers conn 200 "Okay")
            (let ([binds (read-bindings/handled conn meth uri headers host-info)])
              ; more here - keep one channel per connection instead of creating new ones
              (let ([response-channel (make-async-channel)])
                (let-values ([(host-ip client-ip) (tcp-addresses (connection-o-port conn))])
                  (if (url-params uri)
                      (resume-servlet response-channel meth uri headers binds host-ip client-ip)
                      (start-servlet a-resource custodian response-channel meth uri headers binds host-info host-ip client-ip)))
                (output-page/port conn (async-channel-get response-channel))))))
      
      ;; read-bindings/handled: connection symbol url headers host -> (listof (list (symbol string))
      ;; read the bindings and handle any exceptions
      (define (read-bindings/handled conn meth uri headers host-info)
        (with-handlers ([exn? (lambda (e)
                                (report-error conn meth ((responders-protocol (host-responders host-info))
                                                         (exn-message e))))])
          (read-bindings conn meth uri headers)))
      
      ; cached-load : str -> script
      ; timestamps are no longer checked for performance.  The cache must be explicitly
      ; refreshed (see dispatch).
      (define (cached-load name)
        (hash-table-get (unbox config:scripts)
                        name
                        (lambda () (reload-servlet-script name))))
      
      ; exn:i/o:filesystem:servlet-not-found =
      ; (make-exn:i/o:filesystem:servlet-not-found str continuation-marks str sym)
      (define-struct (exn:i/o:filesystem:servlet-not-found exn:i/o:filesystem) ())
      
      
      ;; reload-servlet-script : str -> script
      ;; The servlet is not cached in the servlet-table, so reload it from the filesystem.
      (define (reload-servlet-script servlet-filename)
        (cond
          [(load-servlet/path servlet-filename)
           => (lambda (svlt)
                (hash-table-put! (unbox config:scripts) servlet-filename svlt)
                svlt)]
          [else
           (raise (make-exn:i/o:filesystem:servlet-not-found
                   (format "Couldn't find ~a" servlet-filename)
                   (current-continuation-marks)
                   servlet-filename 'ill-formed-path))]))
      
      ;; servlet-resolver: x y z -> symbol
      ;; Search help-desk to get a contract for a current-module-name-resolver
      ;; This is the name resolver to install into a servlet namespace
      ;; so that the right version of servlet-library.ss is grabbed when the servlet
      ;; is loaded from the server.
      (define servlet-resolver
        (let ([cr (current-module-name-resolver)])
          (lambda (x y z)
            (if (equal? x '(lib "servlet-library.ss" "web-server"))
                (cr '(lib "servlet-library-internal.ss" "web-server") y #f)
                (cr x y z)))))
      
      ;; load-servlet/path path -> (union #f signed-unit)
      ;; given a string path to a filename attempt to load a servlet
      ;; A servlet-file will contain either
      ;;;; A signed-unit-servlet
      ;;;; A module servlet
      ;;;;;; (two versions, 'v1 and I don't know what 'typed-model-split-store0 is)
      ;;;; A response
      (define (load-servlet/path a-path)
        (and (file-exists? a-path)
             (let ([s (load/use-compiled a-path)])
               (cond
                 ;; signed-unit servlet
                 ; MF: I'd also like to test that s has the correct import signature.
                 [(unit/sig? s) s]
                 ; FIX - reason about exceptions from dynamic require (catch and report if not already)
                 ;; module servlet
                 [(void? s) 
                  (parameterize ([current-namespace (config:make-servlet-namespace)])
                    (eval `(current-module-name-resolver ,servlet-resolver))
                    (let* ([module-name `(file ,(path->string a-path))]
                           [version (dynamic-require module-name 'interface-version)])
                      (case version
                        [(v1)
                         (let ([timeout (dynamic-require module-name 'timeout)]
                               [start (dynamic-require module-name 'start)])
                           (unit/sig ()
                             (import servlet^)
                             (adjust-timeout! timeout)
                             (start initial-request)))]
                        [(typed-model-split-store-0)
                         (let ([constrained (dynamic-require module-name 'type)]
                               [the-servlet (dynamic-require module-name 'servlet)])
                           ; more here - check constraints
                           the-servlet)]
                        [else (raise (format "unknown sevlet version ~e" version))])))]
                 ;; response
                 [(response? s)
                  (letrec ([go (lambda ()
                                 (begin
                                   (set! go (lambda () (load/use-compiled a-path)))
                                   s))])
                    (unit/sig () (import servlet^) (go)))]
                 [else
                  (raise (format "Loading ~e produced ~n~e~n instead of a servlet." a-path s))]))))
      
      
      
      (define URL-PARAMS:REGEXP (regexp "([^\\*]*)\\*(.*)"))
      
      (define (match-url-params x) (regexp-match URL-PARAMS:REGEXP x))
      ;:(define match-url-params (type: (str -> (union false (list str str str)))))
      
      ; resume-servlet : channel method Url bindings bindings str str -> void
      ; to pass the request to the waiting thread that suspended the computation refered to by this url
      ; the url-params can't be #f
      (define (resume-servlet response method uri headers bindings host-ip client-ip)
	(with-handlers ([exn:application:mismatch?
			 (lambda (exn)
                           (timeout-error method uri response))])
	  (cond
	    [(match-url-params (url-params uri))
	     => (lambda (ids)
		  (let* ([invoke-id (string->symbol (cadr ids))]
			 [k-id (string->symbol (caddr ids))]
			 [inst (hash-table-get config:instances invoke-id)])
		    (async-channel-put (servlet-instance-channel inst)
                                       (list response (hash-table-get (servlet-instance-cont-table inst) k-id)
                                             (make-request method uri headers bindings host-ip client-ip)))))]
	    [else (raise "malformed url-params when resuming servlet program")])))

      
      ; start-servlet : resource custodian channel method Url bindings bindings host str str -> void
      ; to start a new servlet program that will handle this request
      (define (start-servlet a-resource top-custodian response method uri headers bindings host-info host-ip client-ip)
	(let* ([invoke-id (string->symbol (symbol->string (gensym 'id)))]
               [respond (lambda (page) (async-channel-put response page))]
               [time-out-seconds (timeouts-default-servlet (host-timeouts host-info))])
	  (let* ([servlet-custodian (make-custodian top-custodian)]
                 [shutdown (lambda (x)
                             (purge-table method uri config:instances invoke-id
                                          (lambda (inst) (hash-table-remove! config:instances invoke-id)))
                             (custodian-shutdown-all servlet-custodian))])
	    (parameterize ([current-custodian servlet-custodian]
			   [read-case-sensitive #t]
			   [exit-handler shutdown])
              (let ([ses (create-instance-session
                          (timeouts-default-servlet (host-timeouts host-info))
                          (make-servlet-instance 0 (make-async-channel) (make-hash-table))
                          (lambda () (shutdown #f)))])
                (when ses
                  (let* ([adjust-timeout!
                          (lambda (n)
                            (renew-session! ses n))]
                         [resume-next-request
                          (gen-resume-next-request (lambda ()
                                                     (renew-session! ses time-out-seconds))
                                                   (lambda (new-response-channel)
                                                     (set! response new-response-channel)))])
                    (parameterize ([current-servlet-stuff (make-servlet-stuff uri invoke-id config:instances respond resume-next-request method)])
                      (thread
                       (lambda ()
                         (send/back
                          (with-handlers ([exn:i/o:filesystem:servlet-not-found?
                                           (lambda (exn)
                                             (decapitate method ((responders-file-not-found (host-responders host-info)) uri)))]
                                          [void (lambda (exn)
                                                  (decapitate method ((responders-servlet-loading (host-responders host-info)) uri exn)))])
                            (let ([servlet-program
                                   (cached-load (url-path->path (resource-base a-resource) (resource-path a-resource)))]
                                  [initial-request (make-request method uri headers bindings host-ip client-ip)])
                              (add-new-instance (session->resource ses) invoke-id config:instances)
                              (with-handlers ([void (lambda (exn)
                                                      (decapitate method ((responders-servlet (host-responders host-info)) uri exn)))])
                                (invoke-unit/sig servlet-program servlet^)))))))))))))))
      
      ; --------------------------------------------------------------------------
      ; SERVING FILES
      
      ; file-content-producer : resource Method URL x-table host -> Void
      (define (file-content-producer a-resource method uri headers ses host-info)
	(serve-file a-resource method uri ses host-info))
      
      ; looks-like-directory : str -> bool
      ; to determine if is url style path looks like it refers to a directory
      (define (looks-like-directory? path)
	(eq? #\/ (string-ref path (sub1 (string-length path)))))
      
      ; serve-file : resource Method url session host -> void
      ; to find the file, including searching for implicit index files, and serve it otu
      (define (serve-file a-resource method uri ses host-info)
        (let ([conn (session->resource ses)]
              [path (url-path->path (resource-base a-resource) (resource-path a-resource))])
          (cond
            [(file-exists? path)
             (output-file method path ses host-info)]
            [(directory-exists? path)
             (let loop ([dir-defaults (host-indices host-info)])
               (cond
                 [(pair? dir-defaults)
                  (let ([full-name (build-path path (car dir-defaults))])
                    (if (file-exists? full-name)
                        (cond
                          [(looks-like-directory? (url-path uri))
                           (output-file method full-name ses host-info)]
                          [else
                           ; more here - look into serving the file _and_ providing a Location header or
                           ; was it a content-location header?
                           (output-headers conn 301 "Moved Permanently"
                                            `(("Location: " ,(url-path uri) "/")))
                           (when (eq? method 'get)
                             (write-xml/content
                              (xexpr->xml `(html (head (title "Add a Slash"))
                                                 (body "Please use " (a ([href ,(string-append (url-path uri) "/")]) "this url") " instead.")))
                              (connection-o-port conn)))])
                        (loop (cdr dir-defaults))))]
                 [else (report-error conn method ((responders-file-not-found (host-responders host-info)) uri))]))]
            [else (report-error conn method ((responders-file-not-found (host-responders host-info)) uri))])))
      
      ; output-file : Method str session host -> void
      ; to serve out the file
      (define (output-file method path ses host-info)
	(let ([conn (session->resource ses)])
          (let ([size (file-size path)]
                [timeouts (host-timeouts host-info)])
            (renew-session! ses (+ (timeouts-file-base timeouts) (* size (timeouts-file-per-byte timeouts))))
            (output-headers conn 200 "Okay"
                            `(("Content-length: " ,size))
                            (file-or-directory-modify-seconds path)
                            (get-mime-type path)))
          (when (eq? method 'get)
            (call-with-input-file path (lambda (in) (copy-port in (connection-o-port conn)))))))
      
      
      ; --------------------------------------------------------------------------
      ; ACCESS CONTROL
      
      ; pass-entry = (make-pass-entry str regexp (list sym str))
      (define-struct pass-entry (domain pattern users))
      
      ; access-denied? : Method URL x-table host Access-table -> (+ false str)
      ; the return string is the prompt for authentication
      (define (access-denied? method uri headers host-info access-table)
        ;; denied?: str sym str -> (U str #f)
        ;; a function to authenticate the user
        (let ([denied?
               
               ;; GregP lookup the authenticator function, if you can't find it, then try to load the
               ;; passwords file for this host.
               (hash-table-get
                access-table host-info
                (lambda ()
                  ; more here - a malformed password file will kill the connection
                  (let ([f (read-passwords host-info)])
                    (hash-table-put! access-table host-info f)
                    f)))])
          (let ([user-pass (extract-user-pass headers)])
            (if user-pass
                (denied? (url-path uri) (lowercase-symbol! (car user-pass)) (cdr user-pass))
                (denied? (url-path uri) fake-user "")))))
      
      (define-struct (exn:password-file exn) ())
      
      ; : host -> (str sym str -> (U str #f))
      ; to produce a function that checks if a given url path is accessible by a given user with a given
      ; password.  If not, the produced function returns a string, prompting for the password.
      ; If the password file does not exist, all accesses are allowed.  If the file is malformed, an
      ; exn:password-file is raised.
      (define (read-passwords host-info)
        (let ([password-path (host-passwords host-info)])
          (with-handlers ([void (lambda (exn)
                                  (raise (make-exn:password-file (format "could not load password file ~a" password-path)
                                                                 (current-continuation-marks))))])
            (if (and (file-exists? password-path) (memq 'read (file-or-directory-permissions password-path)))
                (let ([passwords
                       (let ([raw (load password-path)])
                         (unless (password-list? raw)
                           (raise "malformed passwords"))
                         (map (lambda (x) (make-pass-entry (car x) (regexp (cadr x)) (cddr x)))
                              raw))])
                  
                  ;; string symbol bytes -> (union #f string)
                  (lambda (request-path user-name password)
                    (ormap (lambda (x)
                             (and (regexp-match (pass-entry-pattern x) request-path)
                                  (let ([name-pass (assq user-name (pass-entry-users x))])
                                    (if (and name-pass
                                             (string=?
                                              (cadr name-pass)
                                              (bytes->string/utf-8 password)))
                                        #f
                                        (pass-entry-domain x)))))
                           passwords)))
                (lambda (req user pass) #f)))))
      
      (define fake-user (gensym))
      
      ; password-list? : TST -> bool
      
      ; Note: andmap fails for dotted pairs at end.
      ; This is okay, since #f ends up raising a caught exception anyway.
      (define (password-list? passwords)
        (and (list? passwords)
             (andmap (lambda (domain)
                       (and (pair? domain) (pair? (cdr domain)) (list (cddr domain))
                            (string? (car domain))
                            (string? (cadr domain))
                            (andmap (lambda (x)
                                      (and (pair? x) (pair? (cdr x)) (null? (cddr x))
                                           (symbol? (car x)) (string? (cadr x))))
                                    (cddr domain))))
                     passwords)))
      
      ; request-authentication : connection Method URL iport oport host str bool -> bool
      ;; GregP: at first look, it seems that this gets called when the user
      ;; has supplied bad authentication credentials.
      (define (request-authentication conn method uri host-info realm)
        (report-error conn method
                      ((responders-authentication (host-responders host-info))
                       uri `(WWW-Authenticate . ,(string-append " Basic realm=\"" realm "\"")))))
      
      ))
  )
