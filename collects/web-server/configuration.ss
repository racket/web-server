; configuration language example
(module configuration mzscheme
  (provide complete-configuration build-path-maybe
           build-developer-configuration
           default-configuration-table-path
           load-configuration
           load-developer-configuration
	   update-configuration)
  (require "configuration-structures.ss"
           "configuration-table-structs.ss"
           "servlet-sig.ss"
           "util.ss"
           "parse-table.ss"
           (lib "unitsig.ss")
	   (lib "url.ss" "net")
	   (lib "date.ss"))
  
  ;(define myprint printf)
  
  (define default-configuration-table-path
    (build-path (collection-path "web-server") "configuration-table"))
  
  ; get-configuration : str -> configuration-table
  (define (get-configuration table-file-name)
    (parse-configuration-table (call-with-input-file table-file-name read)))
  
  ; load-configuration : str -> configuration
  (define (load-configuration table-file-name)
    (complete-configuration (directory-part table-file-name) (get-configuration table-file-name)))
  
  ; load-developer-configuration : str -> configuration
  (define (load-developer-configuration table-file-name)
    (complete-developer-configuration (directory-part table-file-name) (get-configuration table-file-name)))

  ; build-developer-configuration : tst -> configuration-table
  (define (build-developer-configuration s-expr)
    (complete-developer-configuration (directory-part default-configuration-table-path)
                                      (parse-configuration-table s-expr)))
  
  ; : str configuration-table -> configuration
  (define (complete-configuration base table)
    (build-configuration
     table
     (let ([default-host
	     (apply-default-functions-to-host-table
	      base (configuration-table-default-host table) gen-log-message)]
	   [expanded-virtual-host-table
	    (map (lambda (x)
		   (list (regexp (string-append (car x) "(:[0-9]*)?"))
			 (apply-default-functions-to-host-table base (cdr x) gen-log-message)))
		 (configuration-table-virtual-hosts table))])
       (gen-virtual-hosts expanded-virtual-host-table default-host))))
  
  ; : str configuration-table -> configuration
  (define (complete-developer-configuration base table)
    (build-configuration
     table
     (gen-virtual-hosts null (apply-default-functions-to-host-table
			      base
                              (configuration-table-default-host table) ignore-log))))

  ; : configuration-table host-table -> configuration
  (define (build-configuration table the-virtual-hosts)
    (unit/sig web-config^
      (import)
      (define port (configuration-table-port table))
      (define max-waiting (configuration-table-max-waiting table))
      (define listen-ip #f) ; more here - add to configuration table
      (define initial-connection-timeout (configuration-table-initial-connection-timeout table))
      (define virtual-hosts the-virtual-hosts)
      (define access (make-hash-table))
      (define instances (make-hash-table))
      (define scripts (box (make-hash-table)))
      (define make-servlet-namespace the-make-servlet-namespace)))



  ; begin stolen from commander.ss, which was stolen from private/drscheme/eval.ss
  ; FIX - abstract this out to a namespace library somewhere (ask Robby and Matthew)
  
  ; JBC : added error-handler hack; the right answer is only to transfer the 'mred' 
  ; module binding when asked to, e.g. by a field in the configuration file.
  (define to-be-copied-module-specs
    '(mzscheme
      
      ;; allow people to use MrEd primitives from servlets.
      ;;(lib "mred.ss" "mred")
      ;; GregP This should not be allowed in this way, because
      ;; then web-server-text has a dependency on mred that it shouldn't
      ;; (see PR# 6620)
      
      (lib "servlet-sig.ss" "web-server")
      ; internal structs needed for parameter
      (lib "internal-structs.ss" "web-server")))
  (for-each (lambda (x) (with-handlers ([not-break-exn? (lambda (exn) 'dont-care)]) 
                          ; dynamic-require will fail when running web-server-text.
                          ; maybe a warning message in the exception-handler?
                          (dynamic-require x #f))) 
            to-be-copied-module-specs)

  ;; get the names of those modules.
  (define to-be-copied-module-names
    (let ([get-name
	   (lambda (spec)
	     (if (symbol? spec)
		 spec
		 ((current-module-name-resolver) spec #f #f)))])
      (map get-name to-be-copied-module-specs)))
  ; end stolen

  (define (the-make-servlet-namespace)
    (let ([server-namespace (current-namespace)]
	  [new-namespace (make-namespace)])
      (parameterize ([current-namespace new-namespace])
	(for-each (lambda (name) (namespace-attach-module server-namespace name))
		  to-be-copied-module-names)
	new-namespace)))

  ; : (listof (cons sym TST)) -> configuration
  ; more here - this is ugly.  It also does not catch "unbound identifiers" since I use symbols.
  ; I considered several other solutions:
  ; - write the compound unit multiple times (no abstraction)
  ; - use opt-lambda and pass in 'please-use-the-default for unchanged flags
  ; - write three different functional updaters and re-compound the unit 1--3 times
  (define (update-configuration configuration flags)
    (compound-unit/sig
     (import)
     (link
      [config : web-config^ (configuration)]
      [new-config : web-config/local^
		  ((unit/sig web-config/local^
		     (import (raw : web-config/local^))
		     (define port (extract-flag 'port flags raw:port))
		     (define listen-ip (extract-flag 'ip-address flags raw:listen-ip))
		     (define instances (extract-flag 'instances flags raw:instances))
		     (define make-servlet-namespace (extract-flag 'namespace flags raw:make-servlet-namespace)))
		   (config : web-config/local^))])
     (export (open (config : web-config/pervasive^))
	     (open (new-config : web-config/local^)))))
  
  (define TEXT/HTML-MIME-TYPE "text/html")
  
  ; gen-servlet-path : str -> str str -> (U #f str)
  ; to return the pathname of a servlet or #f
  (define (gen-servlet-path servlet-root)
    (lambda (host-name path-from-url)
      (and (servlet? path-from-url)
           (url-path->path servlet-root path-from-url))))
  
  ; error-response : nat str str [(cons sym str) ...] -> response
  ; more here - cache files with a refresh option.
  ; The server should still start without the files there, so the
  ; configuration tool still runs.  (Alternatively, find an work around.)
  (define (error-response code short text-file . extra-headers)
    (make-response/full code short (current-seconds) TEXT/HTML-MIME-TYPE
                        extra-headers
                        (list (read-file text-file))))
  
  ; servlet-loading-responder : url tst -> response
  ; more here - parameterize error based on a configurable file, perhaps?
  ; This is slightly tricky since the (interesting) content comes from the exception.
  (define (servlet-loading-responder url exn)
    (make-response/full 500 "Servlet didn't load"
                        (current-seconds)
                        TEXT/HTML-MIME-TYPE
                        null ; check
                        (list "Servlet didn't load.\n"
                              (if (exn? exn)
                                  (exn-message exn)
                                  (format "~s~n" exn)))))
  
  ; gen-servlet-not-found : str -> url -> response
  (define (gen-servlet-not-found file-not-found-file)
    (lambda (url)
      (error-response 404 "Servlet not found" file-not-found-file)))
  
  ; gen-servlet-responder : str -> url tst -> response
  (define (gen-servlet-responder servlet-error-file)
    (lambda (url exn)
      ; more here - use separate log file
      (printf "Servlet exception: ~s~n"
              (if (exn? exn) (exn-message exn) exn))
      (error-response 500 "Servlet error" servlet-error-file)))
  
  ; gen-servlets-refreshed : str -> -> response
  (define (gen-servlets-refreshed servlet-refresh-file)
    (lambda ()
      (error-response 200 "Servlet cache refreshed" servlet-refresh-file)))
  
  ; gen-passwords-refreshed : str -> -> response
  (define (gen-passwords-refreshed password-refresh-file)
    (lambda ()
      (error-response 200 "Passwords refreshed" password-refresh-file)))
  
  ; gen-authentication-responder : str -> url (cons sym str) -> response
  (define (gen-authentication-responder access-denied-file)
    (lambda (uri recommended-header)
      (error-response 401 "Authorization Required" access-denied-file
                      recommended-header)))
  
  ; gen-protocol-responder : str -> str -> response
  (define (gen-protocol-responder protocol-file)
    (lambda (error-message)
      (error-response 400 "Malformed Request" protocol-file)))
  
  ; gen-file-not-found-responder : str -> url -> response
  (define (gen-file-not-found-responder file-not-found-file)
    (lambda (url)
      (error-response 404 "File not found" file-not-found-file)))
  
  (define servlet? (prefix? "/servlets/"))
  
  ; access-denied? : str sym str -> (U #f str)
  ; (define (access-denied? client-ip user-name password) ???)
  ; The configuration needs a simple way to combine ip and username authentication with
  ; boolean-and, boolean-or, and perhaps others operations.
  ; Using quote in the old password system enabled abstraction, which
  ; I never used.
  ; ...
  
  ; gen-log-message : sym str -> str str sym url str -> str
  ; more here - check apache log configuration formats
  ; other server's include the original request line,
  ; including the major and minor HTTP version numbers
  ; to produce a string that is displayed into the log file
  (define (gen-log-message log-format log-path)
    (let ([out (open-output-file log-path 'append)])
      (lambda (host-ip client-ip method uri host)
        ; do the display all at once by formating first
	(display
	  (format "~s~n"
	    (list 'from client-ip 'to host-ip 'for (url->string uri) 'at 
	      (date->string (seconds->date (current-seconds)) #t)))
	  out))))
  
  ; ignore-log : sym str -> str str sym url str -> str
  (define (ignore-log log-format log-path) void)
  
  ; read-file : str -> str
  (define (read-file path)
    (call-with-input-file path
      (lambda (in) (read-string (file-size path) in))))
  
  ; apply-default-functions-to-host-table : str host-table (sym str -> str str sym url str -> str) -> host
  (define (apply-default-functions-to-host-table web-server-root host-table gen-log-message-maybe)
    (let ([paths (expand-paths web-server-root (host-table-paths host-table))])
      (make-host
       (host-table-indices host-table)
       (gen-servlet-path (paths-servlet paths))
       (gen-log-message-maybe (host-table-log-format host-table) (paths-log paths))
       (paths-passwords paths)
       (let ([m (host-table-messages host-table)]
             [conf (paths-conf paths)])
         (make-responders
          (gen-servlet-responder (build-path-maybe conf (messages-servlet m)))
          servlet-loading-responder
          (gen-authentication-responder (build-path-maybe conf (messages-authentication m)))
          (gen-servlets-refreshed (build-path-maybe conf (messages-servlets-refreshed m)))
          (gen-passwords-refreshed (build-path-maybe conf (messages-passwords-refreshed m)))
          (gen-file-not-found-responder (build-path-maybe conf (messages-file-not-found m)))
          (gen-protocol-responder (build-path-maybe conf (messages-protocol m)))))
       (host-table-timeouts host-table)
       paths)))
  
  ; expand-paths : str paths -> paths
  (define (expand-paths web-server-root paths)
    (let ([host-base (build-path-maybe web-server-root (paths-host-base paths))])
      (make-paths (build-path-maybe host-base (paths-conf paths))
                  host-base
                  (build-path-maybe host-base (paths-log paths))
                  (build-path-maybe host-base (paths-htdocs paths))
                  (build-path-maybe host-base (paths-servlet paths))
                  (build-path-maybe host-base (paths-passwords paths)))))
  
  ; gen-virtual-hosts : (listof (list regexp host)) host ->
  ; str -> host-configuration
  (define (gen-virtual-hosts expanded-virtual-host-table default-host)
    (lambda (host-name-possibly-followed-by-a-collon-and-a-port-number)
      (or (ormap (lambda (x)
                   (and (regexp-match (car x) host-name-possibly-followed-by-a-collon-and-a-port-number)
                        (cadr x)))
                 expanded-virtual-host-table)
          default-host)))
  
  ; build-path-maybe : str str -> str
  (define (build-path-maybe base path)
    (if (absolute-path? path)
        path
        (build-path base path))))
