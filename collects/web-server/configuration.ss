; configuration language example
(module configuration mzscheme
  (provide virtual-hosts
           default-host
           format-log-message
           (all-from-except "configuration-table.ss" default-host-table virtual-host-table))
  (require "configuration-table.ss"
           "configuration-structures.ss"
           "configuration-table-structures.ss"
           "servlet-sig.ss"
           "util.ss"
           (lib "url.ss" "net")
           (lib "etc.ss"))
  
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
                                  (format "~s" exn)))))
  
  ; gen-servlet-not-found : str -> url -> response
  (define (gen-servlet-not-found file-not-found-file)
    (lambda (url)
      (error-response 404 "Servlet not found" file-not-found-file)))
  
  ; gen-servlet-responder : str -> url tst -> response
  (define (gen-servlet-responder servlet-error-file)
    (lambda (url exn)
      ; more here - use separate log file
      (printf "Servlet exception: ~s"
              (if (exn? exn) (exn-message exn) exn))
      (error-response 500 "Servlet error" servlet-error-file)))
  
  ; gen-servlets-refreshed : str -> -> responseapply-default-functions-to-host-table
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
  
  ; format-log-message : str str sym url str -> str
  ; more here - check apache log configuration formats
  ; other server's include the original request line,
  ; including the major and minor HTTP version numbers
  ; to produce a string that is displayed into the log file
  (define (format-log-message host-ip client-ip method uri host)
    (format "~s~n" (list 'from client-ip 'to host-ip 'for (url->string uri))))
  
  ; read-file : str -> str
  (define (read-file path)
    (call-with-input-file path
      (lambda (in) (read-string (file-size path) in))))
  
  ; apply-default-functions-to-host-table : host-table -> host
  (define (apply-default-functions-to-host-table host-table)
    (make-host
     (host-table-indices host-table)
     (gen-servlet-path (host-table-servlet-root host-table))
     format-log-message
     (host-table-passwords host-table)
     (let ([m (host-table-messages host-table)])
       (make-responders
        (gen-servlet-responder (messages-servlet m))
        servlet-loading-responder
        (gen-authentication-responder (messages-authentication m))
        (gen-servlets-refreshed (messages-servlets-refreshed m))
        (gen-passwords-refreshed (messages-passwords-refreshed m))
        (gen-file-not-found-responder (messages-file-not-found m))
        (gen-protocol-responder (messages-protocol m))))
     (host-table-timeouts host-table)
     (host-table-paths host-table)
     (open-output-file (paths-log (host-table-paths host-table)) 'append)))
  
  (define default-host (apply-default-functions-to-host-table default-host-table))
  
  ; expanded-virtual-host-table : (listof (list regexp str))
  ; Replacing the map here 
  (define expanded-virtual-host-table
    (map (lambda (x)
           (list (regexp (string-append (car x) "(:[0-9]*)?"))
                 (apply-default-functions-to-host-table (cdr x))))
         virtual-host-table))
  
  ; virtual-hosts : str -> host-configuration
  (define (virtual-hosts host-name-possibly-followed-by-a-collon-and-a-port-number)
    (or (ormap (lambda (x)
                 (and (regexp-match (car x) host-name-possibly-followed-by-a-collon-and-a-port-number)
                      (cadr x)))
               expanded-virtual-host-table)
        default-host)))