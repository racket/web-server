; DO NOT DELETE THIS SERVLET,
; unless you never want to reconfigure the Web server again.
; The servlet accepts requests only from the *same machine* as the Web server
; for security purposes.

(module configure mzscheme
  (provide servlet)
  (require (lib "servlet-sig.ss" "web-server")
           (lib "servlet-helpers.ss" "web-server")
           (lib "unitsig.ss")
           (lib "url.ss" "net")
           (lib "etc.ss")
           (lib "pretty.ss")
           (lib "file.ss")
           (rename (lib "configuration.ss" "web-server")
                   build-path-maybe build-path-maybe)
           (lib "configuration-table-structures.ss" "web-server")
           (lib "parse-table.ss" "web-server"))
  
  (define servlet
    (unit/sig ()
      (import servlet^)
      
      (define CONFIGURE-SERVLET-NAME "configure.ss")
      
      (adjust-timeout! (* 12 60 60))
      (error-print-width 800) ; 10-ish lines
      
      ; passwords = (listof realm)
      ; realm = (make-realm str str (listof user-pass))
      (define-struct realm (name pattern allowed))
      
      ; user-pass = (make-user-pass sym str)
      (define-struct user-pass (user pass))
      
      ; default-configuration-path : str
      (define default-configuration-path
        (build-path (collection-path "web-server") "configuration-table"))
      
      (define doc-dir "Defaults/documentation")
      
      ; build-footer : str -> html
      (define (build-footer base)
        (let ([scale (lambda (n) (number->string (round (/ n 4))))])
          `(p "Powered by "
              (a ([href "http://www.plt-scheme.org/"])
                 (img ([width ,(scale 211)] [height ,(scale 76)]
                       [src ,(string-append base doc-dir "/plt-logo.gif")]))))))
      
      (define footer (build-footer "/"))
      
      ; access-error-page : html
      (define access-error-page
        `(html (head (title "Web Server Configuration Access Error"))
               (body ([bgcolor "white"])
                     (p "You must connect to the configuration tool from the machine the server runs on.")
                     ,footer)))
      
      ; permission-error-page : str -> html
      (define (permission-error-page configuration-path)
        `(html (head (title "Web Server Configuration Permissions Error"))
               (body ([bgcolor "white"])
                     (p "You must have read and write access to "
                        (code ,configuration-path)
                        " in order to configure the server."))))
      
      ; check-ip-address : request -> request
      (define (check-ip-address request)
        (unless (string=? "127.0.0.1" (request-host-ip request))
          (send/finish access-error-page))
        request)
      
      (check-ip-address initial-request)
      
      ; more here - abstract with static pages?
      (define web-server-icon
        `(img ([src ,(string-append "/" doc-dir "/web-server.gif")]
               ;[width "123"] [height "115"]
               [width "61"] [height "57"])))
      
      ; send/back : response -> doesn't
      ; more here - consider providing an optimized version from the server
      (define (send/back page)
        (send/suspend (lambda (unused-k-url) page))
        (error 'send/back "Alert! send/back returned: someone is guessing URLs."))
      
      ; interact : (str -> response) -> bindings
      (define (interact page)
        (request-bindings (check-ip-address (send/suspend page))))
      
      ; choose-configuration-file : -> doesn't
      (define (choose-configuration-file)
        (let ([configuration-path (ask-for-configuration-path)])
          (let loop ()
            (if (file-exists? configuration-path)
                (let ([perms (file-or-directory-permissions configuration-path)])
                  ; race condition - changing the permissions after the check
                  ; will result in an exception later (which serves them right)
                  (if (and (memq 'write perms) (memq 'read perms))
                      (configure-top-level configuration-path)
                      (send/finish (permission-error-page configuration-path))))
                (begin (send/suspend (copy-configuration-file configuration-path))
                       (let-values ([(base name must-be-dir) (split-path configuration-path)])
                         (unless (directory-exists? base)
                           (make-directory* base)))
                       (copy-file default-configuration-path configuration-path)
                       (loop))))))
      
      ; copy-configuration-file : str -> html
      (define (copy-configuration-file configuration-path)
        (build-suspender
         '("Copy Configuration File")
         `((h1 "Copy Configuration File")
           (p "The configuration file "
              (blockquote (code ,configuration-path))
              "does not exist.  Would you like to copy the default configuration to this "
              "location?")
           (center (input ([type "submit"] [name "ok"] [value "Copy"]))))))
      
      ; ask-for-configuration-path : -> str
      (define (ask-for-configuration-path)
        (extract-binding/single
         'path
         (request-bindings (send/suspend configuration-path-page))))
      
      ; configuration-path-page : str -> html
      (define configuration-path-page
        (build-suspender
         '("Choose a Configuration File")
         `((h1 "Choose a Web Server Configuration File")
           ,web-server-icon
           (p "Choose a Web server configuration file to edit. "
              (br)
              "By default the Web server uses the configuration in "
              (blockquote (code ,default-configuration-path)))
           (table (tr (th "Configuration path")
                      (td (input ([type "text"] [name "path"] [size "80"]
                                  [value ,default-configuration-path]))))
                  (tr (td ([colspan "2"] [align "center"])
                          (input ([type "submit"] [name "choose-path"] [value "Select"]))))))))
      
      ; configure-top-level : str -> doesn't
      (define (configure-top-level configuration-path)
        (with-handlers ([void (lambda (exn) (send/back (exception-error-page exn)))])
          (let loop ([configuration (read-configuration configuration-path)])
            (let* ([update-bindings (interact (request-new-configuration-table configuration))]
                   [form-configuration
                    (delete-hosts (update-configuration configuration update-bindings)
                                  (extract-bindings 'deactivate update-bindings))]
                   [new-configuration
                    (cond
                      [(assq 'add-host update-bindings)
                       (add-virtual-host form-configuration (extract-bindings 'host-prefixes update-bindings))]
                      [(assq 'edit-host-details update-bindings)
                       ; write the configuration twice when editing a host: once before and once after.
                       ; The after may never happen if the user doesn't continue
                       (write-configuration form-configuration configuration-path)
                       (let ([x (assq 'edit-which-host update-bindings)])
                         (if x
                             (configure-hosts form-configuration (string->number (cdr x)))
                             (send/back must-select-host-page)))]
                      [else form-configuration])])
              (write-configuration new-configuration configuration-path)
              (loop new-configuration)))))
      
      ; add-virtual-host : configuration-table (listof str) -> configuration-table
      (define (add-virtual-host conf existing-prefixes)
        (update-hosts conf (cons (cons "my-host.my-domain.org"
                                       (configuration-table-default-host conf))
                                 (configuration-table-virtual-hosts conf))))
      
      ; update-hosts : configuration-table (listof (cons str host-table))
      (define (update-hosts conf new-hosts)
        (make-configuration-table
         (configuration-table-port conf)
         (configuration-table-max-waiting conf)
         (configuration-table-initial-connection-timeout conf)
         (configuration-table-default-host conf)
         new-hosts))
      
      ; write-to-file : str TST -> void
      (define (write-to-file file-name x)
        (call-with-output-file file-name
	  (lambda (out) (pretty-print x out))
          'truncate))
      
      ; delete-hosts : configuration-table (listof str) -> configuration-table
      ; pre: (>= (length (configuration-table-virtual-hosts conf)) (max to-delete))
      (define (delete-hosts conf to-delete)
        ; the if is not needed, it just avoids some work
        (if (null? to-delete)
            conf
            (update-hosts
             conf
             (drop (configuration-table-virtual-hosts conf) to-delete))))
      
      ; drop : (listof a) (listof str) -> (listof a)
      ; pre: (apply < to-delete)
      ; to delete the entries in to-filter indexed by to-delete
      (define (drop to-filter to-delete)
        (let loop ([to-filter to-filter] [to-delete (map string->number to-delete)] [i 0])
          (cond
            [(null? to-delete) to-filter]
            [else (if (= i (car to-delete))
                      (loop (cdr to-filter) (cdr to-delete) (add1 i))
                      (cons (car to-filter) (loop (cdr to-filter) to-delete (add1 i))))])))
      
      ; configure-hosts : configuration-table (U #f nat) -> configuration-table
      (define (configure-hosts old n)
        (if n
            (update-hosts old
                          ; more here - consider restructuring this map.  Perhaps it is fine.
                          ; Perhaps it should short circuit.  Perhaps the number of virtual hosts
                          ; is small so it doesn't matter. Perhaps that is a sloppy way to think/program.
                          ; The code is really a functional array update except it's on a list.
                          (map (lambda (host this-n)
                                 (if (= n this-n)
                                     (cons (car host) (configure-host (cdr host)))
                                     host))
                               (configuration-table-virtual-hosts old)
                               (build-list (length (configuration-table-virtual-hosts old)) (lambda (x) x))))
            (make-configuration-table
             (configuration-table-port old)
             (configuration-table-max-waiting old)
             (configuration-table-initial-connection-timeout old)
             (configure-host (configuration-table-default-host old))
             (configuration-table-virtual-hosts old))))
      
      ; configure-host : host-table -> host-table
      (define (configure-host old)
        (let* ([bindings (interact (request-new-host-table old))]
               [new (update-host-table old bindings)])
          (when (assq 'edit-passwords bindings)
            (let ([paths (host-table-paths new)])
              (configure-passwords (build-path-maybe (build-path-maybe (collection-path "web-server")
                                                                       (paths-host-base paths))
                                                     (paths-passwords paths)))))
          new))
      
      (define restart-message
        `((h3 (font ([color "red"]) "Restart the Web server to use the new settings."))
          "You may need to choose a different port or wait a while before restarting."))
      
      ; request-new-configuration-table : configuration-table -> str -> html
      (define (request-new-configuration-table old)
        (build-suspender
         '("PLT Web Server Configuration")
         `((h1 "PLT Web Server Configuration Management")
           ,web-server-icon
           "copyright 2001 by Paul Graunke and PLT"
           (hr)
           (table ([width "90%"])
                  (tr (td ,@restart-message)
                      (td ([align "right"]) (input ([type "submit"] [name "configure"] [value "Update Configuration"])))))    
           (hr)
           (h2 "Basic Configuration")
           (table
            ; more-here - make left-hand sides links to help
            ;(tr (th ([colspan "2"]) "Basic Configuration"))
            ,(make-table-row "Port" 'port (configuration-table-port old))
            ,(make-table-row "Maximum Waiting Connections"
                             'waiting (configuration-table-max-waiting old))
            ,(make-table-row "Initial Connection Timeout (seconds)" 'time-initial
                             (configuration-table-initial-connection-timeout old)))
           (hr)
           (h2 "Host Name Configuration")
           (p "The Web server accepts requests on behalf of multiple " (em "hosts")
              " each corresponding to a domain name."
              " The table below maps domain names to host specific configurations.")
           (table ([width "50%"])
                  ;(tr (th ([colspan "2"]) "Host Configuration"))
                  (tr (th ([align "left"]) "Name") ;(th "Host configuration path")
                      (th "Delete")
                      (th "Edit"))
                  (tr (td ,"Default Host")
                      (td nbsp)
                      (td ([align "center"])
                          (input ([type "radio"] [name "edit-which-host"] [value "default"]))))
                  ,@(map (lambda (host n)
                           `(tr (td ,(make-field "text" 'host-regexps (car host)))
                                (td ([align "center"])
                                    (input ([type "checkbox"] [name "deactivate"] [value ,n])))
                                (td ([align "center"])
                                    (input ([type "radio"] [name "edit-which-host"] [value ,n])))))
                         (configuration-table-virtual-hosts old)
                         (build-list (length (configuration-table-virtual-hosts old)) number->string))
                  (tr (td (input ([type "submit"] [name "add-host"] [value "Add Host"])))
                      (td nbsp); (input ([type "submit"] [name "configure"] [value "Delete"]))
                      (td (input ([type "submit"] [name "edit-host-details"] [value "Edit"])))))
           (hr)
           ,footer)))
      
      ; make-table-row : xexpr sym str [xexpr ...] -> xexpr
      (define (make-table-row label tag default-text . extra-tds)
        `(tr (td (a ([href ,(format "/~a/terms/~a.html" doc-dir tag)]) ,label))
             (td ,(make-field "text" tag (format "~a" default-text)))
             . ,extra-tds))
      
      ; make-field : str sym str -> xexpr
      (define (make-field type label value)
        `(input ([type ,type] [name ,(symbol->string label)] [value ,value] [size "30"])))
      
      ; update-configuration : configuration-table bindings -> configuration-table
      (define (update-configuration old bindings)
        (make-configuration-table
         (string->nat (extract-binding/single 'port bindings))
         (string->nat (extract-binding/single 'waiting bindings))
         (string->num (extract-binding/single 'time-initial bindings))
         (configuration-table-default-host old)
         (map (lambda (h pattern)
                (cons pattern (cdr h)))
              (configuration-table-virtual-hosts old)
              (extract-bindings 'host-regexps bindings))))
      
      ; string->num : str -> nat
      (define (string->num str)
        (let ([n (string->number str)])
          (or n
              (error 'string->nat "~s is not a number" str))))
      
      ; string->nat : str -> nat
      (define (string->nat str)
        (let ([n (string->number str)])
          (if (and n (integer? n) (exact? n) (>= n 0))
              n
              (error 'string->nat "~s is not exactly a natural number" str))))
      
      ; request-new-host-table : host-table -> str -> response
      (define (request-new-host-table old)
        (let ([timeouts (host-table-timeouts old)]
              [paths (host-table-paths old)]
              [m (host-table-messages old)])
          (build-suspender
           '("Configure Host")
           `((h1 "PLT Web Server Host configuration")
             (input ([type "submit"] [value "Save Configuration"]))
             (hr)
             (table (tr (th ([colspan "2"]) "Timeout Seconds") (th nbsp))
                    ,(make-3columns "Default Servlet" 'time-default-servlet (timeouts-default-servlet timeouts))
                    ,(make-3columns "Password" 'time-password (timeouts-password timeouts))
                    ,(make-3columns "Servlet Connection" 'time-servlet-connection (timeouts-servlet-connection timeouts))
                    ,(make-3columns "per Byte When Transfering Files" 'time-file-per-byte (timeouts-file-per-byte timeouts))
                    ,(make-3columns "Base When Transfering Files" 'time-file-base (timeouts-file-base timeouts))
                    (tr (td ([colspan "3"]) (hr)))
                    (tr (th ([colspan "2"]) "Paths") (th ([width "50%"]) nbsp))
                    ; more here - add links to descriptions, esp. what's relative to what
                    ,(make-dir-row "Host root" (collection-path "web-server")
                                   'path-host-root (paths-host-base paths))
                    ,(make-dir-row "Log file" "Host root" 'path-log (paths-log paths))
                    ,(make-dir-row "Web document root" "Host root"
                                   'path-htdocs (paths-htdocs paths))
                    ,(make-dir-row "Servlet root" "Host root"
                                   'path-servlet (paths-servlet paths))
                    ,(make-dir-row "Password File" "Host root"
                                   'path-password (paths-passwords paths))
                    (tr (td ([colspan "2"])
                            ,(make-field "submit" 'edit-passwords "Edit Passwords")))
                    (tr (td ([colspan "3"]) (hr)))
                    (tr (th ([colspan "2"]) "Message Paths") (th ([width "50%"]) nbsp))
                    ,(make-dir-row "Message root" "Host root"
                                   'path-message-root (paths-conf paths))
                    ,(make-dir-row "Servlet error" "Message root"
                                   'path-servlet-message (messages-servlet m))
                    ,(make-dir-row "Access Denied" "Message root" 'path-access-message (messages-authentication m))
                    ,(make-dir-row "Servlet cache refreshed" "Message root"
                                   'path-servlet-refresh-message (messages-servlets-refreshed m))
                    ,(make-dir-row "Password cache refreshed" "Message root"
                                   'path-password-refresh-message
                                   (messages-passwords-refreshed m))
                    ,(make-dir-row "File not found" "Message root"
                                   'path-not-found-message (messages-file-not-found m))
                    ,(make-dir-row "Protocol error" "Message root"
                                   'path-protocol-message (messages-protocol m)))
             (hr)
             (input ([type "submit"] [value "Save Configuration"]))
             ,footer))))
      
      ; make-dir-row : str str sym tst -> xexpr
      (define (make-dir-row dir parent tag default-text)
        (make-3columns `(p ,dir (br) "(relative to " ,parent ")") tag default-text))
      
      ; make-3columns : str sym tst -> xexpr
      (define (make-3columns label tag default-text)
        (make-table-row label tag default-text '(td nbsp)))
      
      ; update-host-table : host-table (listof (cons sym str)) -> host-table
      (define (update-host-table old bindings)
        (let ([eb (lambda (tag) (extract-binding/single tag bindings))])
          (make-host-table
           (host-table-indices old)
           (host-table-log-format old)
           (apply make-messages
                  (map eb '(path-servlet-message path-access-message path-servlet-refresh-message path-password-refresh-message path-not-found-message path-protocol-message)))
           (apply make-timeouts
                  (map (lambda (tag) (string->number (extract-binding/single tag bindings)))
                       '(time-default-servlet time-password time-servlet-connection time-file-per-byte time-file-base)))
           (apply make-paths
                  (map eb '(path-message-root path-host-root path-log path-htdocs path-servlet path-password))))))
      
      ; Password Configuration
      
      ; configure-passwords : str -> void
      (define (configure-passwords password-path)
        (edit-passwords
         password-path
         (if (file-exists? password-path)
             (call-with-input-file password-path read-passwords)
             null)))
      
      ; edit-passwords : str passwords -> passwords
      (define (edit-passwords which-one passwords)
        (let* ([bindings (interact (password-updates which-one passwords))]
               [to-deactivate (extract-bindings 'deactivate bindings)]
               [again
                (lambda (new-passwords)
                  (write-to-file which-one (format-passwords new-passwords))
                  (edit-passwords which-one new-passwords))])
          (cond
            [(assq 'edit bindings)
             => (lambda (edit)
                  (again (drop (map (let ([to-edit (string->number (cdr edit))])
                                      (lambda (r n)
                                        (if (= to-edit n)
                                            (edit-realm r)
                                            r)))
                                    passwords
                                    (build-list (length passwords) (lambda (x) x)))
                               to-deactivate)))]
            [(assq 'add bindings)
             (again (cons (make-realm "new realm" "" null)
                          (drop passwords to-deactivate)))]
            [else (drop passwords to-deactivate)])))
      
      ; password-updates : str passwords -> request
      (define (password-updates which-one passwords)
        (build-suspender
         `("Updating Passwords for " ,which-one)
         `((h1 "Updating Passwords for ")
           (h3 ,which-one)
           (h2 "You may wish to " (font ([color "red"]) "backup") " this password file.")
           (p "Each authentication " (em "realm") " password protects URLs that match a pattern. "
              "Choose a realm to edit below:")
           (table
            (tr (th "Realm Name") (th "Delete") (th "Edit"))
            . ,(map (lambda (realm n)
                      `(tr (td ,(realm-name realm))
                           (td ,(make-field "checkbox" 'deactivate n))
                           (td ,(make-field "radio" 'edit n))))
                    passwords
                    (build-list (length passwords) number->string)))
           ,(make-field "submit" 'submit "Add Realm")
           ,(make-field "submit" 'submit "Edit")
           ,footer)))
      
      ; edit-realm : realm -> realm
      (define (edit-realm realm)
        (let* ([bindings (interact (realm-updates realm))]
               [new-name (extract-binding/single 'realm-name bindings)]
               [new-pattern (extract-binding/single 'realm-pattern bindings)]
               [new-allowed
                (drop (map (lambda (u p) (make-user-pass (string->symbol u) p))
                           (extract-bindings 'user bindings)
                           (extract-bindings 'pass bindings))
                      (extract-bindings 'deactivate bindings))])
          ; more here - check something?  Everything is a string or symbol, though.
          (cond
            [(assq 'add-user bindings)
             (edit-realm (make-realm new-name new-pattern
                                     (cons (make-user-pass 'ptg "Scheme-is-cool!") new-allowed)))]
            [(assq 'update bindings)
             (make-realm new-name new-pattern new-allowed)]
            [else (error 'edit-realm "Didn't find either 'add-user or 'update in ~s" bindings)])))
      
      ; realm-updates : realm -> request
      (define (realm-updates realm)
        (build-suspender
         `("Update Authentication Realm " ,(realm-name realm))
         `((h1 "Update Authentication Realm")
           (table
            ,(make-table-row "Realm Name" 'realm-name (realm-name realm))
            ,(make-table-row "Protected URL Path Pattern" 'realm-pattern (realm-pattern realm)))
           (hr)
           (table 
            (tr (th "User Name") (th "Password") (th "Delete"))
            . ,(map (lambda (x n)
                      `(tr (td ,(make-field "text" 'user (symbol->string (user-pass-user x))))
                           (td ,(make-field "text" 'pass (user-pass-pass x)))
                           (td ,(make-field "checkbox" 'deactivate n))))
                    (realm-allowed realm)
                    (build-list (length (realm-allowed realm)) number->string)))
           (input ([type "submit"] [name "add-user"] [value "Add User"]))
           (input ([type "submit"] [name "update"] [value "Update Realm"]))
           ,footer)))
      
      ; read-passwords : iport -> passwords
      ; only works if the file starts with (quote ...)
      (define (read-passwords in)
        (let ([raw (read in)])
          (unless (and (pair? raw) (eq? 'quote (car raw))
                       (null? (cddr raw)))
            (error 'read-passwords "The password file must be quoted to use the configuration tool."))
          (map (lambda (raw-realm)
                 ; more here - error checking
                 (make-realm (car raw-realm)
                             (cadr raw-realm)
                             (map (lambda (x) (make-user-pass (car x) (cadr x)))
                                  (cddr raw-realm))))
               (cadr raw))))
      
      ; format-passwords : passwords -> s-expr
      (define (format-passwords passwords)
        (list 'quote
              (map (lambda (r)
                     (list* (realm-name r)
                            (realm-pattern r)
                            (map (lambda (x)
                                   (list (user-pass-user x) (user-pass-pass x)))
                                 (realm-allowed r))))
                   passwords)))
      
      ; Little Helpers
      
      ; initialization-error-page : response
      (define initialization-error-page
        `(html (head (title "Web Server Configuration Program Invocation Error"))
               (body ([bgcolor "white"])
                     (p "Please direct your browser directly to the "
                        (a ([href ,(url->string (request-uri initial-request))]) "configuration program,")
                        " not through another URL.")
                     ,footer)))
      
      ; done-page : html
      (define done-page
        ; more-here - consider adding more useful information
        `(html (head (title "done"))
               (body ([bgcolor "white"])
                     (h2 "Configuration Saved.")
                     (p "Click your browser's back button to continue configuring the server.")
                     ,footer)))
      
      ; exception-error-page : TST -> html
      (define (exception-error-page exn)
        `(html (head (title "Error"))
               (body ([bgcolor "white"])
                     (p "Servlet exception: "
                        ,(if (exn? exn)
                             (exn-message exn)
                             (format "~s" exn)))
                     ,footer)))
      
      (define must-select-host-page
        `(html (head (title "Web Server Configuration Error"))
               (body ([bgcolor "white"])
                     (p "Please select which host to edit before clicking the Edit button.")
                     ,footer)))
      
      ; io
      
      ; read-configuration : str -> configuration-table
      (define (read-configuration configuration-path)
        (parse-configuration-table (call-with-input-file configuration-path read)))
      
      ; write-configuration : configuration-table str -> void
      ; writes out the new configuration file and
      ; also copies the configure.ss servlet to the default-host's servlet directory
      (define (write-configuration new configuration-path)
        (ensure-configuration-servlet (configuration-table-default-host new))
        (let ([new-module
               `((port ,(configuration-table-port new))
                 (max-waiting ,(configuration-table-max-waiting new))
                 (initial-connection-timeout ,(configuration-table-initial-connection-timeout new))
                 (default-host-table
                  ,(format-host (configuration-table-default-host new)))
                 (virtual-host-table
                  . ,(map (lambda (h) (list (car h) (format-host (cdr h))))
                          (configuration-table-virtual-hosts new))))])
          (write-to-file configuration-path new-module)))
      
      
      
      ; ensure-configuration-servlet : host-table -> void
      (define (ensure-configuration-servlet host)
        (let* ([paths (host-table-paths host)]
               [root (build-path-maybe (collection-path "web-server")
                                       (paths-host-base paths))]
               [servlets-path
                (build-path (build-path-maybe root (paths-servlet paths)) "servlets")])
          (ensure-file (collection-path "web-server" "default-web-root" "servlets")
                       servlets-path CONFIGURE-SERVLET-NAME)
          (let ([defaults "Defaults"])
            (ensure* (collection-path "web-server" "default-web-root" "htdocs")
                     (build-path (build-path-maybe root (paths-htdocs paths)))
                     defaults))))
      
      ; ensure-file : str str str -> void
      ; to copy (build-path from name) to (build-path to name), creating directories as
      ; needed if the latter does not already exist.  
      (define (ensure-file from to name)
        (let ([to (simplify-path to)])
          (ensure-directory-shallow to)
          (let ([to-path (build-path to name)])
            (unless (file-exists? to-path)
              (copy-file (build-path from name) to-path)))))
      
      ; ensure* : str str str -> void
      (define (ensure* from to name)
        (ensure-directory-shallow to)
        (let ([p (build-path from name)])
          (cond
            [(directory-exists? p)
             (let ([dest (build-path to name)])
               (ensure-directory-shallow dest)
               (for-each (lambda (x) (ensure* p dest x))
                         (directory-list p)))]
            [(file-exists? p)
             (ensure-file from to name)])))
      
      ; ensure-directory-shallow : str -> void
      (define (ensure-directory-shallow to)
        (unless (directory-exists? to)
          ; race condition - someone else could make the directory
          (make-directory* to)))
      
      ; format-host : host-table
      (define (format-host host)
        (let ([t (host-table-timeouts host)]
              [p (host-table-paths host)]
              [m (host-table-messages host)])
          `(host-table
            ; more here - configure
            (default-indices "index.html" "index.htm")
            ; more here - configure
            (log-format parenthesized-default)
            (messages
             (servlet-message ,(messages-servlet m))
             (authentication-message ,(messages-authentication m))
             (servlets-refreshed ,(messages-servlets-refreshed m))
             (passwords-refreshed ,(messages-passwords-refreshed m))
             (file-not-found-message ,(messages-file-not-found m))
             (protocol-message ,(messages-protocol m)))
            (timeouts
             (default-servlet-timeout ,(timeouts-default-servlet t))
             (password-connection-timeout ,(timeouts-password t))
             (servlet-connection-timeout ,(timeouts-servlet-connection t))
             (file-per-byte-connection-timeout ,(timeouts-file-per-byte t))
             (file-base-connection-timeout ,(timeouts-file-base t)))
            (paths
             (configuration-root ,(paths-conf p))
             (host-root ,(paths-host-base p))
             (log-file-path ,(paths-log p))
             (file-root ,(paths-htdocs p))
             (servlet-root ,(paths-servlet p))
             (password-authentication ,(paths-passwords p))))))
      
      ; extract-definition : sym (listof s-expr) -> s-expr
      ; to return the rhs from (def name rhs) not (def (name . args) body)
      (define (extract-definition name defs)
        (or (ormap (lambda (def)
                     (and (pair? def) (eq? 'define (car def))
                          (pair? (cdr def)) (eq? name (cadr def))
                          (pair? (cddr def))
                          (caddr def)))
                   defs)
            (error 'extract-definition "definition for ~a not found" name)))
      
      ; passwords = str (i.e. path to a file)
      
      (define build-path-maybe-expression->file-name caddr)
      
      ; main
      (choose-configuration-file))))

;; open it up
(require configure)
servlet
