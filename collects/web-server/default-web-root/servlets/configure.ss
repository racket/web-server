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
           (rename (lib "host-configuration-table-language.ss" "web-server") build-path-maybe build-path-maybe))
  
  (define servlet
    (unit/sig ()
      (import servlet^)
      
      (adjust-timeout! (* 12 60 60))
      (error-print-width 800) ; 10-ish lines
      
      ; top-configuration : (make-top-configuration nat nat nat host-configuration (listof (cons str host-configuration)))
      (define-struct top-configuration (port waiting initial-timeout default-host hosts))
      
      ; host-configuration = (make-host-configuration timeouts paths)
      (define-struct host-configuration (timeouts paths))
      
      ; timeouts = (make-timeouts nat^5)
      (define-struct timeouts (default-servlet password servlet-connection file-per-byte file-base))
      
      ; paths = (make-paths str^12)
      (define-struct paths (host-root log htdocs servlet passwords message-root servlet-message access-message servlet-refresh-message password-refresh-message not-found-message protocol-message))
      
      ; passwords = (listof realm)
      ; realm = (make-realm str str (listof user-pass))
      (define-struct realm (name pattern allowed))
      
      ; user-pass = (make-user-pass sym str)
      (define-struct user-pass (user pass))
      
      ; build-footer : str -> html
      (define (build-footer base)
        (let ([scale (lambda (n) (number->string (round (/ n 4))))])
          `(p "Powered by "
              (a ([href "http://www.plt-scheme.org/"])
                 (img ([width ,(scale 211)] [height ,(scale 76)]
                       [src ,(string-append base "documentation/plt-logo.gif")]))))))
      
      (define footer (build-footer "/"))
      
      ; access-error-page : html
      (define access-error-page
        `(html (head (title "Web Server Configuration Access Error"))
               (body ([bgcolor "white"])
                     (p "You must connect to the configuration tool from the machine the server runs on.")
                     ,footer)))
      
      ; check-ip-address : request -> request
      (define (check-ip-address request)
        (unless (string=? "127.0.0.1" (request-host-ip request))
          (send/finish access-error-page))
        request)
      
      (check-ip-address initial-request)
      
      ; more here - abstract with static pages?
      (define web-server-icon
        `(img ([src "/documentation/web-server.gif"]
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
      
      ; configure-top-level : -> responose
      (define (configure-top-level)
        (with-handlers ([void (lambda (exn) (send/back (exception-error-page exn)))])
          (let loop ([configuration (read-configuration)])
            (let* ([update-bindings (interact (request-new-top-configuration configuration))]
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
                       (write-configuration form-configuration)
                       (let ([x (assq 'edit-which-host update-bindings)])
                         (if x
                             (configure-hosts form-configuration (string->number (cdr x)))
                             (send/back must-select-host-page)))]
                      [else form-configuration])])
              (write-configuration new-configuration)
              (loop new-configuration)))))
      
      ; add-virtual-host : top-configuration (listof str) -> top-configuration
      (define (add-virtual-host conf existing-prefixes)
        (update-hosts conf (cons (cons "my-host.my-domain.org"
                                       (top-configuration-default-host conf))
                                 (top-configuration-hosts conf))))
      
      ; update-hosts : top-configuration (listof (cons str host-configuration))
      (define (update-hosts conf new-hosts)
        (make-top-configuration
         (top-configuration-port conf)
         (top-configuration-waiting conf)
         (top-configuration-initial-timeout conf)
         (top-configuration-default-host conf)
         new-hosts))
      
      ; write-to-file : str TST -> void
      (define (write-to-file file-name x)
        (call-with-output-file file-name
	  (lambda (out) (pretty-print x out))
          'truncate))
      
      ; delete-hosts : top-configuration (listof str) -> top-configuration
      ; pre: (>= (length (top-configuration-hosts conf)) (max to-delete))
      (define (delete-hosts conf to-delete)
        ; the if is not needed, it just avoids some work
        (if (null? to-delete)
            conf
            (update-hosts
             conf
             (drop (top-configuration-hosts conf) to-delete))))
      
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
      
      ; configure-hosts : top-configuration (U #f nat) -> top-configuration
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
                               (top-configuration-hosts old)
                               (build-list (length (top-configuration-hosts old)) (lambda (x) x))))
            (make-top-configuration
             (top-configuration-port old)
             (top-configuration-waiting old)
             (top-configuration-initial-timeout old)
             (configure-host (top-configuration-default-host old))
             (top-configuration-hosts old))))
      
      ; configure-host : host-configuration -> host-configuration
      (define (configure-host old)
        (let* ([bindings (interact (request-new-host-configuration old))]
               [new (update-host-configuration old bindings)])
          (when (assq 'edit-passwords bindings)
            (let ([paths (host-configuration-paths new)])
              (configure-passwords (build-path-maybe (build-path-maybe (collection-path "web-server")
                                                                       (paths-host-root paths))
                                                     (paths-passwords paths)))))
          new))
      
      ; request-new-top-configuration : top-configuration -> str -> html
      (define (request-new-top-configuration old)
        (build-suspender
         '("PLT Web Server Configuration")
         `((h1 "PLT Web Server Configuration Management")
           ,web-server-icon
           "copyright 2001 by Paul Graunke and the PLT" (br)
           (hr)
           (h2 "Basic Configuration")
           (table
            ; more-here - make left-hand sides links to help
            ;(tr (th ([colspan "2"]) "Basic Configuration"))
            ,(make-table-row "Port" 'port (top-configuration-port old))
            ,(make-table-row "Maximum Waiting Connections"
                             'waiting (top-configuration-waiting old))
            ,(make-table-row "Initial Connection Timeout (seconds)" 'time-initial
                             (top-configuration-initial-timeout old)))
           (hr)
           (h2 "Host Name Configuration")
           (p "The Web server accepts requests on behalf of multiple " (em "hosts")
              "each corresponding to a domain name such as " (code "www.plt-scheme.org")
              " The table below maps domain names to host specific configurations.")
           (table 
            ;(tr (th ([colspan "2"]) "Host Configuration"))
            (tr (th "Name pattern") ;(th "Host configuration path")
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
                   (top-configuration-hosts old)
                   (build-list (length (top-configuration-hosts old)) number->string))
            (tr (td (input ([type "submit"] [name "add-host"] [value "Add Host"])))
                (td nbsp); (input ([type "submit"] [name "configure"] [value "Delete"]))
                (td (input ([type "submit"] [name "edit-host-details"] [value "Edit"])))))
           (input ([type "submit"] [name "configure"] [value "Update Configuration"]))
           ,footer)))
      
      ; make-table-row : xexpr sym str [xexpr ...] -> xexpr
      (define (make-table-row label tag default-text . extra-tds)
        `(tr (td (a ([href ,(format "/documentation/terms/~a.html" tag)]) ,label))
             (td ,(make-field "text" tag (format "~a" default-text)))
             . ,extra-tds))
      
      ; make-field : str sym str -> xexpr
      (define (make-field type label value)
        `(input ([type ,type] [name ,(symbol->string label)] [value ,value] [size "30"])))
      
      ; update-configuration : top-configuration bindings -> top-configuration
      (define (update-configuration old bindings)
        (make-top-configuration
         (string->nat (extract-binding/single 'port bindings))
         (string->nat (extract-binding/single 'waiting bindings))
         (string->nat (extract-binding/single 'time-initial bindings))
         (top-configuration-default-host old)
         (map (lambda (h pattern)
                (cons pattern (cdr h)))
              (top-configuration-hosts old)
              (extract-bindings 'host-regexps bindings))))
      
      ; string->nat : str -> nat
      (define (string->nat str)
        (let ([n (string->number str)])
          (if (and n (integer? n) (exact? n) (>= n 0))
              n
              (error 'string->nat "~s is not exactly a natural number" str))))
      
      ; request-new-host-configuration : host-configuration -> str -> response
      (define (request-new-host-configuration old)
        (let ([timeouts (host-configuration-timeouts old)]
              [paths (host-configuration-paths old)])
          (build-suspender
           '("Configure Host")
           `((h1 "PLT Web Server Host configuration")
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
                                   'path-host-root (paths-host-root paths))
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
                                   'path-message-root (paths-message-root paths))
                    ,(make-dir-row "Servlet error" "Message root"
                                   'path-servlet-message (paths-servlet-message paths))
                    ,(make-dir-row "Access Denied" "Message root" 'path-access-message (paths-access-message paths))
                    ,(make-dir-row "Servlet cache refreshed" "Message root"
                                   'path-servlet-refresh-message (paths-servlet-refresh-message paths))
                    ,(make-dir-row "Password cache refreshed" "Message root"
                                   'path-password-refresh-message
                                   (paths-password-refresh-message paths))
                    ,(make-dir-row "File not found" "Message root"
                                   'path-not-found-message (paths-not-found-message paths))
                    ,(make-dir-row "Protocol error" "Message root"
                                   'path-protocol-message (paths-protocol-message paths)))
             (input ([type "submit"] [value "Save Configuration"]))
             ,footer))))
      
      ; make-dir-row : str str sym tst -> xexpr
      (define (make-dir-row dir parent tag default-text)
        (make-3columns `(p ,dir (br) "(relative to " ,parent ")") tag default-text))
      
      ; make-3columns : str sym tst -> xexpr
      (define (make-3columns label tag default-text)
        (make-table-row label tag default-text '(td nbsp)))
      
      ; update-host-configuration : host-configuration (listof (cons sym str)) -> host-configuration
      (define (update-host-configuration old bindings)
        (make-host-configuration
         (apply make-timeouts
                (map (lambda (tag) (string->number (extract-binding/single tag bindings)))
                     '(time-default-servlet time-password time-servlet-connection time-file-per-byte time-file-base)))
         (apply make-paths
                (map (lambda (tag) (extract-binding/single tag bindings))
                     '(path-host-root path-log path-htdocs path-servlet path-password path-message-root path-servlet-message path-access-message path-servlet-refresh-message path-password-refresh-message path-not-found-message path-protocol-message)))))
      
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
           (p "Each authentication " (em "realm") " password protects URLs that match a pattern."
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
      
      ; configuration-path : str
      (define configuration-path (build-path (collection-path "web-server") "configuration-table.ss"))
      
      ; read-configuration : -> top-configuration
      ; This assumes that the module is well-formed once it sees the module is written in the right language.
      (define (read-configuration)
        (let ([configuration (call-with-input-file configuration-path read)])
          (unless (and (pair? configuration) (eq? 'module (car configuration))
                       (pair? (cdr configuration)) (pair? (cddr configuration))
                       (equal? configuration-table-language (caddr configuration)))
            (error 'read-configuration
                   "The top level configuration file is not a module in the configuration-table-language"))
          (let ([body (cdddr configuration)])
            (make-top-configuration (extract-definition 'port body)
                                    (extract-definition 'max-waiting body)
                                    (extract-definition 'initial-connection-timeout body)
                                    (parse-host-configuration (extract-definition 'default-host-table body))
                                    (map (lambda (x)
                                           (unless (and (pair? x) (eq? 'cons (car x))
                                                        (pair? (cdr x)) (string? (cadr x)))
                                             (error 'read-configuration "expected `(cons ,str ,host-configuration-syntax) received ~s" x))
                                           (cons (cadr x)
                                                 (parse-host-configuration (caddr x))))
                                         ; skip the symbol 'list
                                         (cdr (extract-definition 'virtual-host-table body)))))))
      
      ; parse-host-configuartion : s-expr -> host-configuration
      ; more here - better error checking
      (define (parse-host-configuration x)
        (unless (and (pair? x) (eq? (car x) 'let*) (pair? (cdr x)) (list? (cadr x)) (pair? (cddr x)))
          (error 'parse-host-configuration "malformed host table: expected a let*, given ~s" x))
        (let* ([bindings (cadr x)]
               [gen-check-bindings
                (lambda (binding-ok? expected)
                  (lambda (name)
                    (let ([binding (assq name bindings)])
                      (unless binding
                        (error 'parse-host-configuration
                               "missing binding for ~a in ~s" name bindings))
                      (let ([v (cadr binding)])
                        (unless (binding-ok? v)
                          (error 'parse-host-configuration
                                 "expected ~a for ~a, received ~s"
                                 expected
                                 name v))
                        v))))])
          (make-host-configuration
           (apply make-timeouts
                  (map (gen-check-bindings number? "a literal number")
                       '(default-servlet-timeout
                         password-connection-timeout
                         servlet-connection-timeout
                         file-per-byte-connection-timeout
                         file-base-connection-timeout)))
           (apply make-paths (map (compose caddr
                                           (gen-check-bindings build-path-maybe? "a build-path-maybe expression"))
                                  '(host-root
                                    log-file-path
                                    file-root
                                    servlet-root
                                    password-authentication
                                    configuration-root
                                    servlet-message
                                    authentication-message
                                    servlets-refreshed
                                    passwords-refreshed
                                    file-not-found-message
                                    protocol-message))))))
      
      ; build-path-maybe? : tst -> bool
      (define (build-path-maybe? x)
        (and (pair? x) (eq? (car x) 'build-path-maybe)
             (pair? (cdr x)) (symbol? (cadr x))
             (pair? (cddr x)) (string? (caddr x))
             (null? (cdddr x))))
      
      ; write-configuration : top-configuration -> void
      (define (write-configuration new)
        (let ([new-module
               `(module configuration-table
                  ,configuration-table-language
                  (provide
                   port
                   max-waiting
                   initial-connection-timeout
                   virtual-host-table
                   default-host-table)
                  
                  (define port ,(top-configuration-port new))
                  (define max-waiting ,(top-configuration-waiting new))
                  (define initial-connection-timeout ,(top-configuration-initial-timeout new))
                  
                  (define default-host-table
                    ,(format-host (top-configuration-default-host new)))
                  
                  (define virtual-host-table
                    ,(cons 'list (map (lambda (h)
                                        `(cons ,(car h) ,(format-host (cdr h))))
                                      (top-configuration-hosts new)))))])
          (write-to-file configuration-path new-module)))
      
      ; format-host : host-configuration
      (define (format-host host)
        (let ([t (host-configuration-timeouts host)]
              [p (host-configuration-paths host)])
          `(let* ([host-root (build-path-maybe web-server-collection ,(paths-host-root p))]
                  [log-file-path (build-path-maybe host-root ,(paths-log p))]
                  [file-root (build-path-maybe host-root ,(paths-htdocs p))]
                  [servlet-root (build-path-maybe host-root ,(paths-servlet p))]
                  [configuration-root (build-path-maybe host-root ,(paths-message-root p))]
                  [default-indices (list "index.html" "index.htm")] ; more here - configure
                  [log-format 'parenthesized-default] ; more here - configure
                  [password-authentication (build-path-maybe host-root ,(paths-passwords p))]
                  [servlet-message (build-path-maybe configuration-root ,(paths-servlet-message p))]
                  ;[servlet-loading-responder ...]
                  [authentication-message (build-path-maybe configuration-root ,(paths-access-message p))]
                  [servlets-refreshed (build-path-maybe configuration-root
                                                        ,(paths-servlet-refresh-message p))]
                  [passwords-refreshed (build-path-maybe configuration-root
                                                         ,(paths-password-refresh-message p))]
                  [file-not-found-message (build-path-maybe configuration-root
                                                            ,(paths-not-found-message p))]
                  [protocol-message (build-path-maybe configuration-root ,(paths-protocol-message p))]
                  [default-servlet-timeout ,(timeouts-default-servlet t)]
                  [password-connection-timeout ,(timeouts-password t)]
                  [servlet-connection-timeout ,(timeouts-servlet-connection t)]
                  [file-per-byte-connection-timeout ,(timeouts-file-per-byte t)]
                  [file-base-connection-timeout ,(timeouts-file-base t)])
             (make-host-table
              default-indices
              servlet-root
              log-format
              password-authentication
              (make-messages
               servlet-message
               ; servlet-loading-responder
               authentication-message
               servlets-refreshed
               passwords-refreshed
               file-not-found-message
               protocol-message)
              (make-timeouts
               default-servlet-timeout
               password-connection-timeout
               servlet-connection-timeout
               file-per-byte-connection-timeout
               file-base-connection-timeout)
              (make-paths
               host-root
               log-file-path
               file-root
               servlet-root)))))
      
      (define configuration-table-language "configuration-table-language.ss")
      
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
      (configure-top-level))))

;; open it up
(require configure)
servlet
