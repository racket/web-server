(module web-server-unit mzscheme
  (provide web-server@)
  (require (lib "unitsig.ss")
           (lib "url.ss" "net")
           (lib "xml.ss" "xml")
           (lib "string.ss")
           (lib "list.ss")
           (lib "etc.ss")
           (lib "base64.ss" "net")
           (lib "tcp-sig.ss" "net")
           "channel.ss"
           "util.ss"
           "servlet-sig.ss"
           "timer.ss"
           "configuration-structures.ss")
  
  ; Method = (U 'get 'post 'head 'put 'delete 'trace)
  
  (define DEFAULT-HOST-NAME "<none>")
  
  (define web-server@
    (unit/sig web-server^
      (import net:tcp^)
      
      ; -------------------------------------------------------------------------------
      ; The Server
      
      ; serve : configuration Nat [str | #f] -> -> Void
      ; to start the server on the given port and return an un-server to shut it down
      ; the optional port argument overrides the configuration's port
      ; the optional host argument only accepts connections from that host ip address
      ; (a host of #f places no restrictions on the connecting host)
      (define serve
        ; use default values from configuration.ss by default
        (opt-lambda (configuration
                     [port (configuration-port configuration)]
                     [only-from-host #f])
          (let ([virtual-hosts (configuration-virtual-hosts configuration)]
                [max-waiting (configuration-max-waiting configuration)]
                [custodian (make-custodian)])
            (parameterize ([current-custodian custodian])
              (let ([get-ports
                     ; If tcp-listen fails, the exception will be raised in the caller's thread.
                     (let ([listener (tcp-listen port max-waiting #t only-from-host)])
                           (lambda () (tcp-accept listener)))])
                (thread
                 (lambda ()
                   (server-loop custodian get-ports
                                (make-config virtual-hosts (make-hash-table)
                                             (make-hash-table) (make-hash-table))
                                (configuration-initial-connection-timeout configuration)
                                ; more here - log the connection close? optionally?
                                void)))))
            (lambda () (custodian-shutdown-all custodian)))))
      
      ; -------------------------------------------------------------------------------
      ; The Server Loop
      
      (define METHOD:REGEXP
        (regexp "^(GET|HEAD|POST|PUT|DELETE|TRACE) (.+) HTTP/([0-9]+)\\.([0-9]+)$"))
      
      (define (match-method x)
        (regexp-match METHOD:REGEXP x))
      ;:(define match-method (type: (str -> (union false (list str str str str str)))))
      
      ; server-loop : custodian (-> iport oport) config num (-> void) -> void
      ; note - connection-lost is used by the development environment
      (define (server-loop top-custodian listener tables init-timeout connection-lost)
        (let bigger-loop ()
          (with-handlers ([exn:i/o:tcp? (lambda (exn) (bigger-loop))]
                          [void (lambda (exn)
                                  (fprintf (current-error-port) "server-loop exn: ~a" exn)
                                  (bigger-loop))])
            (let loop ()
              (let ([connection-cust (make-custodian)])
                (parameterize ([current-custodian connection-cust])
                  (let*-values ([(ip op) (listener)]
                                [(shutdown) (lambda ()
                                              (close-input-port ip) ; DEBUG - pipes
                                              (close-output-port op) ; DEBUG - pipes
                                              (custodian-shutdown-all connection-cust))])
                    (thread (lambda ()
                              (with-handlers ([exn:i/o:port:closed?
                                               (lambda (exn)
                                                 (connection-lost)
                                                 (shutdown))]
                                              [void (lambda (exn) (shutdown))])
                                (serve-connection top-custodian ip op tables
                                                  (start-timer init-timeout shutdown)
                                                  init-timeout)
                                (shutdown)))))))
              (loop)))))
      
      ; serve-connection : custodian iport oport Tables timer num -> Void
      ; to respond to all the requests on an http connection 
      ; (Currently only the first request is answered.)
      (define (serve-connection top-custodian ip op tables timer init-timeout)
        (let connection-loop ()
          (let-values ([(method uri-string major-version minor-version) (read-request ip op)])
            (let* ([headers (read-headers ip)]
                   [uri     (string->url uri-string)]
                   [host    (get-host uri headers)]
                   [host-conf ((config-hosts tables) host)])
              ; more here - don't extract host-ip and client-ip twice (leakage)
              (let-values ([(host-ip client-ip) (tcp-addresses ip)])
                ((host-log-message host-conf) host-ip client-ip method uri host)
                (let ([close (close-connection? headers
                                                (string->number major-version)
                                                (string->number minor-version)
                                                client-ip host-ip)])
                  (dispatch top-custodian method host-conf uri headers ip op tables timer close)
                  (reset-timer timer init-timeout)
                  (unless close (connection-loop))))))))
      
      ; close-connection? : table nat nat str str -> bool
      (define (close-connection? headers major minor client-ip host-ip)
        (or (< major 1)
            (and (= major 1) (= minor 0))
            (cond
              [(assq 'connection headers)
               => (lambda (x) (string-ci=? "close" (cdr x)))]
              [else #f])
            (msie-from-local-machine? headers client-ip host-ip)))
      
      ; : table str str -> bool
      ; to work around a bug in MSIE for documents < 265 bytes when connecting from the local
      ; machine.  The server could pad the response as MSIIS does, but closing the connection works, too.
      ; We do not check for version numbers since IE 6 under windows is 5.2 under macosX
      (define (msie-from-local-machine? headers client-ip host-ip)
        (and (string=? host-ip client-ip)
             (cond
               [(or (assq 'HTTP_USER_AGENT headers)
                    (assq 'user-agent headers))
                => (lambda (client) (regexp-match MSIE-regexp (cdr client)))]
               [else #f])))
      (define MSIE-regexp (regexp "MSIE"))
      
      ; read-request : iport oport -> Symbol String String String
      ; to read in the first line of an http request,
      ; effect: in case of errors, complain [MF: where] and close the ports
      (define (read-request ip op)
        (let ([line (read-line ip 'any)])
          (if (eof-object? line)
              (error 'read-request "http input closed abruptly")
              (cond
                [(match-method line)
                 => (lambda (x)
                      (apply values (cons (lowercase-symbol! (cadr x)) (cddr x))))]
                [else (error 'read-request "malformed request ~a" line)]))))
      
      ; get-host : Url (listof (cons Symbol String)) -> String
      ; host names are case insesitive---Internet RFC 1034
      (define (get-host uri headers)
        (let ([s (or (url-host uri) (cdr (or (assq 'host headers) (cons 'dummy DEFAULT-HOST-NAME))))])
          (string-lowercase! s)
          s))
      
      (define COLON:REGEXP (regexp (format "^([^:]*):[ ~a]*(.*)" #\tab)))
      
      (define (match-colon s)
        (regexp-match COLON:REGEXP s))
      ;:(define match-colon (type: (str -> (union false (list str str str)))))
      
      ; read-headers : iport -> (listof (cons Symbol String))
      (define (read-headers in)
        (let read-header ()
          (let ([l (read-line in 'any)])
            (cond
              [(eof-object? l) null]
              [(zero? (string-length l)) null]
              [(match-colon l) =>
               (lambda (match)
                 ; (cadr match) exists because COLON:REGEXP contains two (.)
                 ; (caddr match) exists because COLON:REGEXP contains two (.)
                 (cons (cons (lowercase-symbol! (cadr match))
                             (read-one-head in (caddr match)))
                       (read-header)))]
              [else (error 'read-headers "malformed header")]))))
      
      ; read-one-head : iport str -> str
      (define (read-one-head in rhs)
        (let ([c (peek-char in)])
          (cond
            [(or (eq? c #\space) (eq? c #\tab))
             ; (read-line in 'any) can't return eof
             ; because we just checked with peek-char 
             ; Spidey: FLOW
             (read-one-head in (string-append rhs (read-line in 'any)))]
            [else rhs])))
      
      ; read-mime-multipart : str iport -> (listof part)
      (define (read-mime-multipart boundary in)
        (let* ([boundary-len (string-length boundary)]
               [start-boundary (string-append "--" boundary)]
               [end-boundary (string-append start-boundary "--")])
          (let skip-preamble ()
            (let ([line (read-line in 'return-linefeed)])
              (cond
                [(string=? line start-boundary)
                 (let read-parts ()
                   (let ([headers (read-headers in)])
                     (let read-mime-part-body ([more-k (lambda (contents)
                                                         (cons (construct-mime-part
                                                                headers contents)
                                                               (read-parts)))]
                                               [end-k (lambda (contents)
                                                        (list (construct-mime-part
                                                               headers contents)))])
                       (let ([line (read-line in 'return-linefeed)])
                         (cond
                           [(string=? line start-boundary)
                            (more-k null)]
                           [(string=? line end-boundary)
                            (end-k null)]
                           [else (read-mime-part-body
                                  (lambda (x) (more-k (cons line x)))
                                  (lambda (x) (end-k (cons line x))))])))))]
                [(string=? line end-boundary) null]
                [else (skip-preamble)])))))
      
      ; more here - use structure, perhaps
      ; construct-mime-part : (listof header) (listof str) -> part
      (define (construct-mime-part headers body)
        (cons headers
              (cond
                [(null? body) null]
                [else (cons (car body)
                            (foldr (lambda (str acc)
                                     (list* CR-NL str acc))
                                   null
                                   (cdr body)))])))
      
      (define CR-NL (format "~a~a" #\return #\newline))
      
      
      ; ----------------------------------------------------------------------------
      
      (empty-tag-shorthand html-empty-tags)
      
      ; --------------------------------------------------------------------------
      ; CONSTANTS AND COMPUTED CONSTANTS
      
      (define TEXT/HTML-MIME-TYPE "text/html")
      (define TIMEOUT-DEFAULT (* 2 60)) ; in seconds
      
      (define INPUT-BUFFER-SIZE 4096)
      
      ; copy-port : iport oport -> Void
      (define buffer-size 4096)
      (define (copy-port from to)
        (with-handlers ([void void])
          ; display can raise an error if the tcp port is closed by the client
          (let* ([buffer (make-string buffer-size)])
            (let loop ()
              (let ([l (read-string-avail! buffer from)])
                (unless (eof-object? l)
                  (display (if (< l buffer-size) 
                               (substring buffer 0 l)
                               buffer)
                           to)
                  (loop)))))))
      
      (define MONTHS 
        #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
      
      (define DAYS
        #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
      
      ; --------------------------------------------------------------------------
      ; DISPATCH
      
      ; config = (make-config host-table script-table instance-table access-table)
      (define-struct config (hosts scripts instances access))
      
      ; host-table = str -> host
      ; access-table = (hashtable-of base:sym -> (path:str name:sym password:str -> (+ false str)))
      ; instance-table = (hashtable-of sym Cgi-instance)
      ; script-table : (hashtable-of sym script)
      ; script = (unit servlet^ -> response)
      
      ; dispatch : custodian Method Host URL x-table iport oport Configuration timer bool -> Void
      ; to respond to an HTTP request
      (define (dispatch top-custodian method host-info uri headers in out config timer close)
        (let ([path (url-path uri)])
          (cond
            [(access-denied? method uri headers host-info (config-access config)) =>
             (lambda (realm)
               (reset-timer timer (timeouts-password (host-timeouts host-info)))
               (request-authentication method uri in out host-info realm close))] 
            [(conf-prefix? path)
             (cond
               [(string=? "/conf/refresh-servlets" path)
                ; more here - this is broken - only out of date or specifically mentioned
                ; scripts should be flushed.  This destroys persistent state!
                (set-config-scripts! config (make-hash-table))
                (report-error out method ((responders-servlets-refreshed (host-responders host-info))) close)]
               [(string=? "/conf/refresh-passwords" path)
                ;(set-config-access! config (make-hash-table))
                ; more here - send a nice error page
                (hash-table-put! (config-access config) host-info (read-passwords host-info))
                (report-error out method ((responders-passwords-refreshed (host-responders host-info))) close)]
               [else (report-error out method (responders-file-not-found uri) close)])]
            [(servlet-bin? path)
             (reset-timer timer (timeouts-servlet-connection (host-timeouts host-info)))
             ; more here - make timeout proportional to size of bindings
             (servlet-content-producer top-custodian method uri headers in out host-info (config-scripts config) (config-instances config) close)]
            [else (file-content-producer method uri headers in out host-info timer close)])))
      
      ; --------------------------------------------------------------------------
      ; ACCESS CONTROL
      
      ; pass-entry = (make-pass-entry str regexp (list sym str))
      (define-struct pass-entry (domain pattern users))
      
      (define AUTHENTICATION-REGEXP (regexp "([^:]*):(.*)"))
      (define (match-authentication x) (regexp-match AUTHENTICATION-REGEXP x))
      ;:(define match-authentication (type: (str -> (union false (list str str str)))))
      
      ; access-denied? : Method URL x-table host Access-table -> (+ false str)
      ; the return string is the prompt for authentication
      (define (access-denied? method uri headers host-info access-table)
        (let ([denied?
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
                  ; str sym str -> (+ false str)
                  (lambda (request-path user-name password)
                    (ormap (lambda (x)
                             (and (regexp-match (pass-entry-pattern x) request-path)
                                  (let ([name-pass (assq user-name (pass-entry-users x))])
                                    (if (and name-pass (string=? (cadr name-pass) password))
                                        #f
                                        (pass-entry-domain x)))))
                           passwords)))
                (lambda (req user pass) #f)))))
      
      ; extract-user-pass : (listof (cons sym str)) -> (U #f (cons str str))
      (define (extract-user-pass headers)
        (let ([pass-pair (assq 'authorization headers)])
          (and pass-pair
               (let ([basic-credentials (cdr pass-pair)])
                 (cond
                   [(and (basic? basic-credentials)
                         (match-authentication (base64-decode (substring basic-credentials 6 (string-length basic-credentials)))))
                    => (lambda (user-pass)
                         (cons (cadr user-pass) (caddr user-pass)))]
                   [else #f])))))
      
      (define fake-user (gensym))
      
      ; lowercase-symbol! : String -> Symbol
      (define (lowercase-symbol! s)
        (string-lowercase! s)
        (string->symbol s))
      
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
      
      ; request-authentication : Method URL iport oport host str bool -> bool
      (define (request-authentication method uri in out host-info realm close)
        (report-error out method
                      ((responders-authentication (host-responders host-info))
                       uri `(WWW-Authenticate . ,(string-append " Basic realm=\"" realm "\"")))
                      close))
      
      ; --------------------------------------------------------------------------
      ; SERVING FILES 
      
      ; file-content-producer : Method URL x-table iport oport host timer bool -> Void
      (define (file-content-producer method uri headers in out host-info timer close)
        (serve-file method uri out host-info timer close))
      
      ; looks-like-directory : str -> bool
      ; to determine if is url style path looks like it refers to a directory
      (define (looks-like-directory? path)
        (eq? #\/ (string-ref path (sub1 (string-length path)))))
      
      ; serve-file : Method url oport host timer bool -> void
      ; to find the file, including searching for implicit index files, and serve it otu
      (define (serve-file method uri out host-info timer close)
        (let ([path (url-path->path (paths-htdocs (host-paths host-info)) (url-path uri))])
          (cond
            [(file-exists? path) (output-file method path out timer host-info close)]
            [(directory-exists? path)
             (let loop ([dir-defaults (host-indices host-info)])
               (cond 
                 [(pair? dir-defaults) 
                  (let ([full-name (build-path path (car dir-defaults))])
                    (if (file-exists? full-name)
                        (cond
                          [(looks-like-directory? (url-path uri))
                           (output-file method full-name out timer host-info close)]
                          [else
                           ; more here - look into serving the file _and_ providing a Location header or
                           ; was it a content-location header?
                           (output-headers out 301 "Moved Permanently" (current-seconds) TEXT/HTML-MIME-TYPE `(("Location: " ,(url-path uri) "/")) close)
                           (when (eq? method 'get)
                             (write-xml/content
                              (xexpr->xml `(html (head (title "Add a Slash"))
                                                 (body "Please use " (a ([href ,(string-append (url-path uri) "/")]) "this url") " instead.")))
                              out))])
                        (loop (cdr dir-defaults))))]
                 [else (report-error out method ((responders-file-not-found (host-responders host-info)) uri)
                                     close)]))]
            [else (report-error out method ((responders-file-not-found (host-responders host-info)) uri)
                                close)])))
      
      ; output-file : Method str oport timer host bool -> void
      ; to serve out the file
      (define (output-file method path out timer host-info close)
        (let ([size (file-size path)]
              [timeouts (host-timeouts host-info)])
          (reset-timer timer (+ (timeouts-file-base timeouts) (* size (timeouts-file-per-byte timeouts))))
          (output-headers out 200 "Okay" 
                          (file-or-directory-modify-seconds path)
                          (get-mime-type path)
                          `(("Content-length: " ,size))
                          close))
        (when (eq? method 'get)
          (call-with-input-file path (lambda (in) (copy-port in out)))))
      
      ; get-mime-type : String -> String
      ; to find a mime type based on the filename's suffix
      (define (get-mime-type path)
        (let loop ([n (sub1 (string-length path))])
          (cond
            [(< n 0) DEFAULT-MIME-TYPE]
            [(char=? (string-ref path n) #\.) 
             (hash-table-get MIME-TYPE-TABLE
                             (string->symbol (substring path (+ n 1) (string-length path)))
                             (lambda () DEFAULT-MIME-TYPE))]
            [(char=? (string-ref path n) #\/) DEFAULT-MIME-TYPE]
            [else (loop (sub1 n))])))
      
      (define DEFAULT-MIME-TYPE "text/plain")
      
      (define MIME-TYPE-TABLE
        (let ([table (make-hash-table)])
          (for-each (lambda (x) (hash-table-put! table (car x) (cdr x)))
                    '((htm  . "text/html")
                      (html . "text/html")
                      (css  . "text/css")
                      (txt  . "text/plain")
                      (hqx  . "application/mac-binhex40")
                      (doc  . "application/msword")
                      (plt  . "application/octet-stream")
                      (w02  . "application/octet-stream")
                      (w03  . "application/octet-stream")
                      (exe  . "application/octet-stream")
                      (bin  . "application/octet-stream")
                      (pdf  . "application/pdf")
                      (ps   . "application/postscript")
                      (rtf  . "application/rtf")
                      (dvi  . "application/x-dvi")
                      (tar  . "application/x-tar")
                      (tex  . "application/x-tex")
                      (zip  . "application/zip")
		      (xls  . "application/msexcel")
		      (ppt  . "application/powerpoint")
		      (pot  . "application/powerpoint")
		      (ppf  . "application/persuasion")
		      (fm   . "application/filemaker")
		      (pm6  . "application/pagemaker")
		      (psd  . "application/x-photoshop")
		      (pdd  . "application/x-photoshop")
                      (ram  . "audio/x-pn-realaudio")
                      (ra   . "audio/x-realaudio")
                      (swf  . "application/x-shockwave-flash")
		      (aif  . "audio/aiff")
		      (au   . "audio/basic")
		      (voc  . "audio/voice")
		      (wav  . "audio/wave")
		      (mov  . "video/quicktime")
		      (mpg  . "video/mpeg")
                      (png  . "image/png")
                      (bmp  . "image/bmp")
                      (gif  . "image/gif")
                      (jpg  . "image/jpeg")
		      (tif  . "image/tiff")
		      (pic  . "image/x-pict")))
          table))
      
      
      ; --------------------------------------------------------------------------
      ; SERVING SERVLETS:
      
      ; servlet-instance = (make-servlet-instance Nat Channel (Hashtable Symbol -> cont))
      (define-struct servlet-instance (k-counter channel cont-table))
      
      (define FILE-FORM-REGEXP (regexp "multipart/form-data; *boundary=(.*)"))
      
      ; servlet-content-producer : custodian Method URL Bindings iport oport host Script-table Instance-table bool -> Void
      ; to find and run a servlet program, wait for the result, and output the page
      (define (servlet-content-producer custodian meth uri headers in out host-info scripts instances close)
        (if (eq? meth 'head)
            (output-headers out 200 "Okay" (current-seconds) TEXT/HTML-MIME-TYPE null close)
            (let ([binds
                   (case meth
                     [(get) (url-query uri)]
                     [(post)
                      (let ([content-type (assq 'content-type headers)])
                        (cond
                          [(and content-type (regexp-match FILE-FORM-REGEXP (cdr content-type)))
                           => (lambda (content-boundary)
                                (map (lambda (part)
                                       ; more here - better checks, avoid string-append
                                       (cons (get-field-name (cdr (assq 'content-disposition (car part))))
                                             (apply string-append (cdr part))))
                                     (read-mime-multipart (cadr content-boundary) in)))]
                          [else
                           (let ([len-str (assq 'content-length headers)])
                             (if len-str
                                 (cond
                                   [(string->number (cdr len-str))
                                    => (lambda (len) (read-string len in))]
                                   [else (report-error out meth ((responders-protocol (host-responders host-info)) "Post request contained a non-numeric content-length") close)])
                                 (apply string-append
                                        (let read-to-eof ()
                                          (let ([s (read-string INPUT-BUFFER-SIZE in)])
                                            (if (eof-object? s)
                                                null
                                                (cons s (read-to-eof))))))))]))]
                     [else (raise "not implemented yet")])])
              ; more here - keep one channel per connection instead of creating new ones
              (let ([response (create-channel)])
                (let-values ([(host-ip client-ip) (tcp-addresses out)])
                  ((if (url-params uri) resume-servlet start-servlet)
                   custodian
                   response
                   meth uri headers binds host-info scripts instances host-ip client-ip))
                (output-page/port (channel-get response) out close)))))
      
      ; get-field-name : str -> sym
      (define (get-field-name rhs)
        (let ([x (regexp-match "name=(\"([^\"]*)\"|([^ ;]*))" rhs)])
          (unless x
            (error 'get-field-name "Couldn't extract form field name for file upload from ~a" x))
          (string->symbol (or (caddr x) (cadddr x)))))
      
      ; cached-load : Script-table str -> script
      ; timestamps are no longer checked for performance.  The cache must be explicitly
      ; refreshed (see dispatch).
      (define (cached-load scripts name)
        (let ([paths (build-path-list name)])
          (lookup-path scripts paths
                       (lambda () (reload-servlet-script scripts name paths)))))
      
      ; build-path-list : str -> (listof str)
      ; to build a list of paths from most specific to least specific containing
      ; directories starting with the given path
      (define (build-path-list path)
        (let-values ([(base extra dir?) (split-path path)])
          (if (string? base)
              (cons path (build-path-list base))
              (list path))))
      
      ; lookup-path : Script-table (listof str) (-> script) -> script
      (define (lookup-path scripts path-lst fail)
        (cond
          [(null? path-lst) (fail)]
          [else (hash-table-get scripts (hash-path (car path-lst))
                                (lambda () (lookup-path scripts (cdr path-lst) fail)))]))
      
      ; hash-path : str -> sym
      ; path must not be the empty string
      (define (hash-path path)
        ; Don't use normalize-path because it doesn't work for non-existant paths.
        ; Stick "a" on the end so that it doesn't end in slash.
        ; This is easier than removing the slash that may or may not
        ; be there and is not platform specific.
        (string->symbol (build-path path "a")))
      
      ; exn:i/o:filesystem:servlet-not-found = 
      ; (make-exn:i/o:filesystem:servlet-not-found str continuation-marks str sym)
      (define-struct (exn:i/o:filesystem:servlet-not-found exn:i/o:filesystem) ())
      
      ; reload-servlet-script : Script-table str (listof str) -> script
      (define (reload-servlet-script scripts original-name paths)
        (or (ormap (lambda (name)
                     (and (file-exists? name)
                          ; MF: I'd also like to test that s has the correct import signature.
                          (let ([s (load/use-compiled name)])
                            (if (unit/sig? s)
                                (begin (hash-table-put! scripts (hash-path original-name) s)
                                       (hash-table-put! scripts (hash-path name) s)
                                       s)
                                (raise (format "looking up a script didn't yield a unit/sig: ~e" s))))))
                   paths)
            (raise (make-exn:i/o:filesystem:servlet-not-found
                    (format "Couldn't find ~a" original-name)
                    (current-continuation-marks)
                    original-name 'ill-formed-path))))
      
      (define URL-PARAMS:REGEXP (regexp "([^\\*]*)\\*(.*)"))
      
      (define (match-url-params x) (regexp-match URL-PARAMS:REGEXP x))
      ;:(define match-url-params (type: (str -> (union false (list str str str)))))
      
      ; resume-servlet : custodian channel method Url bindings bindings host scripts-table instance-table str str -> void
      ; to pass the request to the waiting thread that suspended the computation refered to by this url
      ; the url-params can't be #f
      (define (resume-servlet top-custodian response method uri headers bindings host-info unused-scripts instances host-ip client-ip)
        (with-handlers ([exn:application:mismatch?
                         (lambda (exn) (timeout-error method uri response))])
          (cond
            [(match-url-params (url-params uri))
             => (lambda (ids)
                  (let* ([invoke-id (string->symbol (cadr ids))]
                         [k-id (string->symbol (caddr ids))]
                         [inst (hash-table-get instances invoke-id)])
                    (channel-put (servlet-instance-channel inst)
                                 (list response (hash-table-get (servlet-instance-cont-table inst) k-id)
                                       (make-request method uri headers bindings host-ip client-ip)))))]
            [else (raise "malformed url-params when resuming servlet program")])))
      
      ; start-servlet : custodian channel method Url bindings bindings host scripts-table instance-table str str -> void
      ; to start a new servlet program that will handle this request
      (define (start-servlet top-custodian response method uri headers bindings host-info scripts instances host-ip client-ip)
        (let* ([invoke-id (string->symbol (symbol->string (gensym 'id)))]
               [purge-table
                (lambda ()
                  (let ([inst (hash-table-get instances invoke-id (lambda () #f))])
                    (when inst
                      (hash-table-remove! instances invoke-id)
                      (let loop ()
                        (channel-get-available
                         (servlet-instance-channel inst)
                         (lambda (x)
                           (timeout-error method uri (car x))
                           (loop)))))))]
               [time-out-seconds TIMEOUT-DEFAULT]
               [respond (lambda (page) (channel-put response page))])
          (let ([servlet-custodian (make-custodian top-custodian)])
            (parameterize ([current-custodian servlet-custodian]
                           [read-case-sensitive #t]
                           [exit-handler (lambda (x) (purge-table) (custodian-shutdown-all servlet-custodian))])
              (let* ([timer (start-timer time-out-seconds exit)]
                     [adjust-timeout! (lambda (n) (set! time-out-seconds n) (reset-timer timer n))]
                     [resume-next-request
                      (gen-resume-next-request (lambda () (reset-timer timer time-out-seconds)) 
                                               ; channel -> void
                                               (lambda (new-response)
                                                 (set! response new-response)))]
                     [send/suspend ; : (str[Url] -> Response) -> Request
                      (gen-send/suspend uri invoke-id instances respond resume-next-request)]
                     [send/forward ; : (str[Url] -> Response) -> Request
                      (lambda (page-maker) (purge-table) (send/suspend page-maker))]
                     [send/back ; : (Response -> doesn't)
                      (lambda (page)
			(respond page)
			(if (hash-table-empty?
			     (servlet-instance-cont-table
			      (hash-table-get instances invoke-id (lambda () (exit)))))
			    (exit)
			    (resume-next-request
			     (hash-table-get instances invoke-id (lambda () (exit))))))]
		     [send/finish ; : (Response -> doesn't)
		      (lambda (page) (respond page) (exit))])
                (thread
                 (lambda ()
		   (send/back 
		    (with-handlers ([exn:i/o:filesystem:servlet-not-found?
				     (lambda (exn)
				       (decapitate method ((responders-file-not-found (host-responders host-info)) uri)))]
				   [void (lambda (exn)
					   (decapitate method ((responders-servlet-loading (host-responders host-info)) uri exn)))])
				  (let ([servlet-program
					 (cached-load scripts
						      (url-path->path (paths-servlet (host-paths host-info))
								      (url-path uri)))]
					[initial-request (make-request method uri headers bindings host-ip client-ip)])
				    (add-new-instance invoke-id instances)
				    (with-handlers ([void (lambda (exn)
							    (decapitate method ((responders-servlet (host-responders host-info)) uri exn)))])
						   (invoke-unit/sig servlet-program servlet^))))))))))))
      
      ; response = (cons str (listof str)), where the first str is a mime-type
      ;          | x-expression
      ;          | (make-response/full nat str nat str (listof (cons sym str)) (listof str))
      ;          | (make-response/incremental nat str nat str (listof (cons sym str))
      ;              ((str -> void) -> void))
      
      ; output-page/port : response oport bool -> void
      (define (output-page/port page out close)
        ; double check what happens on erronious servlet output
        ; it should output an error for this response
        (cond
          [(response/full? page)
           (cond
             [(response/incremental? page)
              (output-headers out (response/full-code page) (response/full-message page)
                              (response/full-seconds page) (response/full-mime page)
                              (if close
                                  null
                                  `(("Transfer-Encoding: chunked")
                                    . ,(map (lambda (x) (list (symbol->string (car x)) ": " (cdr x)))
                                            (response/full-extras page))))
                              close)
              (if close
                  ; WARNING: This is unreliable because the client can not distinguish between
                  ; a dropped connection and the end of the file.  This is an inherit limitation
                  ; of HTTP/1.0.  Other cases where we close the connection correspond to work arounds
                  ; for buggy IE versions, at least some of which don't support chunked either.
                  ((response/full-body page)
                   (lambda chunks
                     (for-each (lambda (chunk) (display chunk out)) chunks)))
                  (begin 
                    ((response/full-body page)
                     (lambda chunks
                       (fprintf out "~x\r\n" (foldl (lambda (c acc) (+ (string-length c) acc)) 0 chunks))
                       (for-each (lambda (chunk) (display chunk out)) chunks)
                       (fprintf out "\r\n")))
                    ; one \r\n ends the last (empty) chunk and the second \r\n ends the (non-existant) trailers
                    (fprintf out "0\r\n\r\n")))]
             [else 
              (output-headers out (response/full-code page) (response/full-message page)
                              (response/full-seconds page) (response/full-mime page)
                              `(("Content-length: " ,(apply + (map string-length (response/full-body page))))
                                . ,(map (lambda (x) (list (symbol->string (car x)) ": " (cdr x)))
                                        (response/full-extras page)))
                              close)
              (for-each (lambda (str) (display str out))
                        (response/full-body page))])]
          [(string? (car page))
           (output-headers out 200 "Okay" (current-seconds) (car page)
                           `(("Content-length: " ,(apply + (map string-length (cdr page)))))
                           close)
           (for-each (lambda (str) (display str out))
                     (cdr page))]
          [else
           (let ([str (with-handlers ([void (lambda (exn)
                                              (if (exn? exn)
                                                  (exn-message exn)
                                                  (format "~s" exn)))])
                        (xexpr->string page))])
             (output-headers out 200 "Okay" (current-seconds) TEXT/HTML-MIME-TYPE
                             `(("Content-length: " ,(add1 (string-length str))))
                             close)
             (display str out) ; the newline is for an IE 5.5 bug workaround
             (newline out))]))
      
      ; add-new-instance : sym instance-table -> void
      (define (add-new-instance invoke-id instances)
        (hash-table-put! instances invoke-id
                         (make-servlet-instance 0 (create-channel) (make-hash-table))))
      
      ; gen-send/suspend : url sym instance-table (response -> void) (instance -> doesn't) -> (str -> response) -> request
      (define (gen-send/suspend uri invoke-id instances output-page resume-next-request)
        (lambda (page-maker)
          (let/cc k
            (let* ([inst (hash-table-get instances invoke-id)]
                   [c (servlet-instance-channel inst)]
                   [k-count (add1 (servlet-instance-k-counter inst))]
                   [cont-table (servlet-instance-cont-table inst)]
                   [k-id (format "k~a-~a" k-count (random 2000000000))])
              (set-servlet-instance-k-counter! inst k-count)
              (hash-table-put! cont-table (string->symbol k-id) k)
              (output-page (page-maker (update-params uri (format "~a*~a" invoke-id k-id))))
              (resume-next-request inst)))))
      
      ; :  (-> void) (channel -> void) -> instance -> doesn't
      (define (gen-resume-next-request update-time! update-channel!)
        (lambda (inst)
          (let ([resume (channel-get (servlet-instance-channel inst))])
            ; set! - modeling things that change over time
            (update-time!)
            ; set! justified - communicating between threads
            (update-channel! (car resume))
            ((cadr resume) (caddr resume)))))
      
      ; update-params : Url (U #f String) -> String
      ; to create a new url just like the old one, but with a different parameter part
      (define (update-params uri params)
        (url->string
         (make-url (url-scheme uri) (url-host uri) (url-port uri) 
                   (url-path uri) params (url-query uri) 
                   (url-fragment uri))))
      
      (define TIME-OUT-CODE 200)
      (define TIME-OUT-HEADERS null)
      
      ;(define TIME-OUT-HEADERS
      ;  `(("Location: " ,(url-path uri))
      ;    ("Retry-After: 5")))
      
      ; timeout-error : Method Url channel -> void
      ; the code must not be an error because some configurations of IE produce
      ; "Internal Error, please see Microsoft's search engine" for 500 responses
      ; instead of displaying the server's error message.
      (define (timeout-error method uri channel)
        (channel-put
         channel
         (make-response/full
          TIME-OUT-CODE "Timeout" (current-seconds) TEXT/HTML-MIME-TYPE TIME-OUT-HEADERS
          (if (eq? method 'head)
              null
              (list (xexpr->string
                     `(html (head (title "Timeout"))
                            (body
                             (p "The transaction referred to by this url is no longer active."
                                "  Please " (a ((href ,(update-params uri #f))) "restart")
                                " the transaction.")))))))))
      
      ; --------------------------------------------------------------------------
      ; COMMON LIBRARY FUNCTIONS: 
      
      ; output-headers : oport Nat String Nat String (listof (listof String)) bool -> Void
      (define (output-headers out code message seconds mime extras close)
        (for-each (lambda (line)
                    (for-each (lambda (word) (display word out)) line)
                    (display #\return out)
                    (newline out))
                  (list* `("HTTP/1.1 " ,code " " ,message)
                         `("Date: " ,(seconds->gmt-string seconds))
                         `("Server: PLT Scheme")
                         `("Content-type: " ,mime)
                         ; more here - consider removing Connection fields from extras or raising an error
                         (if close
                             (cons `("Connection: close") extras)
                             extras)))
        (display #\return out)
        (newline out))
      
      ; two-digits : num -> str
      (define (two-digits n)
        (let ([str (number->string n)])
          (if (< n 10) (string-append "0" str) str)))
      
      ; seconds->gmt-string : Nat -> String
      ; format is rfc1123 compliant according to rfc2068 (http/1.1)
      (define (seconds->gmt-string s)
        (let* ([local-date (seconds->date s)]
               [date (seconds->date (- s 
                                       (date-time-zone-offset local-date)
                                       (if (date-dst? local-date) 3600 0)))])
          (format "~a, ~a ~a ~a ~a:~a:~a GMT"
                  (vector-ref DAYS (date-week-day date))
                  (two-digits (date-day date))
                  (vector-ref MONTHS (sub1 (date-month date)))
                  (date-year date)
                  (two-digits (date-hour date))
                  (two-digits (date-minute date))
                  (two-digits (date-second date)))))
      
      ; more here - include doc.txt
      (define DEFAULT-ERROR "An error message configuration file is missing.")
      
      ; report-error : oport method response bool -> void
      (define (report-error out method response close)
        (output-page/port (decapitate method response) out close))
      
      ; decapitate : method response -> response
      ; to remove the body if the method is 'head
      (define (decapitate method response)
        (if (eq? method 'head)
            (cond
              [(response/full? response)
               (make-response/full (response/full-code response)
                                   (response/full-message response)
                                   (response/full-seconds response)
                                   (response/full-mime response)
                                   (response/full-extras response)
                                   (response/full-body response))]
              [else (make-response/full 200 "Okay" (current-seconds) TEXT/HTML-MIME-TYPE null null)])
            response))
      
      ))
  ;: #|
  (define servlet-bin? (prefix? "/servlets/"))
  (define basic? (prefix? "Basic "))
  (define conf-prefix? (prefix? "/conf/"))
  ;: |#
  ;:(define (servlet-bin? x) ((prefix? "/servlets/") x))
  ;:(define (basic? x) ((prefix? "Basic ") x))
  ;:(define (conf-prefix? x) ((prefix? "/conf/") x))
  )
