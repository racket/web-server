(module parse-table mzscheme
  (require (lib "match.ss")
           "configuration-table-structs.ss")
  (provide parse-configuration-table)
    
  ; parse-configuration-table : tst -> configuration-table
  (define parse-configuration-table
    (match-lambda
     [`((port ,port)
        (max-waiting ,max-waiting)
        (initial-connection-timeout ,initial-connection-timeout)
        (default-host-table
         ,default-host-table)
        (virtual-host-table . ,virtual-host-table))
      (if (and (nat? port) (nat? max-waiting) (number? initial-connection-timeout)
               ; more here - list? isn't really picky enough
               (list? virtual-host-table))
          (make-configuration-table
           port max-waiting initial-connection-timeout
           (parse-host default-host-table)
           (map (lambda (h)
                  (if (and (pair? h) (pair? (cdr h)) (null? (cddr h)))
                      (cons (car h) (parse-host (cadr h)))
                      (error 'parse-configuration-table "invalid virtual-host entry ~s" h)))
                virtual-host-table))
          (error 'parse-configuration-table "invalid configuration values ~s"
                 (list port max-waiting initial-connection-timeout default-host-table virtual-host-table)))]
     [x (error 'parse-configuration-table "malformed configuration ~s" x)]))
  
  ; parse-host : tst -> host-table
  (define parse-host
    (match-lambda
     [`(host-table
        (default-indices . ,default-indices)
        (log-format ,log-format)
        (messages
         (servlet-message ,servlet-message)
         (authentication-message ,authentication-message)
         (servlets-refreshed ,servlets-refreshed)
         (passwords-refreshed ,passwords-refreshed)
         (file-not-found-message ,file-not-found-message)
         (protocol-message ,protocol-message))
        (timeouts
         (default-servlet-timeout ,default-servlet-timeout)
         (password-connection-timeout ,password-connection-timeout)
         (servlet-connection-timeout ,servlet-connection-timeout)
         (file-per-byte-connection-timeout ,file-per-byte-connection-timeout)
         (file-base-connection-timeout ,file-base-connection-timeout))
        (paths ,@(path-exps ...)))
      (make-host-table
       default-indices log-format
       (make-messages servlet-message
                      authentication-message
                      servlets-refreshed
                      passwords-refreshed
                      file-not-found-message
                      protocol-message)
       (make-timeouts default-servlet-timeout
                      password-connection-timeout
                      servlet-connection-timeout
                      file-per-byte-connection-timeout
                      file-base-connection-timeout)
       (parse-paths
        path-exps
        (make-paths #f #f #f '() '() #f)
        (lambda (err-str)
          (error 'parse-paths "malformed host, missing ~a" err-str))))]
        ;(make-paths configuration-root host-root log-file-path file-root servlet-root password-authentication))]
     [x (error 'parse-host "malformed host ~s" x)]))
  
  ;; a path-exp is either
  ;;;; (configuration-root ,string)
  ;;;; (host-root ,string)
  ;;;; (log-file-path ,string)
  ;;;; (file-root ,string)
  ;;;; (file-root ,string ,string)
  ;;;; (servlet-root ,string)
  ;;;; (servlet-root ,string ,string)
  ;;;; (password-authentication ,string)
  
  ;; parse-paths : (listof path-exp) paths (string -> ) -> paths
  ;; parse the path-exp part of a configuration
  ;; it is now possible to specify multiple file-roots and servlet-roots
  (define (parse-paths path-exps new-paths path-error)
    (cond
      [(null? path-exps)
       (and (or (paths-conf new-paths)
                (path-error "configuration-root"))
            (or (paths-host-base new-paths)
                (path-error "host-root"))
            (or (paths-log new-paths)
                (path-error "log-file-path"))
            (when (null? (paths-htdocs new-paths))
              (path-error "file-root"))
            (when (null? (paths-servlet new-paths))
              (path-error "servlet-root"))
            (or (paths-passwords new-paths)
                (path-error "password-authentication"))
            new-paths)]
      [else (parse-path (car path-exps) new-paths)
            (parse-paths (cdr path-exps) new-paths path-error)]))
  
  ;; parse-path: path-exp paths ->
  ;; parse the path-exp and mutate the paths structure
  (define (parse-path path-exp new-paths)
    (match path-exp
      [`(configuration-root ,configuration-root)
        (and (paths-conf new-paths)
             (error 'parse-path "duplicate configuration-root"))
        (set-paths-conf! new-paths configuration-root)]
      [`(host-root ,host-root)
        (and (paths-host-base new-paths)
             (error 'parse-path "duplicate host-root"))
        (set-paths-host-base! new-paths host-root)]
      [`(log-file-path ,log-file-path)
        (and (paths-log new-paths)
             (error 'parse-path "duplicate log-file-path"))
        (set-paths-log! new-paths log-file-path)]
      [`(file-root ,path-prefix ,a-path)
        (set-paths-htdocs! new-paths (cons (cons path-prefix a-path) (paths-htdocs new-paths)))]
      [`(file-root ,a-path)
        (set-paths-htdocs! new-paths (cons (cons "/" a-path) (paths-htdocs new-paths)))]
      [`(servlet-root ,path-prefix ,a-path)
        (set-paths-servlet! new-paths (cons (cons path-prefix a-path) (paths-servlet new-paths)))]
      [`(servlet-root ,a-path)
        (set-paths-servlet! new-paths (cons (cons "/servlets" (build-path a-path "servlets")) (paths-servlet new-paths)))]
      [`(password-authentication ,password-authentication)
        (and (paths-passwords new-paths)
             (error 'parse-path "duplicate password-authentication"))
        (set-paths-passwords! new-paths password-authentication)]
      [else (error 'parse-path "malformed path expression ~s" path-exp)]))
  
  ; nat? : tst -> bool
  (define (nat? x)
    (and (number? x) (exact? x) (integer? x) (<= 0 x))))
  
 