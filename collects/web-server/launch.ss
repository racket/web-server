; The main program of the "web-server" launcher. 
(module launch mzscheme
  (require (lib "cmdline.ss")
           "util.ss"
           "web-server.ss"
           "configuration.ss"
           "configuration-structures.ss")
  
  (parse-command-line
   "web-server"
   (namespace-variable-binding 'argv)
   `((once-each
      [("-f" "--configuration-table")
       ,(lambda (flag file-name)
          (cond
            [(not (file-exists? file-name))
             (error 'web-server "configuration file ~s not found" file-name)]
            [(not (memq 'read (file-or-directory-permissions file-name)))
             (error 'web-server "configuration file ~s is not readable" file-name)]
            [else (cons 'config file-name)]))
       ("Use an alternate configuration table" "file-name")]
      [("-p" "--port")
       ,(lambda (flag port)
          (let ([p (string->number port)])
            (if (and (number? p) (integer? p) (exact? p) (<= 1 p 65535))
                (cons 'port p)
                (error 'web-server "port expects an argument of type <exact integer in [1, 65535]>; given ~s" port))))
       ("Use an alternate network port." "port")]))
   (lambda (flags)
     (let ([configuration
            (load-configuration
             (extract-flag 'config flags default-configuration-table-path))])
       (serve configuration
              (extract-flag 'port flags (configuration-port configuration)))))
   '())
  
  (semaphore-wait (make-semaphore)))
