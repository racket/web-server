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
          (cons 'config file-name))
       ("Use an alternate configuration table" "file-name")]
      [("-p" "--port")
       ,(lambda (flag port)
          (cons 'port (string->number port)))
       ("Use an alternate network port." "port")]))
   (lambda (flags)
     (let ([configuration
            (load-configuration
             (extract-flag 'config flags default-configuration-table-path))])
       (serve configuration
              (extract-flag 'port flags (configuration-port configuration)))))
   '())
  
  (semaphore-wait (make-semaphore)))
