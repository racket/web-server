; The main program of the "web-server" launcher. 
(module launch mzscheme
  (require (lib "cmdline.ss")
           "web-server.ss"
           "configuration.ss"
           "configuration-structures.ss"
           "parse-table.ss")
  
  (define default-configuration-table-path
    (build-path (collection-path "web-server") "configuration-table"))
  
  ; load-configuration : str -> configuration
  (define (load-configuration table-file-name)
    (complete-configuration (parse-configuration-table (call-with-input-file table-file-name read))))
  
  (let* ([configuration-path default-configuration-table-path]
         [port #f])
    (command-line
     "web-server"
     (namespace-variable-binding 'argv)
     (once-each
      [("-f" "--configuration-table") file-name "Use an alternate configuration table"
       (set! configuration-path file-name)]
      [("-p" "--port") new-port "Use an alternate network port."
       (let ([port-number (string->number new-port)])
         (if port-number
             (set! port port-number)
             (error 'web-server "-p expected a numeric argument, given ~a" new-port)))]))
    (let ([configuration (load-configuration configuration-path)])
      (serve configuration
             (or port (configuration-port configuration)))))
  
  (semaphore-wait (make-semaphore)))
