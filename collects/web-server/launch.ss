; The main program of the "web-server" launcher. 
(module launch mzscheme
  (require "web-server.ss"
           "configuration.ss")

  (define argv (namespace-variable-binding 'argv))

  (case (vector-length argv)
    [(0) (serve port virtual-hosts max-waiting)]
    [(1) (serve (string->number (vector-ref argv 0)) virtual-hosts max-waiting)]
    [else (error 'web-server "launcher expects zero or one arguments.  The one argument is the port number.")])
  
  (semaphore-wait (make-semaphore)))
