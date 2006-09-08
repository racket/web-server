(module dispatch-server-sig mzscheme
  (require (lib "unitsig.ss"))

  (define-signature dispatch-server^
    (serve
     serve-ports))
  
  (define-signature dispatch-server-config^
    (port listen-ip max-waiting initial-connection-timeout
          read-request dispatch))
  
  (provide ; XXX contract signature
   dispatch-server^ dispatch-server-config^))