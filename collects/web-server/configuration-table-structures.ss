(module configuration-table-structures mzscheme
  (require "util.ss")
    
  ; host-table = (make-host-table (listof str) str (str str -> (U #f str)) passwords messages timeouts paths)
  (provide-define-struct host-table (indices servlet-root log-format passwords messages timeouts paths))
  
  ; passwords = (listof (list* relm:str protected-dir-regexp:str (listof (list user:sym password:str))))
  ; passwords moved back to a separate file
  
  ; messages = (make-messages str^7)
  (provide-define-struct messages
    (servlet ;servlet-loading
     authentication servlets-refreshed passwords-refreshed file-not-found protocol))
  
  ; timeouts = (make-timeouts nat^6)
  (provide-define-struct timeouts (default-servlet password servlet-connection file-per-byte file-base))
  
  ; paths = (make-paths str^10)
  (provide-define-struct paths (host-base log htdocs servlet)))
