(module web-server mzscheme
  (require (lib "tcp-sig.ss" "net")
           (lib "unitsig.ss")
           "servlet-sig.ss"
           "web-server-unit.ss")
  
  (provide-signature-elements web-server^)
  
  (define-values/invoke-unit/sig web-server^ web-server@ #f net:tcp^))