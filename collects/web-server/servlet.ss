;; Default choice for writing module servlets
;; (To write module servlets using a smaller servlet library then
;;  require send.ss in conjunction with min-servlet.ss)
(module servlet mzscheme
  (require "send.ss"
           "servlet-helpers.ss")

  (provide (all-from "send.ss")
           (all-from "servlet-helpers.ss")))
  
