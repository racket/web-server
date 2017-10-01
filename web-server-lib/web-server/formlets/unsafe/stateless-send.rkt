#lang web-server/base

(require "lib.rkt"
         web-server/lang/web
         web-server/http)

(provide send/formlet)

(define (send/formlet f
                      #:method
                      [method "POST"]
                      #:wrap 
                      [wrapper
                       (lambda (form-xexpr)
                         `(html (head (title "Form Entry"))
                                (body ,form-xexpr)))])
  (formlet-process 
   f
   (send/suspend
    (lambda (k-url)
      (response/xexpr
       (wrapper
        `(form ([action ,k-url] [method ,method])
               ,@(formlet-display f))))))))
