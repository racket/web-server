#lang web-server/base

(require "lib.rkt")

(provide embed-formlet)

(define (embed-formlet embed/url f)
  `(form ([action ,(embed/url
                    (lambda (r)
                      (formlet-process f r)))])
         ,@(formlet-display f)))
