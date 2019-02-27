#lang racket/base
(require racket/contract
         (only-in racket/string non-empty-string?)
         web-server/http/response-structs
         web-server/http/request-structs)

; redirection-status = (redirection-status nat bytes)
(struct redirection-status (code message))

(define permanently
  ;; NOTE: 308 permanent redirect is not supported by
  ;; Internet Explorer on Windows 7 or 8.1 as of 2019-02-26.
  ;; (IE on Windows 10 does support it.)
  ;; https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/308
  ;; https://tools.ietf.org/html/rfc7538#section-4
  (redirection-status 301 #"Moved Permanently"))
(define temporarily (redirection-status 302 #"Found"))
(define temporarily/same-method (redirection-status 307 #"Temporary Redirect"))
(define see-other (redirection-status 303 #"See Other"))

; : str [redirection-status] -> response
(define (redirect-to 
         uri
         [perm/temp temporarily]
         #:headers [headers (list)])
  (response (redirection-status-code perm/temp)
            (redirection-status-message perm/temp)
            (current-seconds) #f
            (list* (make-header #"Location" (string->bytes/utf-8 uri))
                   headers)
            void))

(provide/contract
 [redirect-to
  (->* (non-empty-string?) (redirection-status? #:headers (listof header?))
       response?)]
 [redirection-status? (any/c . -> . boolean?)]
 [permanently redirection-status?]
 [temporarily redirection-status?]
 [temporarily/same-method redirection-status?]
 [see-other redirection-status?])
