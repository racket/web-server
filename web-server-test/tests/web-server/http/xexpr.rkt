#lang racket
(require tests/eli-tester
         web-server/private/timer
         web-server/private/connection-manager
         web-server/http/response
         web-server/http
         (submod web-server/http/response testing)
         "../util.rkt"
         (file "util.rkt"))

(define tm (start-timer-manager))

(test
 (write-response tm (response/xexpr '(a ([href "#"]) "link")))
 =>
 (bytes-sort
  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\n\r\n<a href=\"#\">link</a>")

 (write-response tm (response/xexpr '(html)))
 =>
 (bytes-sort
  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\n\r\n<html></html>")

 (write-response tm (response/xexpr '(img)))
 =>
 (bytes-sort
  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\n\r\n<img/>")

 (write-response tm (response/xexpr '(a ([href "#"]) "link")
                                    #:code 404))
 =>
 (bytes-sort
  #"HTTP/1.1 404 Not Found\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\n\r\n<a href=\"#\">link</a>")

 (write-response tm (response/xexpr '(a ([href "#"]) "link")
                                    #:message #"Bad request"))
 =>
 (bytes-sort
  #"HTTP/1.1 200 Bad request\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\n\r\n<a href=\"#\">link</a>")

 (map
  (λ (b) (regexp-replace
          #"Date: [a-zA-Z0-9:, ]+ GMT"
          b
          #"Date: REDACTED GMT"))
  (write-response tm (response/xexpr '(a ([href "#"]) "link")
                                     #:seconds 0)
                  #f))
 =>
 (bytes-sort
  (bytes-append
   #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: "
   (seconds->gmt-bytes 0)
   #"\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\nConnection: close\r\n\r\n<a href=\"#\">link</a>"))


 (write-response tm (response/xexpr '(a ([href "#"]) "link")
                                    #:mime-type #"application/xml"))
 =>
 (bytes-sort
  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: application/xml\r\n\r\n<a href=\"#\">link</a>")

 (write-response tm (response/xexpr '(a ([href "#"]) "link")
                                    #:headers (list (header #"head" #"value"))))
 =>
 (bytes-sort
  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\nhead: value\r\n\r\n<a href=\"#\">link</a>")

 (write-response tm (response/xexpr '(a ([href "#"]) "link")
                                    #:cookies (list (make-cookie "head" "value"))))
 =>
 (bytes-sort
  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\nSet-Cookie: head=value\r\n\r\n<a href=\"#\">link</a>")

 (write-response tm (response/xexpr '(a ([href "#"]) "link")
                                    #:preamble #"<<!something XMLy>>"))
 =>
 (bytes-sort
  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\n\r\n<<!something XMLy>><a href=\"#\">link</a>"))
