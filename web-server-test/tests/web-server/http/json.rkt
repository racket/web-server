#lang racket

(require tests/eli-tester
         web-server/private/timer
         web-server/private/connection-manager
         web-server/http/response
         web-server/http
         "../util.rkt"
         (file "util.rkt"))

(define tm (start-timer-manager))

(test
 (write-response (response/jsexpr '("booga")))
 =>
 (bytes-sort
  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: application/json; charset=utf-8\r\n\r\n[\"booga\"")

 ; Showing off all default headers and other values (200, "OK"):
 (write-response (response/jsexpr (hasheq 'jay "zee")))
 =>
 (bytes-sort
  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\n\r\n{\"jay\":\"zee\"}")

 ; We respect the json-null parameter:
 (write-response (parameterize ([json-null 'jibby-jab])
                   (response/jsexpr 'jibby-jab)))
 =>
 (bytes-sort
  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\n\r\nnull")

 (write-response (response/jsexpr "jazzy jazz"
                                  #:code 404))
 =>
 (bytes-sort
  #"HTTP/1.1 404 Not Found\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\n\r\n\"jazzy jazz\"")

 ; The message is normally inferred from the status code,
 ; but you can use whatever you want:
 (write-response (response/jsexpr #f
                                  #:message #"Strange request"))
 =>
 (bytes-sort
  #"HTTP/1.1 200 Strange request\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\n\r\nfalse")

 (map
  (λ (b) (regexp-replace
          #"Date: [a-zA-Z0-9:, ]+ GMT"
          b
          #"Date: REDACTED GMT"))
  (write-response (response/jsexpr (list "whoop" (hasheq 'there '("it" "is")))
                                   #:seconds 0)
                  #f))
 =>
 (bytes-sort
  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: Wed, 31 Dec 1969 23:00:00 GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\nConnection: close\r\n\r\n[\"whoop\",{\"there\":[\"it\",\"is\"]")

 ; The default MIME type ("application/json; charset=utf-8")
 ; can be overridden:
 (write-response (response/jsexpr ""
                                  #:mime-type #"application/xml"))
 =>
 (bytes-sort
  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: application/xml\r\n\r\n\"\"")

 ; Custom headers can be used:
 (write-response (response/jsexpr #t
                                  #:headers (list (header #"head" #"bang"))))
 =>
 (bytes-sort
  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\nhead: bang\r\n\r\ntrue"))
