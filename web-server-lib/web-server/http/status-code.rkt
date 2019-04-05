#lang racket/base

(require racket/contract
         racket/match
         (only-in racket/string
                  non-empty-string?))

(provide/contract
 [message-for-status-code
  (number? . -> . (or/c false/c non-empty-string?))]
 [DEFAULT-STATUS-MESSAGE
  bytes?]
 [infer-response-message
  (number? (or/c false/c bytes?) . -> . bytes?)])

(module+ test
  (require rackunit))

;; HTTP status codes coming from
;;
;; + Hypertext Transfer Protocol (HTTP/1.1): Semantics and Content
;;   (https://tools.ietf.org/html/rfc7231)
;;
;; + Hypertext Transfer Protocol (HTTP/1.1): Authentication
;;   (https://tools.ietf.org/html/rfc7235)

(define/contract common-http-status-codes&messages
  (and/c (hash/c (integer-in 100 599) non-empty-string?)
         immutable?)
  (hasheq
   100 "Continue"
   101 "Switching Protocols"

   200 "OK"
   201 "Created"
   202 "Accepted"
   203 "Non-Authoritative Information"
   204 "No Content"
   205 "Reset Content"

   300 "Multiple Choices"
   301 "Moved Permanently"
   302 "Found"
   303 "See Other"
   305 "Use Proxy"
   307 "Temporary Redirect"

   400 "Bad Request"
   401 "Unauthorized"
   402 "Payment Required"
   403 "Forbidden"
   404 "Not Found"
   405 "Method Not Allowed"
   406 "Not Acceptable"
   407 "Proxy Authentication Required"
   408 "Request Timeout"
   409 "Conflict"
   410 "Gone"
   411 "Length Required"
   413 "Payload Too Large"
   414 "URI Too Long"
   415 "Unsupported Media Type"
   417 "Expectation Failed"
   426 "Upgrade Required"

   500 "Internal Server Error"
   501 "Not Implemented"
   502 "Bad Gateway"
   503 "Service Unavailable"
   504 "Gateway Timeout"
   505 "HTTP Version Not Supported"))

(module+ test
  (check-equal?
   (list 100 101
         200 201 202 203 204 205
         300 301 302 303 305 307
         400 401 402 403 404 405 406 407 408 409 410 411 413 414 415 417 426
         500 501 502 503 504 505)
   (sort (hash-keys common-http-status-codes&messages) <)))

(define (message-for-status-code code)
  (hash-ref common-http-status-codes&messages code #f))

(define DEFAULT-STATUS-MESSAGE #"OK")

(define (infer-response-message code message)
  (match message
    [(? bytes?)
     message]
    [else
     (match (message-for-status-code code)
       [(? string? s)
        (string->bytes/utf-8 s)]
       [else
        DEFAULT-STATUS-MESSAGE])]))
