#lang racket/base

(require net/url
         racket/contract
         racket/date
         (prefix-in srfi-date: srfi/19)
         web-server/dispatchers/dispatch
         web-server/http
         "private/log.rkt")

(define format-req/c (request? . -> . string?))

(provide/contract
 [format-req/c contract?]
 [log-format/c contract?]
 [log-format->format (log-format/c . -> . format-req/c)]
 [paren-format format-req/c]
 [extended-format format-req/c]
 [apache-default-format format-req/c]
 [interface-version dispatcher-interface-version/c]
 [make (->* ()
            (#:format (or/c log-format/c format-req/c)
             #:log-path (or/c path-string? output-port?))
            dispatcher/c)])

(define interface-version 'v1)

(define (make #:format [format paren-format]
              #:log-path [log-path "log"])
  (define final-format
    (if (symbol? format)
        (log-format->format format)
        format))
  (define log-message (make-log-message log-path final-format))
  (lambda (_conn req)
    (log-message req)
    (next-dispatcher)))

(define (log-format->format log-format)
  (case log-format
    [(parenthesized-default)
     paren-format]
    [(extended)
     extended-format]
    [(apache-default)
     apache-default-format]))

(define (request-line-raw req)
  (format "~a ~a HTTP/1.1"
          (string-upcase (bytes->string/utf-8 (request-method req)))
          (url->string (request-uri req))))

(define (apache-default-format/obj req)
  (define request-time (srfi-date:current-date))
  (list (request-client-ip req)
        (srfi-date:date->string request-time "~d/~b/~Y:~T ~z")
        (request-line-raw req)))

(define apache-default-format
  (make-format "~a - - [~a] \"~a\" - -\n" apache-default-format/obj))

(define (paren-format/obj req)
  (list (list 'from (request-client-ip req)
              'to (request-host-ip req)
              'for (url->string (request-uri req))
              'at (date->string (seconds->date (current-seconds)) #t))))

(define paren-format (make-format "~s\n" paren-format/obj))

(define (extended-format/obj req)
  `(((client-ip ,(request-client-ip req))
     (host-ip ,(request-host-ip req))
     (referer ,(let ([R (headers-assq* #"Referer" (request-headers/raw req))])
                 (if R
                     (header-value R)
                     #f)))
     (uri ,(url->string (request-uri req)))
     (time ,(current-seconds)))))

(define extended-format (make-format "~s\n" extended-format/obj))

(module+ private
  (provide apache-default-format/obj
           paren-format/obj
           extended-format/obj))
