#lang racket/base
(require net/url
         (prefix-in srfi-date: srfi/19)
         racket/date
         racket/async-channel
         racket/match
         racket/contract)
(require web-server/dispatchers/dispatch
         web-server/http
         web-server/http/response)
(define format-reqresp/c (request? response? . -> . string?))
(define log-format/c (symbols 'parenthesized-default 'extended 'apache-default))

(provide/contract
 [format-reqresp/c contract?]
 [log-format/c contract?]
 [log-format->format (log-format/c . -> . format-reqresp/c)]
 [paren-format format-reqresp/c]
 [extended-format format-reqresp/c]
 [apache-default-format format-reqresp/c]
 [interface-version dispatcher-interface-version/c]
 [make (->* (dispatcher/c)
            (#:format (or/c log-format/c format-reqresp/c)
             #:log-path (or/c path-string? output-port?))
            dispatcher/c)])

(define ((log-header-handler log-message req original-handler) resp)
  (log-message req resp)
  (original-handler resp))

(define interface-version 'v1)
(define (make #:format [format paren-format]
              #:log-path [log-path "log"]
              dispatcher)
  (define final-format
    (if (symbol? format)
        (log-format->format format)
        format))
  (define log-message (make-log-message log-path final-format))
  (lambda (conn req)
    (with-handlers ([exn:dispatcher? (lambda (e) (next-dispatcher))])
      (parameterize ([current-header-handler (log-header-handler log-message req (current-header-handler))])
        (dispatcher conn req)))))

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
(define (apache-default-format req resp)
  (define request-time (srfi-date:current-date))
  (format "~a - - [~a] \"~a\" ~a ~a\n"
          (request-client-ip req)
          (srfi-date:date->string request-time "~d/~b/~Y:~T ~z")
          (request-line-raw req)
          (response-code resp)
          "-"))

(define (paren-format req resp)
  (format "~s\n"
          (list 'from (request-client-ip req)
                'to (request-host-ip req)
                'for (url->string (request-uri req))
                'at (date->string (seconds->date (current-seconds)) #t)
                'code (response-code resp))))

(define (extended-format req resp)
  (format "~s\n"
          `((client-ip ,(request-client-ip req))
            (host-ip ,(request-host-ip req))
            (referer ,(let ([R (headers-assq* #"Referer" (request-headers/raw req))])
                        (if R
                            (header-value R)
                            #f)))
            (uri ,(url->string (request-uri req)))
            (time ,(current-seconds))
            (code ,(response-code resp)))))

(define (make-log-message log-path-or-port format-reqresp)
  (define log-ch (make-async-channel))
  (define log-thread
    (thread/suspend-to-kill
     (lambda ()
       (let loop ([log-p #f])
         (sync
          (handle-evt
           log-ch
           (match-lambda
             [(list req resp)
              (loop
               (with-handlers ([exn:fail? (lambda (e)
                                            ((error-display-handler) "dispatch-logresp.rkt Error writing log entry" e)
                                            (with-handlers ([exn:fail? (lambda (e) #f)])
                                              (close-output-port log-p))
                                            #f)])
                 (define the-log-p
                   (if (path-string? log-path-or-port)
                       (if (not (and log-p (file-exists? log-path-or-port)))
                           (begin
                             (unless (eq? log-p #f)
                               (close-output-port log-p))
                             (let ([new-log-p (open-output-file log-path-or-port #:exists 'append)])
                               (file-stream-buffer-mode new-log-p 'line)
                               new-log-p))
                           log-p)
                       log-path-or-port))
                 (display (format-reqresp req resp) the-log-p)
                 the-log-p))])))))))
  (lambda args
    (thread-resume log-thread (current-custodian))
    (async-channel-put log-ch args)
    (void)))
