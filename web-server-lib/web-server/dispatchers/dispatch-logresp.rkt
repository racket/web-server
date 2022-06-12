#lang racket/base

(require racket/contract
         web-server/dispatchers/dispatch
         web-server/http
         web-server/http/response
         "private/log.rkt"
         (submod web-server/dispatchers/dispatch-log private))

(define format-reqresp/c
  (or/c (-> request? string?)
        (-> request? response? string?)))

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

(define interface-version 'v1)

(define ((log-header-handler log-message req original-handler) resp)
  (define new-resp (original-handler resp))
  (log-message req new-resp)
  new-resp)

(define (make #:format [format paren-format]
              #:log-path [log-path "log"]
              dispatcher)
  (define final-format
    (if (symbol? format)
        (log-format->format format)
        format))
  (define log-message (make-log-message
                       log-path
                       (λ (req resp)
                         (cond
                           [(procedure-arity-includes? final-format 2)
                            (final-format req resp)]
                           [else (final-format req)]))))
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

(define apache-default-format
  (make-format "~a - - [~a] \"~a\" ~a -\n"
               (λ (req resp)
                 (append (apache-default-format/obj req)
                         (list (response-code resp))))))

(define paren-format
  (make-format "~s\n"
               (λ (req resp)
                 (list (append (car (paren-format/obj req))
                               (list 'code (response-code resp)))))))

(define extended-format
  (make-format "~s\n"
               (λ (req resp)
                 (list (append (car (extended-format/obj req))
                               (list (list 'code (response-code resp))))))))
