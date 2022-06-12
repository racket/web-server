#lang racket/base

(require net/url
         racket/contract
         racket/date
         racket/path
         (prefix-in srfi-date: srfi/19)
         web-server/dispatchers/dispatch
         web-server/http)

(define format-req/c (request? . -> . string?))
(define log-format/c (symbols 'parenthesized-default 'extended 'apache-default))

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

(define (apache-default-format req)
  (define request-time (srfi-date:current-date))
  (format "~a - - [~a] \"~a\" ~a ~a\n"
          (request-client-ip req)
          (srfi-date:date->string request-time "~d/~b/~Y:~T ~z")
          (request-line-raw req)
          "-"
          "-"))

(define (paren-format req)
  (format "~s\n"
          (list 'from (request-client-ip req)
                'to (request-host-ip req)
                'for (url->string (request-uri req)) 'at
                (date->string (seconds->date (current-seconds)) #t))))

(define (extended-format req)
  (format "~s\n"
          `((client-ip ,(request-client-ip req))
            (host-ip ,(request-host-ip req))
            (referer ,(let ([R (headers-assq* #"Referer" (request-headers/raw req))])
                        (if R
                            (header-value R)
                            #f)))
            (uri ,(url->string (request-uri req)))
            (time ,(current-seconds)))))

(define (make-log-message log-path-or-port format-req)
  (define path (if (output-port? log-path-or-port) #f log-path-or-port))
  (define dir-path (and path (simple-form-path (build-path path 'up))))
  (define (make-dir-change-evt)
    (if dir-path (filesystem-change-evt dir-path) never-evt))
  (define (open-output-port)
    (cond
      [path
       (define out (open-output-file path #:exists 'append))
       (begin0 out
         (file-stream-buffer-mode out 'line))]
      [else
       log-path-or-port]))
  (define log-ch (make-channel))
  (define log-thread
    (thread/suspend-to-kill
     (lambda ()
       (let loop ([log-p #f]
                  [dir-evt (make-dir-change-evt)])
         (sync
          (handle-evt
           dir-evt
           (lambda (_)
             (define-values (the-log-p the-dir-evt)
               (with-handlers ([exn:fail?
                                (lambda (e)
                                  ((error-display-handler) "dispatch-log.rkt Error watching filesystem" e)
                                  (close-output-port/safe log-p)
                                  (values #f never-evt))])
                 ;; Something in the directory changed ...
                 (cond
                   [(not log-p)
                    ;; ... but we haven't opened the file yet.
                    (values #f (make-dir-change-evt))]
                   [(file-exists? path)
                    ;; ... but our target file is intact.
                    (values log-p (make-dir-change-evt))]
                   [else
                    ;; ... and the file has been rotated, so open a new port.
                    (close-output-port/safe log-p)
                    (values (open-output-port) (make-dir-change-evt))])))
             (loop the-log-p the-dir-evt)))
          (handle-evt
           log-ch
           (lambda (req)
             (define the-log-p
               (with-handlers ([exn:fail?
                                 (lambda (e)
                                   ((error-display-handler) "dispatch-log.rkt Error writing log entry" e)
                                   (close-output-port/safe log-p)
                                   (loop #f dir-evt))])
                 (define the-log-p
                   (or log-p (open-output-port)))
                 (begin0 the-log-p
                   (display (format-req req) the-log-p))))
             (loop the-log-p dir-evt))))))))
  (lambda (req)
    (thread-resume log-thread (current-custodian))
    (channel-put log-ch req)))

(define (close-output-port/safe p)
  (when p
    (with-handlers ([exn:fail? void])
      (close-output-port p))))
