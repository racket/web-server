#lang racket/base
(require rackunit
         (only-in mzlib/file
                  make-temporary-file)
         racket/port
         racket/promise
         net/url
         mzlib/list
         web-server/http
         web-server/dispatchers/dispatch
         (prefix-in logger: web-server/dispatchers/dispatch-log)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         "../util.rkt")

(let ([bad-logger 'this-should-fail])
  (check-exn exn:fail:contract?
             (lambda ()
               (logger:make #:format bad-logger
                            #:log-path (open-output-nowhere)))))

; connection? request? -> symbol?
(define (do-nothing-dispatcher conn req)
  'do-nothing)

(let ()
  (define-values (ip op) (make-pipe))
  (define log-dispatcher
    (logger:make #:format 'apache-default
                 #:log-path op))
  (define dispatcher
    (sequencer:make log-dispatcher
                    do-nothing-dispatcher))
  (define req
    (request #"GET"
             (string->url "whatever")
             (list)
             (delay (list))
             #f
             "localhost"
             80
             "nada"))
  (define conn (fake-connection-for-bytes #""))
  (check-equal? 'do-nothing (dispatcher conn req))
  (define log-entry (read-line ip))
  ;; the [...] part of the regexp matches a time-dependent piece of
  ;; the log data
  (check-true (regexp-match-exact? #px"nada - - \\[[^\\]].+\\] \"GET whatever HTTP/1.1\" - -"
                                   log-entry)))
