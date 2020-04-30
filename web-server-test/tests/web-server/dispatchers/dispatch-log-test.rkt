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

(let* ([op (open-output-string)]
       [log-dispatcher (logger:make #:format 'apache-default
                                    #:log-path op)]
       [dispatcher (sequencer:make log-dispatcher
                                   do-nothing-dispatcher)]
       [req (request #"GET"
                     (string->url "whatever")
                     (list)
                     (delay (list))
                     #f
                     "localhost"
                     80
                     "nada")]
       [conn (fake-connection-for-bytes #"")])
  (check-equal? 'do-nothing (dispatcher conn req))

  (sleep 2) ; flaky delay needed to ensure that the log
            ; channel has a moment to process its input

  ; the [...] part of the regexp matches a time-dependent
  ; piece of the log data
  (check-true (regexp-match-exact? #px"nada - - \\[[^\\]].+\\] \"GET whatever HTTP/1.1\" - -\n"
                                   (bytes->string/utf-8 (get-output-bytes op)))))
