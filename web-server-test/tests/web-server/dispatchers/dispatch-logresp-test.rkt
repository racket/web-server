#lang racket/base
(require rackunit
         racket/port
         racket/promise
         net/url
         web-server/http
         (prefix-in logger: web-server/dispatchers/dispatch-logresp)
         (prefix-in logger*: web-server/dispatchers/dispatch-log)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         "../util.rkt")

(let ([bad-logger 'this-should-fail])
  (check-exn exn:fail:contract?
             (lambda ()
               (logger:make #:format bad-logger
                            #:log-path (open-output-nowhere)))))

(let ()
  (define-values (ip op) (make-pipe))

  (define (make-dispatcher formatter code)
    (logger:make #:format formatter
                 #:log-path op
                 (lift:make
                  (λ (req)
                    (response/xexpr #:code code
                                    '(hello world))))))

  (define dispatcher/req+resp/200
    (make-dispatcher (logger:log-format->format 'apache-default)
                     200))

  (define dispatcher/req+resp/404
    (make-dispatcher (logger:log-format->format 'apache-default)
                     404))

  (define dispatcher/req/200
    (make-dispatcher (logger*:log-format->format 'apache-default)
                     200))

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

  (test-case "req+resp 200"
    (dispatcher/req+resp/200 conn req)
    ;; the [...] part of the regexp matches a time-dependent piece of
    ;; the log data
    (check-regexp-match #px"^nada - - \\[[^\\]].+\\] \"GET whatever HTTP/1.1\" 200 -$"
                        (read-line ip)))

  (test-case "req+resp 404"
    (dispatcher/req+resp/404 conn req)
    (check-regexp-match #px"^nada - - \\[[^\\]].+\\] \"GET whatever HTTP/1.1\" 404 -$"
                        (read-line ip)))

  (test-case "req 200"
    (dispatcher/req/200 conn req)
    (check-regexp-match #px"^nada - - \\[[^\\]].+\\] \"GET whatever HTTP/1.1\" - -$"
                        (read-line ip))))
