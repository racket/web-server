#lang racket/base
(require racket/contract
         racket/match
         web-server/http/request-structs
         "status-code.rkt")

(module+ test
  (require rackunit))

(define TEXT/HTML-MIME-TYPE #"text/html; charset=utf-8")

(struct response (code message seconds mime headers output))

(define (response/full code message seconds mime headers body)
  (response code
            (infer-response-message code message)
            seconds
            mime
            (list* (make-header #"Content-Length"
                                (string->bytes/utf-8
                                 (number->string
                                  (for/fold ([len 0])
                                    ([b (in-list body)])
                                    (+ len (bytes-length b))))))
                   headers)
            (lambda (op)
              (for ([b (in-list body)])
                (write-bytes b op)))))

(define (response/output output
                         #:code [code 200]
                         #:message [message #f]
                         #:seconds [seconds (current-seconds)]
                         #:mime-type [mime-type TEXT/HTML-MIME-TYPE]
                         #:headers [headers '()])
  (response code
            (infer-response-message code message)
            seconds
            mime-type
            headers
            output))

(module+ test
  (let ([output (lambda (op) void)])
    ;; check message as bytes
    (let [(resp (response/output output
                                 #:code 123
                                 #:message #"bites!"))]
      (check-equal? (response-code resp) 123)
      (check-equal? (response-message resp) #"bites!"))
    ;; check message as #f
    (let [(resp (response/output output
                                 #:code 200
                                 #:message #f))]
      (check-equal? (response-code resp) 200)
      (check-equal? (response-message resp) #"OK"))
    ;; check message not supplied, but code supplied
    (let [(resp (response/output output
                                 #:code 200))]
      (check-equal? (response-code resp) 200)
      (check-equal? (response-message resp) #"OK"))
    ;; check code not supplied, message supplied
    (let [(resp (response/output output
                                 #:message #"bite this"))]
      (check-equal? (response-code resp) 200)
      (check-equal? (response-message resp) #"bite this"))
    ;; check neither message nor code supplied
    (let [(resp (response/output output))]
      (check-equal? (response-code resp) 200)
      (check-equal? (response-message resp) #"OK"))
    ;; check non-standard status code
    (let [(resp (response/output output #:code 123))]
      (check-equal? (response-code resp) 123)
      (check-equal? (response-message resp) #"OK"))))

(provide/contract
 [struct response
         ([code number?]
          [message bytes?]
          [seconds number?]
          [mime (or/c false/c bytes?)]
          [headers (listof header?)]
          [output (output-port? . -> . any)])]
 [response/full (-> number? (or/c false/c bytes?) number? (or/c false/c bytes?) (listof header?) (listof bytes?) response?)]
 [response/output (->* ((-> output-port? any))
                       (#:code number?
                        #:message (or/c false/c bytes?)
                        #:seconds number?
                        #:mime-type (or/c bytes? #f)
                        #:headers (listof header?))
                       response?)]
 [TEXT/HTML-MIME-TYPE bytes?])
