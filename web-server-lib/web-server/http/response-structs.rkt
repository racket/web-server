#lang racket/base
(require racket/contract 
         web-server/http/request-structs)

(define TEXT/HTML-MIME-TYPE #"text/html; charset=utf-8")

(struct response (code message seconds mime headers output))

(define (response/full code message seconds mime headers body)
  (response code message seconds mime
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
                         #:message [message #"Okay"]
                         #:seconds [seconds (current-seconds)]
                         #:mime-type [mime-type TEXT/HTML-MIME-TYPE]
                         #:headers [headers '()])
  (response code message seconds mime-type headers
            output))

(define/final-prop response-code/c
  (integer-in 100 999))

(provide response-code/c)
(provide/contract
 [struct response
         ([code response-code/c]
          [message bytes?]
          [seconds real?]
          [mime (or/c #f bytes?)]
          [headers (listof header?)]
          [output (output-port? . -> . any)])]
 [response/full (-> response-code/c bytes? real? (or/c #f bytes?) (listof header?) (listof bytes?) response?)]
 [response/output (->* ((-> output-port? any))
                       (#:code response-code/c
                        #:message bytes?
                        #:seconds real?
                        #:mime-type (or/c bytes? #f)
                        #:headers (listof header?))
                       response?)]
 [TEXT/HTML-MIME-TYPE bytes?])
