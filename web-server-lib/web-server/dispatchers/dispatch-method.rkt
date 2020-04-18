#lang racket/base

(require racket/contract
         web-server/dispatchers/dispatch
         web-server/http
         (only-in "../private/util.rkt"
                  bytes-ci=?))

(provide/contract
 [interface-version dispatcher-interface-version/c]
 [make (-> (or/c symbol? (cons/c symbol? (listof symbol?)))
           dispatcher/c
           dispatcher/c)])

(define interface-version 'v1)

; symbol? -> bytes?
;
; uppercase the (name of the) symbol to optimized for the
; normal case, where HTTP methods are given in uppercase
; ("GET", "HEAD", etc.)
(define (symbol->bytes symb)
  (string->bytes/utf-8 (string-upcase (symbol->string symb))))

; request? (listof bytes?) -> boolean?
(define (can-request-pass-through? req methods)
  (member (request-method req)
          methods
          bytes-ci=?))

(define (make methods inner)
  (define methods/bytes/list
    (if (symbol? methods)
        (list (symbol->bytes methods))
        (map symbol->bytes methods)))
  (lambda (conn req)
    (cond [(can-request-pass-through? req methods/bytes/list)
           (inner conn req)]
          [else (next-dispatcher)])))
