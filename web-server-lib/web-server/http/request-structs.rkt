#lang racket/base

(require racket/contract
         racket/serialize
         racket/match
         racket/port
         racket/promise
         net/url-structs
         web-server/private/util)

(define-serializable-struct header
  (field value)
  #:transparent)

(define (headers-assq* f hs)
  (match hs
    [(list)
     #f]
    [(list-rest (and h (struct header (af aw))) hs)
     (if (bytes-ci=? af f)
         h
         (headers-assq* f hs))]))

(define (headers-assq f hs)
  (match hs
    [(list)
     #f]
    [(list-rest (and h (struct header (af av))) hs)
     (if (bytes=? af f)
         h
         (headers-assq f hs))]))

(provide/contract
 [headers-assq (bytes? (listof header?) . -> . (or/c false/c header?))]
 [headers-assq* (bytes? (listof header?) . -> . (or/c false/c header?))]
 [struct header ([field bytes?]
                 [value bytes?])])

(define-serializable-struct binding
  (id)
  #:transparent)

(define-serializable-struct (binding:form binding)
  (value)
  #:transparent)

(define-serializable-struct/versions (binding:file binding)
  1 (filename headers in)
  ([0
    (lambda (filename headers content)
      (binding:file filename headers (open-input-bytes content)))
    (lambda ()
      (define b0 (binding:file #f #f #f))
      (values b0
              (lambda (b)
                (set-binding:file-filename! b0 (binding:file-filename b))
                (set-binding:file-headers! b0 (binding:file-headers b))
                (set-binding:file-in! b0 (open-input-bytes (binding:file-headers b))))))])
  #:mutable
  #:transparent)

(define binding:file-content
  (let ([mem (make-weak-hasheq)]
        [sem (make-semaphore 1)])
    (lambda (b)
      (force
       (call-with-semaphore sem
         (lambda _
           (hash-ref! mem b (delay (port->bytes (binding:file-in b))))))))))

(define (bindings-assq ti bs)
  (match bs
    [(list)
     #f]
    [(list-rest (and b (struct binding (i))) bs)
     (if (equal? ti i)
         b
         (bindings-assq ti bs))]))

(define (bindings-assq-all ti bs)
  (for/list ([b (in-list bs)]
             #:when (and (binding? b) (equal? ti (binding-id b))))
    b))

(provide/contract
 [bindings-assq (bytes? (listof binding?) . -> . (or/c false/c binding?))]
 [bindings-assq-all (bytes? (listof binding?) . -> . (listof binding?))]
 [struct binding ([id bytes?])]
 [struct (binding:form binding) ([id bytes?]
                                 [value bytes?])]
 [struct (binding:file binding) ([id bytes?]
                                 [filename bytes?]
                                 [headers (listof header?)]
                                 [in input-port?])]
 [binding:file-content (-> binding:file? bytes?)])

(define-serializable-struct
  request
  (method uri headers/raw bindings/raw-promise post-data/raw
          host-ip host-port client-ip)
  #:transparent)

(define (request-bindings/raw r)
  (force (request-bindings/raw-promise r)))

(provide/contract
 [request-bindings/raw (request? . -> . (listof binding?))]
 [struct request ([method bytes?]
                  [uri url?]
                  [headers/raw (listof header?)]
                  [bindings/raw-promise (promise/c (listof binding?))]
                  [post-data/raw (or/c false/c bytes?)]
                  [host-ip string?] [host-port number?]
                  [client-ip string?])])
