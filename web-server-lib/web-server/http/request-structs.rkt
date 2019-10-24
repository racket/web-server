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

(define-serializable-struct (binding:file binding)
  (filename headers [content #:mutable])
  #:constructor-name -binding:file
  #:transparent)

(define-serializable-struct (binding:file/port binding:file)
  (in)
  #:constructor-name -binding:file/port
  #:transparent)

;; File uploads were changed in version 1.6 so that they can be
;; offloaded to a real file on disk if they are large enough. To
;; preserve backwards-compatibility with `binding:file', we added
;; the new `binding:file/port' struct and this contstructor.
;;
;; Every `binding:file/port' struct is backed by an input port and it
;; impersonates `binding:file' such that when `binding:file-content' is
;; called on the impersonator, the port content is read, memoized and
;; returned.
(define (make-binding:file/port id filename headers content-or-in)
  (impersonate-struct
   (-binding:file/port id filename headers
                       (and (bytes? content-or-in) content-or-in)
                       (if (bytes? content-or-in)
                           (open-input-bytes content-or-in)
                           content-or-in))
   struct:binding:file/port
   binding:file-content
   (let ([sem (make-semaphore 1)])
     (lambda (b c)
       (call-with-semaphore sem
         (lambda _
           (cond
             [c c]
             [else
              (define content (port->bytes (binding:file/port-in b)))
              (set-binding:file-content! b content)
              content])))))))

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
                                 [content bytes?])]
 [struct (binding:file/port binding:file) ([id bytes?]
                                           [filename bytes?]
                                           [headers (listof header?)]
                                           [content bytes?]
                                           [in input-port?])]
 [make-binding:file/port (-> bytes? bytes? (listof header?) input-port? binding:file/port?)]
 [rename make-binding:file/port
         make-binding:file
         (-> bytes? bytes? (listof header?) bytes? binding:file?)])

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
