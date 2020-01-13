#lang racket/base

(require racket/contract
         racket/serialize
         racket/match
         racket/port
         racket/promise
         net/url-structs
         web-server/private/util
         (for-syntax racket/base
                     racket/struct-info
                     racket/match
                     syntax/transformer))

;; Most serializable structs are protected
;; with struct-guard/c rather than contract-out.

(provide
 ;; header
 (struct-out header)
 (contract-out
  [headers-assq
   (-> bytes? (listof header?) (or/c #f header?))]
  [headers-assq*
   (-> bytes? (listof header?) (or/c #f header?))])
 ;; ----------------------------------------
 ;; request
 (contract-out
  [struct request ([method bytes?]
                   [uri url?]
                   [headers/raw (listof header?)]
                   [bindings/raw-promise (promise/c (listof binding?))]
                   [post-data/raw (or/c #f bytes?)]
                   [host-ip string?]
                   [host-port number?]
                   [client-ip string?])]
  [request-bindings/raw
   (-> request? (listof binding?))])
 ;; ----------------------------------------
 ;; binding
 (struct-out binding)
 (struct-out binding:form)
 binding:file/port?
 (contract-out
  [struct (binding:file binding)
    ;; enforce the `binding:file-content` contract to be extra careful
    ([id bytes?]
     [filename bytes?]
     [headers (listof header?)]
     [content bytes?])]
  [binding:file/port-in
   (-> binding:file/port? input-port?)]
  [make-binding:file/port
   (-> bytes? bytes? (listof header?) input-port? binding:file/port?)]
  [bindings-assq
   (-> bytes? (listof binding?) (or/c #f binding?))]
  [bindings-assq-all
   (-> bytes? (listof binding?) (listof binding?))]))

;                                        
;   ;;                      ;;           
;   ;;                      ;;           
;   ;;;;;   ;;    ;;     ;;;;;  ;;   ;; ;
;   ;;  ;  ;  ;  ;  ;   ;   ;; ;  ;  ;;; 
;   ;;  ;; ;  ;     ;;  ;   ;; ;  ;  ;;  
;   ;;  ;;;;;;;;  ;;;; ;;   ;;;;;;;; ;;  
;   ;;  ;; ;     ;  ;;  ;   ;; ;     ;;  
;   ;;  ;; ;    ;;  ;;  ;   ;; ;     ;;  
;   ;;  ;;  ;;;  ;;; ;   ;;; ;  ;;;  ;;  
;                                        

(define-serializable-struct header
  (field value)
  #:guard (struct-guard/c bytes? bytes?)
  #:transparent)

(define (headers-assq* f hs)
  (for/first ([h (in-list hs)]
              #:when (bytes-ci=? f (header-field h)))
    h))

(define (headers-assq f hs)
  (for/first ([h (in-list hs)]
              #:when (bytes=? f (header-field h)))
    h))

;                                          
;                                       ;; 
;                                       ;; 
;   ;; ;  ;;    ;;; ; ;; ;;   ;;    ;; ;;;;
;   ;;;  ;  ;  ;   ;; ;; ;;  ;  ; ;;  ; ;; 
;   ;;   ;  ;  ;   ;; ;; ;;  ;  ;  ;    ;; 
;   ;;  ;;;;;;;;   ;; ;; ;; ;;;;;;  ;;  ;; 
;   ;;   ;     ;   ;; ;; ;;  ;        ;;;; 
;   ;;   ;     ;   ;;  ; ;;  ;    ;   ;  ; 
;   ;;    ;;;   ;;;;;  ;;;;   ;;;  ;;;   ;;
;                  ;;                      
;                  ;;                      
;                  ;;                      
;                                          

(define-struct request
  (method uri headers/raw bindings/raw-promise post-data/raw
          host-ip host-port client-ip)
  #:transparent)

(define (request-bindings/raw r)
  (force (request-bindings/raw-promise r)))


;                                           
;                                           
;   ;;     ;;            ;; ;;              
;   ;;                   ;;                 
;   ;;;;   ;; ; ;;;   ;;;;; ;; ; ;;;   ;;;;;
;   ;;  ;  ;; ;;  ;  ;   ;; ;; ;;  ;  ;  ;  
;   ;;  ;  ;; ;;  ;; ;   ;; ;; ;;  ;;;;  ;; 
;   ;;  ;; ;; ;;  ;;;;   ;; ;; ;;  ;; ;  ;  
;   ;;  ;  ;; ;;  ;; ;   ;; ;; ;;  ;;  ;;   
;   ;;  ;  ;; ;;  ;; ;   ;; ;; ;;  ;;;;     
;   ; ;;   ;; ;;  ;;  ;;; ; ;; ;;  ;; ;;;;; 
;                                     ;   ;;
;                                    ;;   ; 
;                                      ;;;  
;                                           

(define-serializable-struct binding
  (id)
  #:guard (struct-guard/c bytes?)
  #:transparent)

(define (bindings-assq ti bs)
  (for/first ([b (in-list bs)]
              #:when (equal? ti (binding-id b)))
    b))

(define (bindings-assq-all ti bs)
  (for/list ([b (in-list bs)]
             #:when (equal? ti (binding-id b)))
    b))

(define-serializable-struct (binding:form binding)
  (value)
  #:guard (struct-guard/c bytes? bytes?)
  #:transparent)

;; The implementation of `binding:file` is complicated
;; to support the change in web-server-lib v1.6 to
;; offload large files to disk while preserving backwards-compatability.
;; In particular, `binding:file` was originally declared as
;; `#:transparent` and serializable (via `define-serializable-struct`).
;; Changing its nature as a struct would break `typed/web-server/http`.
;;
;; Our basic approach (suggested by Sam T-H:
;; <https://github.com/racket/web-server/pull/77#discussion_r338045221>)
;; is to construct new-style instances using a function `make-binding:file/port`.
;; This function returns an impersonator, and the wrapper for
;; for `binding:file-contents` access reads the port to a byte-string
;; and memoized it.
;;
;; The parent `binding:file` type gets particularly complex
;; because we need to keep the raw constructors and the mutator private
;; to this module, even in the face of reflective operations.
;; The details are explained in comments below.
;;
;; Another design consideration is the the port-based API is stateful,
;; whereas the bytes-based API is purely functional.
;; If a user program uses the port-based API, it must take responsibility for
;; its stateful nature. See docs for more details.


;; First, we create the parent type.
;; Doing this inside a `let` turns out to be the easiest way to get the
;; right names visible in the rest of this module,
;; and hapily also avoids defining names we don't want to accidently use.
(define-values [struct:binding:file/raw
                binding:file?
                binding:file-filename
                binding:file-headers
                binding:file-content
                set-binding:file-content!]
  (let ()
    (struct binding:file binding
      (filename headers [content #:mutable])
      #:property prop:serializable
      (make-serialize-info
       (match-lambda
         [(binding:file id filename headers content)
          (vector id filename headers content)])
       #'deserialize-info:binding:file-v0
       #f
       (or (current-load-relative-directory) (current-directory)))
      #:transparent)
    (values struct:binding:file
            binding:file?
            binding:file-filename
            binding:file-headers
            binding:file-content
            set-binding:file-content!)))

;; We chaperone `struct:binding:file/raw` to define the provided struct type.
(define struct:binding:file
  (chaperone-struct-type
   struct:binding:file/raw
   ;; Wrap struct-info to block use of the mutator:
   (λ (name init-field-cnt auto-field-cnt accessor-proc mutator-proc
            immutable-k-list super-type skipped?)
     (values name init-field-cnt auto-field-cnt accessor-proc
             (chaperone-procedure
              mutator-proc
              (λ (instance index new-value)
                (raise-arguments-error
                 'binding:file
                 "mutation not allowed\n illegal attempt to use mutator from struct-info"
                 "given" instance
                 "index" index
                 "value" new-value)))
             immutable-k-list super-type skipped?))
   ;; Wrap the constructor to check field contracts,
   ;; plus chaperone the resulting instance to block `set-binding:file-content!`
   ;; and make `struct-info` return this chaperoned struct type.
   (λ (ctor)
     (chaperone-procedure
      ctor
      (λ (id filename headers content)
        (define (bad-arg! pos [expected "bytes?"])
          (raise-argument-error
           'make-binding:file expected pos id filename headers content))
        (unless (bytes? id)
          (bad-arg! 0))
        (unless (bytes? filename)
          (bad-arg! 1))
        (unless (and (list? headers) (andmap header? headers))
          (bad-arg! 2 "(listof header?)"))
        (unless (bytes? content)
          (bad-arg! 3))
        (values (λ (instance)
                  (chaperone-struct
                   instance
                   struct:binding:file/raw
                   struct-info
                   (λ (type skipped?)
                     (values struct:binding:file skipped?))
                   set-binding:file-content!
                   (λ (self new-val)
                     (raise-arguments-error
                      'set-binding:file-content!
                      "mutation not allowed\n illegal attempt to mutate an immediate instance"
                      "instance" self
                      "value" new-val))))
                id filename headers content))))
   ;; It looks like we can't protect reflective information
   ;; for instances of subtypes, so we'll just ban instantiating subtypes for now.
   (λ (id filename headers content subtype-name)
     (raise-arguments-error
      'binging:file
      (string-append "attempt to construct an instance of a subtype;\n"
                     " subtyping binging:file is not allowed")
      "subtype name" subtype-name
      "id" id
      "filename" filename
      "headers" headers
      "content" content))))

;; Define a constructor using the chaperoned `struct:binding:file`:
(define make-binding:file
  ;; workaround for https://github.com/racket/racket/issues/2905
  ;; and https://github.com/racket/racket/issues/2924
  (if (eqv? 1 (procedure-arity struct-type-make-constructor))
      (struct-type-make-constructor struct:binding:file)
      (struct-type-make-constructor struct:binding:file
                                    'make-binding:file)))

;; Use the checked constructor for the deserialize info:
(define deserialize-info:binding:file-v0
  (make-deserialize-info
   make-binding:file
   (λ ()
     (error 'binding:file "cycles not allowed"))))

(module+ deserialize-info
  (provide deserialize-info:binding:file-v0))

;; At last, define `binding:file` to work as both static
;; struct information and the checked constructor:
(define-syntax binding:file
  (let ([info-list
         (list #'struct:binding:file
               #'make-binding:file
               #'binding:file?
               (list #'binding:file-content
                     #'binding:file-headers
                     #'binding:file-filename
                     #'binding-id)
               '(#f #f #f #f)
               #'binding)])
    (struct info (transformer)
      #:super struct:struct-info
      #:property prop:procedure (struct-field-index transformer)
      #:property prop:struct-auto-info (λ (__) '(() ())))
    (info (λ () info-list)
          (make-variable-like-transformer #'make-binding:file))))

;; The definition of `binding:file/port` is relatively normal,
;; except that we don't provide the standard constructor.

(struct binding:file/port (in sema)
  #:super struct:binding:file/raw ;; use raw to avoid subtyping & mutation bans
  #:constructor-name binding:file/port/raw)

(define make-binding:file/port
  (let ()
    (define-values [imp-prop:raw has-raw? get-raw]
      ;; store the instance w/o impersonator
      (make-impersonator-property 'impersonator-prop:raw-this))
    (define (make-binding:file/port id filename headers in)
      (define raw
        (binding:file/port/raw id filename headers #f in (make-semaphore 1)))
      (impersonate-struct
       (chaperone-struct raw
                         struct:binding:file/port
                         struct-info
                         (λ (type skipped?)
                           ;; Expose only the chaperoned struct:binding:file
                           ;; (works because binding:file/port is opaque).
                           (values struct:binding:file skipped?)))
       struct:binding:file/port
       binding:file-content
       (λ (this content)
         ;; avoid allocating any closures
         (or content
             (call-with-semaphore (binding:file/port-sema this)
               (λ (this)
                 (cond
                   ;; Could have been used in some other thread.
                   ;; Need to check w/o impersonator, or we diverge.
                   [(binding:file-content (get-raw this))]
                   [else
                    ;; will raise an exn if the port was closed
                    ;; (perhaps concurrently, by user code)
                    (define content
                      (port->bytes (binding:file/port-in this)))
                    (set-binding:file-content! this content)
                    (close-input-port in)
                    content]))
               #f
               this)))
       imp-prop:raw
       raw))
    make-binding:file/port))

