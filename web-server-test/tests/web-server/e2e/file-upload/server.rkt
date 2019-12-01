#lang racket/base

(require openssl/sha1
         racket/port
         web-server/servlet
         web-server/servlet-dispatch
         web-server/safety-limits
         web-server/web-server)

(provide start)

(define (file-upload req)
  (define fs (bindings-assq-all #"file" (request-bindings/raw req)))
  (define hashes (for/list ([f (in-list fs)])
                   (sha1 (binding:file/port-in f))))
  (response/output
   (lambda (out)
     (for ([h (in-list hashes)])
       (displayln h out)))))

(define (start port)
  ;; We're testing file limits and those end up raising exceptions in
  ;; the request-handling threads which get reported to stderr so we
  ;; need to drop those messages in order for drdr not to fail.
  (parameterize ([current-error-port (open-output-nowhere)])
    (serve
     #:port port
     #:dispatch (dispatch/servlet file-upload)
     #:safety-limits (make-safety-limits
                      #:max-form-data-files 2
                      #:max-form-data-file-length 500
                      #:form-data-file-memory-threshold 250))))
