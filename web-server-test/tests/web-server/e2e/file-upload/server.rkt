#lang racket/base

(require openssl/sha1
         racket/async-channel
         racket/port
         web-server/safety-limits
         web-server/servlet
         web-server/servlet-dispatch
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

(define (start)
  ;; We're testing file limits and those end up raising exceptions in
  ;; the request-handling threads which get reported to stderr so we
  ;; need to drop those messages in order for drdr not to fail.
  (parameterize ([current-error-port (open-output-nowhere)])
    (define confirmation-ch
      (make-async-channel))
    (define stop
      (serve
       #:port 0
       #:dispatch (dispatch/servlet file-upload)
       #:confirmation-channel confirmation-ch
       #:safety-limits (make-safety-limits
                        #:max-form-data-files 2
                        #:max-form-data-file-length 500
                        #:form-data-file-memory-threshold 250)))
    (define port-or-exn
      (sync confirmation-ch))
    (when (exn:fail? port-or-exn)
      (raise port-or-exn))
    (values stop port-or-exn)))
