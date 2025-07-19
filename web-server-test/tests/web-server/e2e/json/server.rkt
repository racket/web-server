#lang racket/base

(require json
         racket/async-channel
         racket/port
         web-server/safety-limits
         web-server/servlet
         web-server/servlet-dispatch
         web-server/web-server)

(provide start)

(struct book (title author)
  #:transparent)

(define *all-books* (box null))

(define (response/json e #:code [code 200])
  (response/output
   #:code code
   #:mime-type #"application/json; charset=utf-8"
   (lambda (out)
     (write-json e out))))

(define (all-books _req)
  (response/json
   (for/list ([book (in-list (unbox *all-books*))])
     (hasheq 'title (book-title book)
             'author (book-author book)))))

(define (add-book req)
  (define data (read-json (open-input-bytes (request-post-data/raw req))))
  (define title (hash-ref data 'title #f))
  (define author (hash-ref data 'author #f))
  (cond
    [(and title author)
     (define the-book (book title author))
     (let loop ([old (unbox *all-books*)])
       (if (box-cas! *all-books* old (cons the-book old))
           (response/json (hasheq))
           (loop (unbox *all-books*))))]

    [else
     (response/json
      #:code 400
      (hasheq 'error "missing title or author (or both!)"))]))

(define-values (go _)
  (dispatch-rules
   [("books") #:method "post" add-book]
   [("books") all-books]))

(define (start)
  ;; One of the tests tries to send invalid JSON, which causes the
  ;; request handler to throw an exception, which would normally get
  ;; logged to stderr. This swallows that logging to avoid failing drdr.
  (parameterize ([current-error-port (open-output-nowhere)])
    (define confirmation-ch
      (make-async-channel))
    (define stop
      (serve
       #:port 0
       #:dispatch (dispatch/servlet go)
       #:confirmation-channel confirmation-ch
       #:safety-limits (make-safety-limits
                        #:max-request-body-length 255)))
    (define port-or-exn
      (sync confirmation-ch))
    (when (exn:fail? port-or-exn)
      (raise port-or-exn))
    (values stop port-or-exn)))
