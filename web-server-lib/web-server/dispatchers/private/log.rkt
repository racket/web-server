#lang racket/base

(provide make-log-message
         make-format
         log-format/c)

(require racket/path
         racket/contract)

(define log-format/c (symbols 'parenthesized-default 'extended 'apache-default))

(define (make-log-message log-path-or-port formatter)
  (define path (if (output-port? log-path-or-port) #f log-path-or-port))
  (define dir-path (and path (simple-form-path (build-path path 'up))))
  (define (make-dir-change-evt)
    (if dir-path (filesystem-change-evt dir-path) never-evt))
  (define (open-output-port)
    (cond
      [path
       (define out (open-output-file path #:exists 'append))
       (begin0 out
         (file-stream-buffer-mode out 'line))]
      [else
       log-path-or-port]))
  (define log-ch (make-channel))
  (define log-thread
    (thread/suspend-to-kill
     (lambda ()
       (let loop ([log-p #f]
                  [dir-evt (make-dir-change-evt)])
         (sync
          (handle-evt
           dir-evt
           (lambda (_)
             (define-values (the-log-p the-dir-evt)
               (with-handlers ([exn:fail?
                                (lambda (e)
                                  ((error-display-handler) "Error watching filesystem" e)
                                  (close-output-port/safe log-p)
                                  (values #f never-evt))])
                 ;; Something in the directory changed ...
                 (cond
                   [(not log-p)
                    ;; ... but we haven't opened the file yet.
                    (values #f (make-dir-change-evt))]
                   [(file-exists? path)
                    ;; ... but our target file is intact.
                    (values log-p (make-dir-change-evt))]
                   [else
                    ;; ... and the file has been rotated, so open a new port.
                    (close-output-port/safe log-p)
                    (values (open-output-port) (make-dir-change-evt))])))
             (loop the-log-p the-dir-evt)))
          (handle-evt
           log-ch
           (lambda (args)
             (define the-log-p
               (with-handlers ([exn:fail?
                                 (lambda (e)
                                   ((error-display-handler) "Error writing log entry" e)
                                   (close-output-port/safe log-p)
                                   (loop #f dir-evt))])
                 (define the-log-p
                   (or log-p (open-output-port)))
                 (begin0 the-log-p
                   (display (apply formatter args) the-log-p))))
             (loop the-log-p dir-evt))))))))
  (lambda args
    (thread-resume log-thread (current-custodian))
    (channel-put log-ch args)))

(define (close-output-port/safe p)
  (when p
    (with-handlers ([exn:fail? void])
      (close-output-port p))))

(define (make-format fmt proc)
  (define mask (procedure-arity-mask proc))
  (procedure-reduce-arity-mask
   (λ args (apply format fmt (apply proc args)))
   mask))
