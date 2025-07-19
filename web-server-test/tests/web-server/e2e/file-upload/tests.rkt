#lang racket/base

(require openssl/sha1
         racket/port
         racket/random
         racket/string
         racket/tcp
         rackunit)

(provide make-tests)

(define (make-tests get-port _get-stop)
  (define (upload-files . ins)
    (define-values (in out)
      (tcp-connect "127.0.0.1" (get-port)))

    (define boundary
      (sha1-bytes (open-input-bytes (crypto-random-bytes 32))))

    (with-handlers ([exn:fail:network? (lambda _
                                         (values 'reset null ""))])
      (parameterize ([current-output-port out])
        (printf "POST / HTTP/1.1\r\n")
        (printf "Content-Type: multipart/form-data; boundary=~a\r\n" boundary)
        (printf "Connection: close\r\n")
        (printf "\r\n")
        (for ([in (in-list ins)])
          (printf "--~a\r\n" boundary)
          (printf "Content-Disposition: application/octet-stream; filename=\"data\"; name=\"file\"\r\n")
          (printf "\r\n")
          (copy-port in out)
          (printf "\r\n"))
        (printf "--~a--\r\n" boundary))

      (close-output-port out)
      (values (read-line in 'return-linefeed)
              (let loop ([heads null])
                (define line (read-line in 'return-linefeed))
                (cond
                  [(eof-object? line) (reverse heads)]
                  [(string=? line "") (reverse heads)]
                  [else (loop (cons line heads))]))
              (port->string in))))

  (test-suite
   "file-upload"

   (let-values ([(status headers body) (upload-files (open-input-bytes #"hello"))])
     (test-equal?
      "one small file"
      body
      "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d\n"))

   (let-values ([(status headers body) (upload-files (open-input-bytes #"hello")
                                                     (open-input-bytes #"goodbye"))])
     (test-equal?
      "two small files"
      body
      (string-join (list "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"
                         "3c8ec4874488f6090a157b014ce3397ca8e06d4f"
                         "")
                   "\n")))

   (test-case "three small files"
     (let-values ([(status headers body) (upload-files (open-input-bytes #"hello")
                                                       (open-input-bytes #"hello")
                                                       (open-input-bytes #"goodbye"))])
       (check-true (eof-object? status))))

   (let-values ([(status headers body) (upload-files (open-input-bytes (make-bytes 350)))])
     (test-equal?
      "one large file"
      body
      "948905d56e776f1efa1e026b309c6669b089a2fa\n"))

   (test-case "one infinite file"
     (let-values ([(status headers body) (upload-files (make-input-port 'in
                                                                        (lambda (bs)
                                                                          (begin0 1
                                                                            (bytes-set! bs 0 65)))
                                                                        #f
                                                                        void))])
       (check-equal? status 'reset)))))
