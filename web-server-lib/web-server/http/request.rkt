#lang racket/base

(require net/uri-codec
         net/url
         racket/contract
         racket/file
         racket/list
         racket/match
         racket/port
         racket/promise
         web-server/http/request-structs
         web-server/private/connection-manager
         web-server/private/util)

;;
;;                    **READ ME FIRST**
;;                    =================
;;
;; Before you edit anything in this file, keep in mind that some clients
;; are malicious, therefore all clients must be treated as such and
;; their input MUST NOT be trusted. In that vein, avoid procedures such
;; as `read-{,bytes-}line' and `read-bytes' because they allocate memory
;; up front based on input from the client. Always consume client input
;; by buffering it in (relatively) small chunks.
;;
;; Happy hacking!
;;

(define read-request/c
  (connection?
   listen-port-number?
   (input-port? . -> . (values string? string?))
   . -> .
   (values request? boolean?)))

(provide/contract
 [rename make-ext:read-request
         make-read-request
         (->* ()
              (#:connection-close? boolean?
               #:read-timeout number?
               #:max-request-line-length exact-positive-integer?
               #:max-request-fields exact-positive-integer?
               #:max-request-field-length exact-positive-integer?
               #:max-request-body-length exact-positive-integer?
               #:max-request-files exact-positive-integer?
               #:max-request-file-length exact-positive-integer?
               #:max-request-file-memory-threshold exact-positive-integer?)
              read-request/c)]
 [rename ext:read-request
         read-request
         read-request/c])


;; **************************************************
;; read-request: connection number (input-port -> string string) -> request boolean?
;; read the request line, and the headers, determine if the connection should
;; be closed after servicing the request and build a request structure
(define ((make-read-request
          #:connection-close? [connection-close? #f]
          #:read-timeout [read-timeout 60]
          #:max-request-line-length [max-request-line-length (* 8 1024)]
          #:max-request-fields [max-request-fields 100]
          #:max-request-field-length [max-request-field-length (* 8 1024)]
          #:max-request-body-length [max-request-body-length (* 1 1024 1024)]
          #:max-request-files [max-request-files 100]
          #:max-request-file-length [max-request-file-length (* 10 1024 1024)]
          #:max-request-file-memory-threshold [max-request-file-memory-threshold (* 1 1024 1024)])
         conn host-port port-addresses)

  (reset-connection-timeout! conn read-timeout)
  (define ip (connection-i-port conn))
  (define-values (method uri major minor)
    (read-request-line ip max-request-line-length))
  (define initial-headers
    (read-headers ip
                  max-request-fields
                  max-request-field-length))
  (define-values (data-ip headers)
    (complete-request ip
                      initial-headers
                      max-request-fields
                      max-request-field-length
                      max-request-body-length))
  (define-values (host-ip client-ip)
    (port-addresses ip))
  (define-values (bindings/raw-promise raw-post-data)
    (read-bindings&post-data/raw data-ip method uri headers
                                 #:max-body-length max-request-body-length
                                 #:multipart-reader (lambda (in boundary)
                                                      (read-mime-multipart in boundary
                                                                           #:max-files max-request-files
                                                                           #:max-file-length max-request-file-length
                                                                           #:max-file-memory-threshold max-request-file-memory-threshold
                                                                           #:max-field-length max-request-field-length))))
  (define request
    (make-request method uri headers bindings/raw-promise raw-post-data
                  host-ip host-port client-ip))
  (define close?
    (or connection-close?
        (close-connection? headers major minor
                           client-ip host-ip)))
  (values request close?))

(define make-ext:read-request
  (make-keyword-procedure
   (lambda (kws kw-args . args)
     (define read-request
       (keyword-apply make-read-request kws kw-args args))

     (lambda (conn host-port port-addresses)
       (with-handlers ([exn:fail?
                        (lambda (exn)
                          (kill-connection! conn)
                          (raise exn))])
         (read-request conn host-port port-addresses))))))

(define ext:read-request
  (make-ext:read-request))


;; **************************************************
;; complete-request

(define (hex-string->number s)
  (string->number s 16))

; complete-request: inp (listof header) number number number -> inp (listof header)
; if the request contains chunked body data, then decode that data
; and finish processing the body and remaining headers.
(define (complete-request real-ip initial-headers max-headers max-header-length max-body-length)
  (match (headers-assq* #"Transfer-Encoding" initial-headers)
    [(struct header (f #"chunked"))
     (define max-remaining-headers
       (max 0 (- max-headers (length initial-headers))))

     (define-values (decoded-ip decode-op)
       (make-pipe))

     (define total-size
       (let loop ([total-size 0])
         (define size-line (read-http-line/limited real-ip max-header-length))
         (define size-in-bytes
           (match (regexp-split #rx";" size-line)
             [(cons size-in-hex _)
              (hex-string->number (bytes->string/utf-8 size-in-hex))]))

         (cond
           [(zero? size-in-bytes) total-size]
           [else
            (define new-size (+ total-size size-in-bytes))
            (when (> new-size max-body-length)
              (network-error 'complete-request "chunked content exceeds max body length"))

            ;; This is safe because of the preceding guard on new-size,
            (define limited-input (make-limited-input-port real-ip size-in-bytes #f))
            (copy-port limited-input decode-op)
            (read-http-line/limited real-ip 2)
            (loop new-size)])))

     (define more-headers
       (list* (header #"Content-Length" (string->bytes/utf-8 (number->string total-size)))
              (read-headers real-ip max-remaining-headers max-header-length)))

     (close-output-port decode-op)
     (values decoded-ip (append initial-headers more-headers))]

    [_
     (values real-ip initial-headers)]))


;; **************************************************
;; close-connection?

; close-connection? : (listof (cons symbol bytes)) number number string string -> boolean
; determine if this connection should be closed after serving the
; response
(define close-connection?
  (let ([rx (byte-regexp #"[cC][lL][oO][sS][eE]")])
    (lambda (headers major minor client-ip host-ip)
      (or (< major 1)
          (and (= major 1) (= minor 0))
          (match (headers-assq* #"Connection" headers)
            [(struct header (f v)) (regexp-match? rx v)]
            [#f #f])))))


;; **************************************************
;; safe reading

(define current-http-line-limit
  (make-parameter (* 8 1024)))

(define (CR? b) (eqv? b 13))
(define (LF? b) (eqv? b 10))

(define (bytes-find-crlf bs len)
  (for/first ([i (in-range 0 (sub1 len))]
              #:when (and (CR? (bytes-ref bs i))
                          (LF? (bytes-ref bs (add1 i)))))
    i))

; read-http-line/limited : inp number -> bytes
; `read-bytes-line' against untrusted input is not safe since the client
; could just feed us bytes until we run out of memory. This function
; will attempt to read a line from the input port up to a hard limit.
(define (read-http-line/limited [in (current-input-port)]
                                [limit (current-http-line-limit)]
                                [bufsize 128])
  (define buf (make-bytes bufsize))
  (define-values (line-len suffix-len)
    (let loop ([offset 0]
               [boundary-CR? #f])
      (define len
        (peek-bytes-avail! buf offset #f in))

      (cond
        ;; the input port is depleted
        [(eof-object? len)
         (values offset 0)]

        ;; the previous chunk had a CR right at the end and
        ;; the current chunk has a LF right at the start
        [(and boundary-CR? (LF? (bytes-ref buf 0)))
         (values (sub1 offset) 2)]

        ;; the current chunk has a CRLF somewhere within it
        [(bytes-find-crlf buf len)
         => (lambda (pos)
              (values (+ offset pos) 2))]

        ;; we've read past what we're willing to accept so bail
        [(> offset limit)
         (values offset 0)]

        ;; rinse and repeat
        [else
         (loop (+ offset len)
               (CR? (bytes-ref buf (sub1 len))))])))

  (cond
    ;; preserve the behaviour of (read-bytes-line (open-input-string #""))
    [(and (zero? line-len)
          (zero? suffix-len))
     eof]

    [(> line-len limit)
     (network-error 'read-http-line/limited "line exceeds limit of ~a" limit)]

    [else
     (begin0 (read-bytes line-len in)
       (unless (zero? suffix-len)
         (read-bytes suffix-len in)))]))

(module+ internal-test
  (provide read-http-line/limited))

; read-bytes/lazy : number inp -> bytes
; Like `read-bytes', but waits until the expected number of bytes is
; available within the input port before allocating the final buffer.
(define (read-bytes/lazy n in [bufsize 4096])
  (define buf (make-bytes (min n bufsize)))
  (define offset
    (let loop ([offset 0])
      (define len
        (peek-bytes-avail! buf offset #f in))

      (cond
        [(eof-object? len) offset]
        [else
         (define offset* (+ offset len))
         (cond
           [(>= offset* n) offset*]
           [else (loop offset*)])])))

  (cond
    [(zero? offset) eof]
    [else (read-bytes (min offset n) in)]))

(module+ internal-test
  (provide read-bytes/lazy))


;; **************************************************
;; read-request-line
(define match-method
  (let ([rx (byte-regexp #"^([^ ]+) (.+) HTTP/([0-9]+)\\.([0-9]+)$")])
    (lambda (a) (regexp-match rx a))))

; read-request-line : iport number -> bytes url number number
; to read in the first line of an http request, AKA the "request line"
; effect: in case of errors, complain [MF: where] and close the ports
(define (read-request-line ip [max-length 1024])
  (define line (read-http-line/limited ip max-length))
  (if (eof-object? line)
      (network-error 'read-request "http input closed prematurely")
      (match (match-method line)
        [#f (network-error 'read-request "malformed request ~e" line)]
        [(list _ method url major minor)
         (values method
                 (string->url (bytes->string/utf-8 url))
                 (string->number (bytes->string/utf-8 major))
                 (string->number (bytes->string/utf-8 minor)))])))

(module+ internal-test
  (provide read-request-line))


;; **************************************************
;; read-headers
(define match-colon
  (let ([rx (byte-regexp (bytes-append #"^([^:]*):[ " (bytes 9) #"]*(.*)"))])
    (lambda (a) (regexp-match rx a))))

; read-headers : iport number number -> (listof header?)
(define (read-headers in max-heads max-head-length)
  (parameterize ([current-http-line-limit max-head-length])
    (reverse
     (let loop ([heads null])
       (when (> (length heads) max-heads)
         (network-error 'read-headers "header count exceeds limit of ~a" max-heads))

       (define l (read-http-line/limited in))
       (cond
         [(eof-object? l) heads]
         [(zero? (bytes-length l)) heads]
         [(match-colon l)
          => (match-lambda
               [(list _ field value)
                (define head (make-header field (read-folded-head in value max-head-length)))
                (loop (cons head heads))])]
         [else (network-error 'read-headers "malformed header: ~e" l)])))))

(module+ internal-test
  (provide read-headers))

; read-folded-head : iport bytes number -> bytes
; reads the next line of input for headers that are line-folded
(define (read-folded-head in rhs max-length)
  (match (peek-byte in)
    ;; leading SPACE or TAB
    [(or 32 9)
     (define line (read-http-line/limited in))
     (define rhs* (bytes-append rhs line))
     (when (> (bytes-length rhs*) max-length)
       (network-error 'read-headers "header too long (~a)" max-length))
     (read-folded-head in rhs* max-length)]
    [_ rhs]))


;; **************************************************
;; read-bindings
(define match-urlencoded
  (let ([rx (byte-regexp #"application/x-www-form-urlencoded")])
    (lambda (t)
      (and t (regexp-match rx t)))))

(define match-multipart
  (let ([rx (byte-regexp #"multipart/form-data; *boundary=(.*)")])
    (lambda (t)
      (and t (regexp-match rx t)))))

(define (read-bindings&post-data/raw in meth uri headers
                                     #:max-body-length [max-body-length +inf.0]
                                     #:multipart-reader [multipart-reader (lambda _
                                                                            (raise-user-error 'read-request "multipart reader not set up"))])
  (define bindings-GET
    (delay
      (filter-map
       (match-lambda
         [(list-rest k v)
          (and (symbol? k)
               (string? v)
               (make-binding:form (string->bytes/utf-8 (symbol->string k))
                                  (string->bytes/utf-8 v)))])
       (url-query uri))))

  (define (read-data who proc)
    (match (headers-assq* #"Content-Length" headers)
      [(struct header (_ value))
       (cond
         [(string->number (bytes->string/utf-8 value))
          => (lambda (len)
               (when (> len max-body-length)
                 (network-error 'read-bindings "body length exceeds limit"))

               ;; this is safe because of the preceding guard on the length.
               (define data (read-bytes/lazy len in))
               (cond
                 [(or (eof-object? data)
                      (< (bytes-length data) len))
                  (network-error
                   'read-bindings
                   "port closed prematurely")]

                 [else (proc data)]))]
         [else
          (network-error
           'read-bindings
           "~a request contained a non-numeric content-length"
           who)])]

      [#f
       (values (delay empty) #f)]))

  (define content-type
    (cond
      [(headers-assq* #"Content-Type" headers) => header-value]
      [else #f]))

  (cond
    [(bytes-ci=? #"GET" meth)
     (values bindings-GET #f)]

    [(match-multipart content-type)
     => (match-lambda
          [(list _ content-boundary)
           (define bindings
             (for/list ([part (in-list (multipart-reader in content-boundary))])
               (match part
                 [(struct mime-part (headers contents))
                  (define rhs
                    (cond
                      [(headers-assq* #"Content-Disposition" headers) => header-value]
                      [else #""]))

                  (match* ((regexp-match #"filename=(\"([^\"]*)\"|([^ ;]*))" rhs)
                           (regexp-match #"[^e]name=(\"([^\"]*)\"|([^ ;]*))" rhs))
                    [(#f #f)
                     (network-error
                      'reading-bindings
                      "Couldn't extract form field name for file upload")]

                    [(#f (list _ _ f0 f1))
                     (make-binding:form (or f0 f1)
                                        (port->bytes contents))]

                    [((list _ _ f00 f01) (list _ _ f10 f11))
                     (make-binding:file/port (or f10 f11)
                                             (or f00 f01)
                                             headers
                                             contents)])])))

           (values (delay
                     (append (force bindings-GET) bindings))
                   #f)])]

    [(match-urlencoded content-type)
     (read-data meth (lambda (data)
                       (values
                        (delay
                          (append
                           (parse-bindings data)
                           (force bindings-GET)))
                        data)))]

    [else
     (read-data meth (lambda (data)
                       (values bindings-GET data)))]))

;; parse-bindings : bytes? -> (listof binding?)
(define match-query-key
  (let ([rx (byte-regexp #"^([^=&]+)([=&]?)")])
    (lambda (in)
      (regexp-try-match rx in))))

(define match-query-value
  (let ([rx (byte-regexp #"^([^&]+)(&?)")])
    (lambda (in)
      (regexp-try-match rx in))))

(define (urldecode bs)
  (string->bytes/utf-8
   (form-urlencoded-decode
    (bytes->string/utf-8 bs))))

(define (parse-bindings data)
  (call-with-input-bytes data
    (lambda (in)
      (let loop ([bindings null])
        (match (match-query-key in)
          [(list _ key #"=")
           (match (match-query-value in)
             ;; k=&...
             ;; k=#<eof>
             [#f
              ;; skip the & or do nothing on #<eof>
              (read-byte in)
              (loop (cons (make-binding:form (urldecode key) #"")
                          bindings))]

             ;; k=...&...
             [(list _ value _)
              (loop (cons (make-binding:form (urldecode key)
                                             (urldecode value))
                          bindings))])]

          ;; k
          ;; k&
          [(list _ key (or #"" #"&"))
           (loop (cons (make-binding:form (urldecode key) #"")
                       bindings))]

          ;; #<eof>
          [#f
           (reverse bindings)])))))

(module+ internal-test
  (provide parse-bindings))


;; **************************************************
;; read-mime-multipart

; mime-part : (listof header?) * (listof bytes?)
(struct mime-part (headers contents)
  #:transparent)

; make-spooled-temporary-file : number -> inp outp
; Like `make-pipe', but data is written to a real file if the total amount
; of data starts to excceed `max-length'.  This data type is *not
; safe* against data races!
(define (make-spooled-temporary-file max-length)
  ; Create the file up-front so that we always have a file path to
  ; refer to. This is also used as the name of the custom ports so that
  ; application authors can access the path of the underlying file to
  ; operate directly on it if they need to.
  (define filename (make-temporary-file))
  (define-values (in out)
    (make-pipe))

  (define (choose-writer non-blocking? enable-breaks?)
    (cond
      [enable-breaks? write-bytes-avail/enable-break]
      [non-blocking? write-bytes-avail*]
      [else write-bytes]))

  ; write data to the port until the limit is hit
  (define (write/port! bs start-pos end-pos non-blocking? enable-breaks?)
    (define current-length (pipe-content-length out))
    (define input-length (- end-pos start-pos))
    (cond
      [(> (+ current-length input-length) max-length)
       (transition!)
       (write/file! bs start-pos end-pos non-blocking? enable-breaks?)]

      [else
       ((choose-writer non-blocking? enable-breaks?) bs out start-pos end-pos)]))

  ; write data to the file
  (define (write/file! bs start-pos end-pos non-blocking? enable-breaks?)
    ((choose-writer non-blocking? enable-breaks?) bs out start-pos end-pos))

  ; create a temp file, copy the data from the pipe and then close the pipe
  (define (transition!)
    (define new-out (open-output-file filename #:exists 'truncate/replace))
    (define new-in (open-input-file filename))
    (close-output-port out)
    (copy-port in new-out)
    (close-input-port in)
    (set! in new-in)
    (set! out new-out)
    (set! write! write/file!))

  (define write! write/port!)

  (values
   (make-input-port filename
                    (lambda (bs) (read-bytes-avail! bs in))
                    (lambda (bs s p) (peek-bytes-avail! bs s p in))
                    (lambda _ (close-input-port in)))
   (make-output-port filename
                     (guard-evt (lambda _ out))
                     (lambda args (apply write! args))
                     (lambda _ (close-output-port out)))))

(module+ internal-test
  (provide make-spooled-temporary-file))

;; This is kind of high just to be safe, but I don't think you ever see
;; more than a couple headers (Content-Disposition and Content-Type) per
;; field in practice.
(define MAX-HEADERS/FIELD 20)

; Find the starting position of `needle' within `haystack'.
(define (bytes-find haystack needle)
  (define haystack-len (bytes-length haystack))
  (define needle-len (bytes-length needle))
  (and (<= needle-len haystack-len)
       (or (and (bytes=? (subbytes haystack 0 haystack-len) needle) 0)
           (for*/first ([pos (in-range (add1 (- haystack-len needle-len)))]
                        [haystack* (in-value (subbytes haystack pos (+ pos needle-len)))]
                        #:when (bytes=? haystack* needle))
             pos))))

(define (read-mime-multipart in boundary
                             #:max-files [max-files 100]
                             #:max-file-length [max-file-length (* 10 1024 1024)]
                             #:max-file-memory-threshold [max-file-memory-threshold (* 1 1024 1024)]
                             #:max-fields [max-fields 100]
                             #:max-field-length [max-field-length (* 8 1024)])
  (define start-boundary (bytes-append #"--" boundary))
  (define start-boundary-len (bytes-length start-boundary))

  (define bufsize (max start-boundary-len (* 64 1024)))
  (define buf (make-bytes bufsize))

  (define (find-boundary haystack)
    (bytes-find haystack start-boundary))

  (define (subport in n)
    (make-limited-input-port in n #f))

  (define (collect-part-headers)
    (read-headers in MAX-HEADERS/FIELD (current-http-line-limit)))

  (define (collect-part-content file?)
    (define-values (content-in content-out)
      (make-spooled-temporary-file max-file-memory-threshold))

    (with-handlers ([(lambda (e) #t)
                     (lambda (e)
                       (delete-file/safe (object-name content-in))
                       (raise e))])

      ;; Increase `len' by `n', raising an exception if the limit is exceeded.
      (define (increase-length len n)
        (define len* (+ len n))
        (begin0 len*
          (when (> len* (if file? max-file-length max-field-length))
            (define who (if file? "file" "field"))
            (network-error 'read-mime-multipart "~a exceeds max length" who))))

      ;; Read unitl the position before the CRLF that precedes the
      ;; boundary and then skip over the boundary in the input.
      (define (copy-until-boundary! len pos)
        (define pos* (max 0 (- pos 2)))
        (increase-length len pos*)
        (copy-port (subport in pos*) content-out)

        (define line
          (begin
            ;; Skip the CRLF and then read the boundary line.
            (read-http-line/limited in)
            (read-http-line/limited in)))

        (when (eof-object? line)
          (network-error 'read-mime-multipart "port closed prematurely"))

        (bytes=? line start-boundary))

      (define more-parts?
        (let loop ([len 0]
                   [prev #""])
          (define n (peek-bytes-avail! buf 0 #f in))
          (cond
            [(eof-object? n)
             (network-error 'read-mime-multipart "port closed prematurely")]

            [else
             (define bs (subbytes buf 0 n))
             (cond
               [(find-boundary bs)
                => (lambda (pos)
                     (copy-until-boundary! len pos))]

               [(find-boundary (bytes-append prev bs))
                => (lambda (pos)
                     (copy-until-boundary! len (- pos (bytes-length prev))))]

               [else
                (define new-len (increase-length len n))
                (define new-prev (subbytes bs (max 0 (- n start-boundary-len))))
                (copy-port (subport in n) content-out)
                (loop new-len new-prev)])])))

      (close-output-port content-out)
      (values content-in more-parts?)))

  (define (read-parts [parts null]
                      [total-files 0]
                      [total-fields 0])

    (define headers (collect-part-headers))
    (define file? (file-part? headers))
    (define field? (not file?))
    (define total-files* (if file? (add1 total-files) total-files))
    (define total-fields* (if file? total-fields (add1 total-fields)))
    (cond
      [(and file? (> total-files* max-files))
       (network-error 'read-mime-multipart "too many files")]

      [(and field? (> total-fields* max-fields))
       (network-error 'read-mime-multipart "too many fields")])

    (define-values (content more-parts?)
      (collect-part-content file?))
    (define part (mime-part headers content))
    (define parts* (cons part parts))
    (if more-parts?
      (read-parts parts* total-files* total-fields*)
      (reverse parts*)))

  (let skip-preamble ()
    (define line (read-http-line/limited in))
    (cond
      [(eof-object? line) (network-error 'read-mime-multipart "port closed prematurely")]
      [(bytes=? line start-boundary) (read-parts)]
      [else (skip-preamble)])))

(define (file-part? headers)
  (match (headers-assq* #"Content-Disposition" headers)
    [(header _ (regexp #rx"filename=")) #t]
    [_ #f]))

(define (delete-file/safe p)
  (with-handlers ([exn:fail:filesystem? void])
    (delete-file p)))

(module+ internal-test
  (provide (struct-out mime-part)
           read-mime-multipart))
