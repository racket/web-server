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
         web-server/safety-limits
         (submod web-server/safety-limits private)
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

(provide
 (contract-out
  ;; CAUTION: To maximize backwards compatibility for low-level
  ;; programs, `make-read-request` and `read-request`
  ;; use `(make-unlimited-safety-limits)`
  ;; if the `#:safety-limits` argument is not given,
  ;; which leaves applications vulnerable.
  ;; Generally, you should use `make-read-request` with
  ;; an explicit `#:safety-limits` argument.
  ;; (Even better, don't use this private, undocumented
  ;; module directly in the first place!!)
  ;; To further discourage unsafety, `standard-read-request`
  ;; uses `(make-safety-limits)`.
  [make-read-request
   (->* ()
        (#:connection-close? boolean?
         #:safety-limits safety-limits?)
        read-request/c)]
  [read-request
   read-request/c]
  [standard-read-request
   read-request/c]
  ;; `parse-bindings` and `read-headers` were supposed to be private,
  ;; but there are at least some packages that depend on them,
  ;; e.g. https://pkgs.racket-lang.org/package/rfc6455
  ;; CAUTION: They effectively use `(make-unlimited-safety-limits)`
  ;; by default to maximize compatibility,
  ;; which leaves applications vulnerable as noted above.
  [parse-bindings (-> bytes? (listof binding?))]
  [read-headers (->* (input-port?)
                     (#:safety-limits safety-limits?)
                     (listof header?))]))


(module* internal-test #f
  (provide read-http-line/limited
           read-bytes/lazy
           read-request-line
           read-bindings&post-data/raw
           make-spooled-temporary-file
           (struct-out mime-part)
           read-mime-multipart))

;; **************************************************
;; read-request: connection number (input-port -> string string) -> request boolean?
;; Read the request line, and the headers, determine if the connection should
;; be closed after servicing the request, and build a request structure
(define (make-read-request #:connection-close? [connection-close? #f]
                           #:safety-limits [limits (make-unlimited-safety-limits)])

  (match-define (safety-limits
                 #:request-read-timeout read-timeout
                 #:max-request-line-length max-request-line-length
                 #:max-request-headers max-request-fields
                 #:max-request-header-length max-request-field-length
                 #:max-request-body-length max-request-body-length)
    limits)

  (define (do-read-request conn host-port port-addresses)
    (reset-connection-timeout! conn read-timeout)
    (define ip (connection-i-port conn))
    (define-values (method uri major minor)
      (read-request-line ip max-request-line-length))
    (define initial-headers
      (read-headers* ip
                     #:max-count max-request-fields
                     #:max-length max-request-field-length))
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
                                   #:safety-limits limits))
    (define request
      (make-request method uri headers bindings/raw-promise raw-post-data
                    host-ip host-port client-ip))
    (define close?
      (or connection-close?
          (close-connection? headers major minor
                             client-ip host-ip)))
    (values request close?))

  (define (read-request conn host-port port-addresses)
    (with-handlers ([exn:fail?
                     (λ (exn)
                       (kill-connection! conn)
                       (raise exn))])
      (do-read-request conn host-port port-addresses)))

  read-request)

(define read-request
  (make-read-request))

(define standard-read-request
  (make-read-request #:safety-limits (make-safety-limits)))

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
         (define size-line (read-http-line/limited real-ip #:limit max-header-length))
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
            (copy-bytes! size-in-bytes real-ip decode-op)
            (read-http-line/limited real-ip #:limit 2)
            (loop new-size)])))

     (define more-headers
       (list* (header #"Content-Length" (string->bytes/utf-8 (number->string total-size)))
              (read-headers* real-ip #:max-count max-remaining-headers #:max-length max-header-length)))

     (close-output-port decode-op)
     (values decoded-ip (append initial-headers more-headers))]

    [_
     (values real-ip initial-headers)]))


;; **************************************************
;; close-connection?

; close-connection? : (listof (cons symbol bytes)) number number string string -> boolean
; determine if this connection should be closed after serving the
; response
(define (close-connection? headers major minor client-ip host-ip)
  (or (< major 1)
      (and (= major 1) (= minor 0))
      (match (headers-assq* #"Connection" headers)
        [(struct header (f v))
         (regexp-match? #rx#"[cC][lL][oO][sS][eE]" v)]
        [#f
         #f])))


;; **************************************************
;; safe reading

(define (CR? b)     (eqv? b 13))
(define (LF? b)     (eqv? b 10))
(define (hyphen? b) (eqv? b 45))

(define ((make-finder rx) bs [len #f])
  (match (regexp-match-positions rx bs 0 len)
    [(list (cons start _)) start]
    [_ #f]))

(define find-cr   (make-finder #rx#"\r"))
(define find-crlf (make-finder #rx#"\r\n"))

; read-http-line/limited : inp number -> bytes
; `read-bytes-line' against untrusted input is not safe since the client
; could just feed us bytes until we run out of memory. This function
; will attempt to read a line from the input port up to a hard limit.
(define (read-http-line/limited #:limit limit
                                [in (current-input-port)]
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
        [(find-crlf buf len)
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


; read-bytes/lazy : number inp -> bytes
; Like `read-bytes', but waits until the expected number of bytes is
; available within the input port before allocating the final buffer.
(define (read-bytes/lazy n in [bufsize 4096])
  (cond
    [(zero? n) #""]
    [else
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
       [else (read-bytes (min offset n) in)])]))


;; **************************************************
;; read-request-line
(define (match-method a)
  (regexp-match #rx#"^([^ ]+) (.+) HTTP/([0-9]+)\\.([0-9]+)$" a))

; read-request-line : iport nonnegative-length/c -> bytes url number number
; to read in the first line of an http request, AKA the "request line"
; effect: in case of errors, complain [MF: where] and close the ports
(define (read-request-line ip max-length)
  (define line (read-http-line/limited ip #:limit max-length))
  (if (eof-object? line)
      (network-error 'read-request "http input closed prematurely")
      (match (match-method line)
        [#f (network-error 'read-request "malformed request ~e" line)]
        [(list _ method url major minor)
         (values method
                 (let* ([us (bytes->string/utf-8 url)]
                        [u1 (string->url us)])
                   (cond
                     [(and (url-host u1) (not (url-scheme u1)))
                      (string->url (format "//~a" us))]
                     [else
                      u1]))
                 (string->number (bytes->string/utf-8 major))
                 (string->number (bytes->string/utf-8 minor)))])))



;; **************************************************
;; read-headers

;; read-headers : iport [safety-limits?] -> (listof header?)
;; NOTE: This was supposed to be private, but is used by at least some
;; packages which we don't want to break: see note above, on `provide`.
(define (read-headers in #:safety-limits [limits (make-unlimited-safety-limits)])
  (read-headers* in
                 #:max-count (safety-limits-max-request-headers limits)
                 #:max-length (safety-limits-max-request-header-length limits)))

(define (read-headers* in #:max-count max-heads #:max-length max-length)
  (for/list ([count (in-naturals)]
             [l (in-producer (λ (in max-length) (read-http-line/limited in #:limit max-length))
                             (λ (l) (or (eof-object? l) (zero? (bytes-length l))))
                             in
                             max-length)])
    (when (> count max-heads)
      (network-error 'read-headers "header count exceeds limit of ~a" max-heads))
    (match (regexp-match #rx#"^([^:]*):[ \t]*(.*)" l)
      [(list _ field value)
       (header field (read-folded-head in value max-length))]
      [_
       (network-error 'read-headers "malformed header: ~e" l)])))

; read-folded-head : iport bytes number -> bytes
; reads the next line of input for headers that are line-folded
(define (read-folded-head in rhs max-length)
  (match (peek-byte in)
    ;; leading SPACE or TAB
    [(or 32 9)
     (define line (read-http-line/limited in #:limit max-length))
     (define rhs* (bytes-append rhs line))
     (when (> (bytes-length rhs*) max-length)
       (network-error 'read-headers "header too long (~a)" max-length))
     (read-folded-head in rhs* max-length)]
    [_ rhs]))


;; **************************************************
;; read-bindings

(define (read-bindings&post-data/raw in meth uri headers
                                     #:safety-limits [limits (make-safety-limits)])
  (define max-body-length
    (safety-limits-max-request-body-length limits))
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

               (define data (read-bytes/lazy len in))
               (when (or (eof-object? data) (< (bytes-length data) len))
                 (network-error
                  'read-bindings
                  "port closed prematurely"))

               (proc data))]
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

    [(and content-type
          (regexp-match #rx#"multipart/form-data; *boundary=(.*)" content-type))
     => (match-lambda
          [(list _ content-boundary)
           (define bindings
             (for/list ([part (in-list (read-mime-multipart in content-boundary #:safety-limits limits))])
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

    [(and content-type
          (regexp-match? #rx#"application/x-www-form-urlencoded" content-type))
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


(define (urldecode bs)
  (string->bytes/utf-8
   (form-urlencoded-decode
    (bytes->string/utf-8 bs))))

;; parse-bindings : bytes? -> (listof binding?)
;; NOTE: This was supposed to be private, but is used by at least some
;; packages which we don't want to break: see note above, on `provide`.
(define (parse-bindings data)
  (call-with-input-bytes data
    (lambda (in)
      (let loop ([bindings null])
        (match (regexp-try-match #rx#"^([^=&]+)([=&]?)" in) ;; query key
          [(list _ key #"=")
           (match (regexp-try-match #rx#"^([^&]+)(&?)" in) ;; query value
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


;; **************************************************
;; read-mime-multipart

; mime-part : (listof header?) * input-port?
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

(define MULTIPART-BUFSIZE (* 64 1024))

;; RFC 2046[1] specifies that the maximum boundary length must not be
;; longer than 70 characters, not counting the leading hyphens. To help
;; compatibility with bad client implementations, we double that limit.
;;
;; [1]: https://tools.ietf.org/html/rfc2046#section-5.1.1
(define MAX-BOUNDARY-LEN 140)

;; `read-mime-multipart' permits a "preamble" as defined in RFC 2046[1],
;; which is cited by RFC 7578. However, the `multipart/form-data`
;; definition doesn't explicitly mention a "preamble" and general tries
;; to forbid MIME features it doesn't use, and browsers etc. don't seem
;; to use this. Thus, we use a small limit that hopefully will be enough
;; for any legacy clients out there.
;;
;; [1]: https://tools.ietf.org/html/rfc2046#section-5.1
(define MAX-PREAMBLE-LINES 20)

;; This is a guess at a limit that should be long enough for a legacy
;; "preamble": it is the limit on message body lines from RFC 821, which
;; is cited by (but not incorporated into) RFC 2046. Because RFC 2046
;; limits boundaries to 70 ASCII characters, this will always be longer
;; than `end-boundary-len`, too.
(define MAX-PREAMBLE-LINE-LEN 998)

;; Maximum number of headers allowed for a single multipart/form-data
;; part. This is a constant because multipart/form-data headers are
;; limited by RFC7578[1]. It is high to avoid rejecting any reasonable
;; request.
;;
;; [1]: https://tools.ietf.org/html/rfc7578#section-4.8
(define MAX-HEADERS/PART 20)

(define (read-mime-multipart in boundary #:safety-limits [limits (make-safety-limits)])
  (match-define (safety-limits #:max-form-data-parts max-parts
                               #:max-form-data-files max-files
                               #:max-form-data-file-length max-file-length
                               #:form-data-file-memory-threshold max-file-memory-threshold
                               #:max-form-data-fields max-fields
                               #:max-form-data-field-length max-field-length
                               #:max-form-data-header-length max-header-length)
    limits)

  (when (> (bytes-length boundary) MAX-BOUNDARY-LEN)
    (network-error 'read-mime-multipart "boundary too long"))

  (define start-boundary (bytes-append #"--" boundary))
  (define start-boundary-len (bytes-length start-boundary))
  (define start-boundary-rx (byte-regexp (regexp-quote start-boundary)))
  (define end-boundary (bytes-append start-boundary #"--"))
  (define end-boundary-len
    ;; trailing CRLF is handled by `read-http-line/limited`
    (bytes-length end-boundary))

  (define buf (make-bytes MULTIPART-BUFSIZE))
  (define find-boundary (make-finder start-boundary-rx))

  (define (read-part-headers)
    (read-headers* in
                   #:max-count MAX-HEADERS/PART
                   #:max-length max-header-length))

  (define (read-part-content file?)
    (define-values (content-in content-out)
      (make-spooled-temporary-file max-file-memory-threshold))

    ;; Increase `len' by `n', raising an exception if the limit is exceeded.
    (define-syntax-rule (increase-length len n)
      (let ([len* (+ len n)])
        (begin0 len*
          (when (> len* (if file? max-file-length max-field-length))
            (define who (if file? "file" "field"))
            (network-error 'read-mime-multipart "~a exceeds max length" who)))))

    (with-handlers ([(lambda (_) #t)
                     (lambda (e)
                       (delete-file/safe (object-name content-in))
                       (raise e))])
      (let read-loop ([len 0])
        (define n-read (peek-bytes-avail! buf 0 #f in))
        (when (eof-object? n-read)
          (network-error 'read-mime-multipart "port closed prematurely"))

        ;; We are guaranteed to have read at least 1 byte at this point.
        (cond
          ;; We found a boundary in the buffer, so we're nearly done.
          ;; Commit to the content up to the CRLF that precedes the
          ;; boundary.
          [(find-boundary buf n-read)
           => (lambda (pos)
                (when (< pos 2)
                  (network-error 'read-mime-multipart "part without data"))
                (define pos* (- pos 2))
                (increase-length len pos*)
                (copy-bytes! pos* in content-out))]

          ;; We found a CR in the buffer, so a boundary might follow.
          [(find-cr buf n-read)
           => (lambda (pos)
                ;; Commit to the content up to CR.
                (define new-len (increase-length len pos))
                (copy-bytes! pos in content-out)
                ;; Check for a CRLF followed by a boundary.
                (define crlf+boundary-len (+ 2 start-boundary-len))
                (define maybe-crlf+boundary (peek-bytes crlf+boundary-len 0 in))
                (unless (find-boundary maybe-crlf+boundary)
                  ;; We're not at a boundary so just consume the CR.
                  (define new-len* (increase-length new-len 1))
                  (copy-bytes! 1 in content-out)
                  (read-loop new-len*)))]

          ;; We're right at a boundary but aren't expecting it so the
          ;; request must not have contained any data for a part.
          [(and (hyphen? (bytes-ref buf 0))
                (find-boundary (peek-bytes start-boundary-len 0 in)))
           (network-error 'read-mime-multipart "part without data")]

          ;; No boundary and no CR found so we can just commit
          ;; whatever's in the buffer then loop.
          [else
           (define new-len (increase-length len n-read))
           (copy-bytes! n-read in content-out)
           (read-loop new-len)]))

      ;; Skip the CRLF.
      (define maybe-crlf (read-bytes 2 in))
      (unless (equal? #"\r\n" maybe-crlf)
        (network-error 'read-mime-multipart
                       "malformed part\n  expected: CRLF before boundary\n  given: ~e"
                       maybe-crlf))

      ;; Read the next boundary.
      (define line (read-http-line/limited in #:limit end-boundary-len))
      (define more-parts?
        (cond
          [(eof-object? line) (network-error 'read-mime-multipart "port closed prematurely")]
          [(bytes=? line start-boundary) #t]
          [(bytes=? line end-boundary) #f]
          [else (network-error 'read-mime-multipart "malformed boundary line: ~.s" line)]))

      (close-output-port content-out)
      (values content-in more-parts?)))

  (define (read-parts [parts null]
                      [total-files 0]
                      [total-fields 0])

    (define headers (read-part-headers))
    (define file? (file-part? headers))

    (let-values ([{total-files total-fields}
                  (if file?
                      (values (add1 total-files) total-fields)
                      (values total-files (add1 total-fields)))])
      (if file?
          (when (> total-files max-files)
            (network-error 'read-mime-multipart "too many files"))
          (when (> total-fields max-fields)
            (network-error 'read-mime-multipart "too many fields")))
      (when (> (+ total-files total-fields) max-parts)
        (network-error 'read-mime-multipart "too many multipart/form-data parts"))

      (define-values (content more-parts?)
        (read-part-content file?))
      (define part (mime-part headers content))
      (define parts* (cons part parts))
      (if more-parts?
          (read-parts parts* total-files total-fields)
          (reverse parts*))))

  (let skip-preamble ([preamble-line-count 0])
    (unless (< preamble-line-count MAX-PREAMBLE-LINES)
      (network-error 'read-mime-multipart "too many \"preamble\" lines"))

    (define line (read-http-line/limited in #:limit MAX-PREAMBLE-LINE-LEN))
    (cond
      [(eof-object? line) (network-error 'read-mime-multipart "port closed prematurely")]
      [(bytes=? line start-boundary) (read-parts)]
      [(bytes=? line end-boundary) null]
      [else (skip-preamble (add1 preamble-line-count))])))

(define (copy-bytes! amt from to)
  (copy-port (make-limited-input-port from amt #f) to))

(define (file-part? headers)
  (match (headers-assq* #"Content-Disposition" headers)
    [(header _ (regexp #rx"filename=")) #t]
    [_ #f]))

(define (delete-file/safe p)
  (with-handlers ([exn:fail:filesystem? void])
    (delete-file p)))
