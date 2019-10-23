#lang racket/base

(require net/uri-codec
         net/url
         racket/contract
         racket/generator
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
               #:max-request-file-length exact-positive-integer?)
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
          #:max-request-file-length [max-request-file-length (* 1 1024 1024)])
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
    (read-bindings&post-data/raw data-ip
                                 method
                                 uri
                                 headers
                                 max-request-body-length
                                 max-request-files
                                 max-request-file-length))
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
              (network-error 'complete-request "content (~a) exceeds max length (~a)" new-size max-body-length))

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

; read-http-line/limited : inp number -> bytes
; read-bytes-line against untrusted input is not safe since the client
; could just feed us bytes until we run out of memory. This function will
; attempt to read a line from the input port up to a hard limit.
(define current-http-line-limit
  (make-parameter (* 8 1024)))

(define (CR? b) (eqv? b 13))
(define (LF? b) (eqv? b 10))

(define (bytes-find-crlf bs len)
  (for/first ([i (in-range 0 (sub1 len))]
              #:when (and (CR? (bytes-ref bs i))
                          (LF? (bytes-ref bs (add1 i)))))
    i))

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
           [(> offset* n) offset*]
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
      (network-error 'read-request "http input closed abruptly")
      (match (match-method line)
        [#f (network-error 'read-request "malformed request ~a" line)]
        [(list _ method url major minor)
         (define us (bytes->string/utf-8 url))
         (define u1 (string->url us))

         (values method
                 (cond
                   [(and (url-host u1) (not (url-scheme u1)))
                    (string->url (format "//~a" us))]
                   [else
                    u1])
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

;; read-bindings&post-data/raw: input-port symbol url (listof header?) number number number -> (values (or/c (listof binding?) string?) (or/c bytes? false/c?))
(define (read-bindings&post-data/raw in meth uri headers max-body-length max-files max-file-length)
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
                 (network-error 'read-bindings "body length exceeds limit of ~a" max-body-length))

               ;; this is safe because of the preceding guard on the length.
               (define data (read-bytes/lazy len in))
               (cond
                 [(eof-object? data)
                  (network-error
                   'read-bindings
                   "Post data ended pre-maturely")]

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
           (define content-length
             (cond
               [(headers-assq* #"Content-Length" headers) => header-value]
               [else (network-error 'read-bindings "multipart data without Content-Length is not allowed")]))

           (define bindings
             (for/list ([part (in-list (read-mime-multipart in content-boundary content-length max-files max-file-length))])
               (match part
                 [(struct mime-part (headers contents))
                  (define rhs
                    (header-value
                     (headers-assq* #"Content-Disposition" headers)))

                  (match* ((regexp-match #"filename=(\"([^\"]*)\"|([^ ;]*))" rhs)
                           (regexp-match #"[^e]name=(\"([^\"]*)\"|([^ ;]*))" rhs))
                    [(#f #f)
                     (network-error
                      'reading-bindings
                      "Couldn't extract form field name for file upload")]

                    [(#f (list _ _ f0 f1))
                     (make-binding:form (or f0 f1)
                                        ;; FIXME
                                        (port->bytes contents))]

                    [((list _ _ f00 f01) (list _ _ f10 f11))
                     (make-binding:file (or f10 f11)
                                        (or f00 f01)
                                        headers
                                        ;; FIXME
                                        (port->bytes contents))])])))

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

; in-http-lines : inp number -> sequence-of bytes
(define (in-http-lines in limit [bufsize (* 16 1024)])
  (in-generator
   (let loop ([buff #""]
              [remaining limit])
     (define data (read-bytes bufsize in))
     (cond
       ;; the port was closed prematurely so yield whatever we buffered
       ;; up so far
       [(eof-object? data)
        (yield buff)]

       [else
        (define bytes-read (bytes-length data))
        (define remaining* (- remaining bytes-read))
        (define (yield-lines buff)
          (let loop ([buff buff])
            (cond
              [(bytes-find-crlf buff (bytes-length buff))
               => (lambda (pos)
                    (yield (subbytes buff 0 pos))
                    (loop (subbytes buff (+ pos 2))))]

              [else buff])))

        (cond
          ;; we've managed to read past the limit so split the lines
          ;; up to that point and then discard the rest
          [(< remaining* 0)
           (define data* (subbytes data 0 (+ bytes-read remaining*)))
           (define buff* (yield-lines (bytes-append buff data*)))
           (yield buff*)]

          ;; we haven't hit the limit yet so we can split whatever
          ;; lines we've got and then recur
          [(> remaining* 0)
           (define buff* (yield-lines (bytes-append buff data)))
           (loop buff* remaining*)]

          ;; we've hit the limit exactly so yield what we've got
          [else
           (define buff* (yield-lines (bytes-append buff data)))
           (yield buff*)])]))))

(module+ internal-test
  (provide in-http-lines))

;; This is kind of high just to be safe, but I don't think you ever see
;; more than a couple headers (Content-Disposition and Content-Type) per
;; field in practice.
(define MAX-HEADERS/FIELD 20)

(define CRLF #"\r\n")

; read-mime-multipart : inp string number number number -> list-of mime-part
(define (read-mime-multipart in boundary [max-length +inf.0] [max-files 100] [max-file-length (* 1 1024 1024)])
  (define start-boundary (bytes-append #"--" boundary))
  (define end-boundary (bytes-append start-boundary #"--"))
  (define-values (more? next-line)
    (sequence-generate (in-http-lines in max-length)))

  (define (collect-part-headers)
    (define-values (in out)
      (make-pipe))

    (let loop ([line (next-line)])
      (cond
        [(bytes=? line #"")
         (display CRLF out)]

        [else
         (display line out)
         (display CRLF out)
         (loop (next-line))]))

    (close-output-port out)
    (read-headers in MAX-HEADERS/FIELD (current-http-line-limit)))

  (define (collect-part-content)
    (define-values (in out)
      (make-pipe))

    (define-values (len more-parts?)
      (let loop ([len 0])
        (define line (next-line))
        (cond
          [(bytes=? line start-boundary)
           (values len #t)]

          [(or (bytes=? line #"")
               (bytes=? line end-boundary))
           (values len #f)]

          [else
           (define len* (+ len (bytes-length line) 2))
           (when (> len* max-file-length)
             (network-error 'read-mime-multipart "field exceeds max file length"))

           (display line out)
           (display CRLF out)
           (loop len*)])))

    ;; Strip the final CRLF from the contents.
    (define limited-in (make-limited-input-port in (max 0 (- len 2))))
    (close-output-port out)
    (values limited-in more-parts?))

  (define (read-parts [parts null])
    (cond
      [(more?)
       (when (= (length parts) max-files)
         (network-error 'read-mime-multipart "too many fields"))

       (define headers (collect-part-headers))
       (define-values (content more-parts?) (collect-part-content))
       (define part (mime-part headers content))
       (if more-parts?
           (read-parts (cons part parts))
           (reverse (cons part parts)))]

      [else
       (reverse parts)]))

  (with-handlers ([(lambda (e)
                     (and (exn:fail:contract? e)
                          (regexp-match #"sequence has no more values" (exn-message e))))
                   (lambda _
                     (network-error 'read-mime-multipart "port closed prematurely"))])
    (let skip-preamble ()
      (if (bytes=? (next-line) start-boundary)
          (read-parts)
          (skip-preamble)))))

(module+ internal-test
  (provide (struct-out mime-part)
           read-mime-multipart))
