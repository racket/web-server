#lang racket/base

(require racket/contract
         file/md5
         racket/port
         racket/list
         racket/match
         xml/xml
         web-server/private/connection-manager
         (submod web-server/private/connection-manager private)
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/private/util
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse/lib/function-header))

(provide
 (contract-out
  [print-headers
   (output-port? (listof header?) . -> . any)]
  [output-response
   (connection? response? . -> . any)]
  [output-response/method
   (connection? response? bytes? . -> . any)]
  [output-file
   (connection? path-string? bytes? (or/c bytes? #f) (or/c pair? #f) . -> . any)]))

(define-simple-macro (define/ext (~and (name:id conn:id arg:formal ...) fun-header)
                       body:expr ...+)
  (define fun-header
    (with-handlers ([exn:fail? (λ (e)
                                 (kill-connection! conn)
                                 (raise e))])
      body ...
      (flush-output (connection-o-port conn)))))



(define (output-response conn resp)
  (output-response/method conn resp #"GET"))

(define/ext (output-response/method conn resp meth)
  (cond
    [(or
      ;; If it is terminated, just continue
      (terminated-response? resp)
      ;; If it is HTTP/1.0, ditto
      (connection-close? conn)
      ;; Or, if it is a HEAD request
      (bytes-ci=? meth #"HEAD"))
     (output-response-head conn resp)
     (unless (bytes-ci=? meth #"HEAD")
       (output-response-body conn resp))]
    ;; Otherwise, use chunked encoding
    [else
     (output-response-head conn resp
                           (list (header #"Transfer-Encoding" #"chunked")))
     (output-response-body/chunked conn resp)]))

;; Write the headers portion of a response to an output port.
;; NOTE: According to RFC 2145 the server should write HTTP/1.1
;;       header for *all* clients.
(define-syntax-rule (maybe-header h k v)
  (if (hash-has-key? h k)
      empty
      (list (header k v))))
(define-syntax-rule (maybe-headers h [k v] ...)
  (append (maybe-header h k v)
          ...))

(define (output-response-head conn bresp [more-hs empty])
  (fprintf (connection-o-port conn)
           "HTTP/1.1 ~a ~a\r\n"
           (response-code bresp)
           (response-message bresp))
  (define hs (append (response-headers bresp) more-hs))
  (define seen? (make-hash))
  (for ([h (in-list hs)])
    (hash-set! seen? (header-field h) #t))
  (output-headers
   conn
   (append
    (maybe-headers
     seen?
     [#"Date"
      (string->bytes/utf-8 (seconds->gmt-string (current-seconds)))]
     [#"Last-Modified"
      (string->bytes/utf-8 (seconds->gmt-string (response-seconds bresp)))]
     [#"Server"
      #"Racket"])
    (if (response-mime bresp)
        (maybe-headers
         seen?
         [#"Content-Type"
          (response-mime bresp)])
        empty)
    (if (connection-close? conn)
        (maybe-headers
         seen?
         [#"Connection" #"close"])
        empty)
    hs)))

;; output-headers : connection (list-of header) -> void
(define (output-headers conn headers)
  (print-headers (connection-o-port conn) headers))

;; print-headers : output-port (list-of header) -> void
(define (print-headers out headers)
  (for-each (match-lambda
              [(struct header (field value))
               (fprintf out "~a: ~a\r\n" field value)])
            headers)
  (fprintf out "\r\n"))

; RFC 2616 Section 4.4
(define (terminated-response? r)
  (define hs (response-headers r))
  (or (headers-assq* #"Content-Length" hs)
      (cond
        [(headers-assq* #"Transfer-Encoding" hs)
         => (λ (h) (not (bytes=? (header-value h) #"identity")))]
        [else #f])))

(define (output-response-body conn bresp)
  ;; This function gets whatever time is left after the headers were
  ;; written to write its body to the client.
  (define o-port (connection-o-port conn))
  ((response-output bresp) o-port)
  (flush-output o-port))

(define (output-response-body/chunked conn bresp)
  ;; Flush the headers immediately since the response handler can wait
  ;; a while before writing anything out to the output port.
  (flush-output (connection-o-port conn))

  (define-values (from-servlet to-chunker) (make-pipe))
  (define to-client (connection-o-port conn))
  (define to-chunker-t
    (thread
     (λ ()
       ;; When errors occur, we immediately kill the connection to
       ;; make it clear to the client that something went wrong.  By
       ;; doing this, we ensure that the terminating chunk won't be
       ;; sent and the client can't misinterpret the response as
       ;; successful.  This is in line with how other web servers
       ;; behave.
       ;;
       ;; See: https://github.com/racket/web-server/pull/93
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (kill-connection! conn)
                          (close-output-port to-chunker)
                          (raise e))])
         ((response-output bresp) to-chunker)
         (close-output-port to-chunker)))))

  ;; The client might go away while the response is being generated,
  ;; in which case the output port will be closed so we have to
  ;; gracefully back out when that happens.
  (with-handlers ([exn:fail?
                   (lambda (_)
                     (kill-thread to-chunker-t))])
    (define buffer (make-bytes 16384))
    (let loop ()
      (define bytes-read-or-eof
        (read-bytes-avail! buffer from-servlet))
      (unless (eof-object? bytes-read-or-eof)
        ;; For every chunk, increase the connection timeout s.t.
        ;; a responder can run indefinitely as long as it writes
        ;; *something* every (current-send-timeout) seconds.
        (reset-connection-response-send-timeout! conn)
        (fprintf to-client "~a\r\n" (number->string bytes-read-or-eof 16))
        (write-bytes buffer to-client 0 bytes-read-or-eof)
        (fprintf to-client "\r\n")
        (flush-output to-client)
        (loop)))
    (thread-wait to-chunker-t)
    (fprintf to-client "0\r\n")
    (fprintf to-client "\r\n")
    (flush-output to-client)))

; seconds->gmt-string : Nat -> String
; format is rfc1123 compliant according to rfc2068 (http/1.1)
(module+ testing
  (provide seconds->gmt-string))
(define (seconds->gmt-string s)
  (let* ([local-date (seconds->date s)]
         [date (seconds->date (- s (date-time-zone-offset local-date)))])
    (format "~a, ~a ~a ~a ~a:~a:~a GMT"
            (vector-ref DAYS (date-week-day date))
            (two-digits (date-day date))
            (vector-ref MONTHS (sub1 (date-month date)))
            (date-year date)
            (two-digits (date-hour date))
            (two-digits (date-minute date))
            (two-digits (date-second date)))))

; two-digits : num -> str
(define (two-digits n)
  (let ([str (number->string n)])
    (if (< n 10) (string-append "0" str) str)))

(define MONTHS
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define DAYS
  #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))


;; output-file: connection
;;              path
;;              symbol
;;              bytes
;;              (U (listof (U byte-range-spec suffix-byte-range-spec)) #f)
;;           -> void
;;
;; Ranges is #f if the client did not specify a Range header, or:
;;
;;     (list-of (U byte-range-spec suffix-byte-range-spec))
;;
;; where:
;;
;;     byte-range-spec : (cons integer (U integer #f))
;;     suffix-byte-range-spec : (cons #f integer)
;;
;; as described in the comments in dispatchers/dispatch-files.
;;
;; A boundary is generated only if a multipart/byteranges response needs
;; to be generated (i.e. if a Ranges header was specified with more than
;; one range in it).
(define/ext (output-file conn file-path method maybe-mime-type ranges)
  (output-file/boundary
   conn
   file-path
   method
   maybe-mime-type
   ranges
   (if (and ranges (> (length ranges) 1))
       (md5 (string->bytes/utf-8 (number->string (current-inexact-milliseconds))))
       #f)))

;; output-file/boundary: connection
;;                       path
;;                       symbol
;;                       bytes
;;                       (U (listof (U byte-range-spec suffix-byte-range-spec)) #f)
;;                       (U bytes #f)
;;                       -> void
(define (output-file/boundary conn file-path method maybe-mime-type ranges boundary)
  ;; Ensure there is enough time left to write the first chunk of
  ;; response data. `output-file-range' then resets the connection
  ;; timeout once for every chunk it's able to write to the client.
  (reset-connection-response-send-timeout! conn)

  ; total-file-length : integer
  (define total-file-length
    (file-size file-path))
  ; modified-seconds : integer
  (define modified-seconds
    (file-or-directory-modify-seconds file-path))
  ; boundary-length : (U integer #f)
  (define boundary-length
    (if boundary
        (bytes-length boundary)
        #f))
  ; If convert-http-ranges fails, send a 416 bad range resposne:
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (output-response-head
                      conn
                      (make-416-response modified-seconds maybe-mime-type)))])
    (let* (; converted-ranges : (alist-of integer integer)
           ; This is a list of actual start and end offsets in the file.
           ; See the comments for convert-http-ranges for more information.
           [converted-ranges
            (if ranges
                (convert-http-ranges ranges total-file-length)
                (list (cons 0 total-file-length)))]
           ; multipart-headers : (list-of bytes)
           ; This is a list of header blocks to prepend to each range being sent.
           ; The idea is so we can calculate an overall content-length for the
           ; response. This *must be* the same length as converted-ranges.
           [multipart-headers
            (if (> (length converted-ranges) 1)
                (prerender-multipart/byteranges-headers maybe-mime-type converted-ranges total-file-length)
                (list #""))]
           ; total-content-length : integer
           [total-content-length
            (if (= (length converted-ranges) 1)
                ; One range: content-length is the length of the range being sent:
                (- (cdar converted-ranges) (caar converted-ranges))
                ; Multiple ranges: content-length is the length of the multipart,
                ; including content, headers and boundaries:
                (+ (for/sum ([range (in-list converted-ranges)]
                             [headers (in-list multipart-headers)])
                     (+ (bytes-length headers)      ; length of the headers and header newline
                        (- (cdr range) (car range)) ; length of the content
                        2))                         ; length of the content newline
                   ; length of the intermediate boundaries
                   (* (+ boundary-length 4)
                      (length converted-ranges))
                   ; length of the final boundary
                   (+ boundary-length 6)))])
      ; Send a 206 iff ranges were specified in the request:
      (output-response-head
       conn
       (if ranges
           (make-206-response modified-seconds maybe-mime-type total-content-length total-file-length converted-ranges boundary)
           (make-200-response modified-seconds maybe-mime-type total-content-length)))
      ; Send the appropriate file content:
      ; TODO: What if we want to output-file during a POST?
      (when (bytes-ci=? method #"GET")
        (with-handlers ([exn:fail? (lambda (exn)
                                     (network-error 'output-file "~a" (exn-message exn)))])
          (call-with-input-file* file-path
            (lambda (input)
              (if (= (length converted-ranges) 1)
                  ; Single ranges (in 200 or 206 responses) are sent straight out
                  ; in their simplest form:
                  (output-file-range conn input (caar converted-ranges) (cdar converted-ranges))
                  ; Multiple ranges are encoded as multipart/byteranges:
                  (let loop ([ranges converted-ranges]
                             [multipart-headers multipart-headers])
                    (match ranges
                      [(list)
                       ; Final boundary (must start on new line; ends with a new line)
                       (fprintf (connection-o-port conn) "--~a--\r\n" boundary)
                       (void)]
                      [(list-rest (list-rest start end) rest)
                       ; Intermediate boundary (must start on new line; ends with a new line)
                       (fprintf (connection-o-port conn) "--~a\r\n" boundary)
                       ; Headers and new line
                       (display (car multipart-headers) (connection-o-port conn))
                       ; Content
                       (output-file-range conn input start end)
                       ; Newline before next field
                       (fprintf (connection-o-port conn) "\r\n")
                       (loop rest (cdr multipart-headers))]))))))))))

;; prerender-multipart/byteranges-headers : bytes (alist-of integer integer) integer -> (list-of bytes)
(define (prerender-multipart/byteranges-headers maybe-mime-type converted-ranges total-file-length)
  (map (lambda (range)
         (match range
           [(list-rest start end)
            (let ([out (open-output-bytes)])
              (when maybe-mime-type
                (print-headers out (list (make-header #"Content-Type" maybe-mime-type))))
              (print-headers out (list (make-content-range-header start end total-file-length)))
              (begin0 (get-output-bytes out)
                (close-output-port out)))]))
       converted-ranges))

;; output-file-range : connection file-input-port integer integer -> void
;;
;; start must be inclusive; end must be exclusive.
(define (output-file-range conn input start end)
  (file-position input start)
  (define custodian (make-custodian))
  (parameterize ([current-custodian custodian])
    (define limited-input (make-limited-input-port input (- end start) #f))
    (thread
     (lambda _
       (let loop ()
         ;; Every time the limited-input is read from, reset
         ;; the connection timeout and give the client another
         ;; `response-send-timeout` seconds' worth of lease.
         (sync (port-progress-evt limited-input))
         (unless (port-closed? limited-input)
           (reset-connection-response-send-timeout! conn)
           (loop)))))
    (copy-port limited-input (connection-o-port conn))
    (close-input-port limited-input)))

;; convert-http-ranges : (alist-of (U integer #f) (U integer #f))
;;                       integer
;;                    -> (alist-of integer integer)
;;
;; Converts a list of HTTP ranges:
;;
;; into pairs of offsets we can use to read from a file:
;;
;;   - suffix-byte-range-specs are converted to pairs of absolute offsets;
;;   - missing end offsets in byte-range-specs ranges are filled in;
;;   - end offsets are exclusive (as opposed to the inclusive offsets in ranges and the HTTP spec).
;;
;; The HTTP spec recommends that ranges are sent in the order they are specified in the request.
(define (convert-http-ranges ranges total-file-length)
  (define converted
    (filter-map (lambda (range)
                  ; a : (U integer #f)
                  ; b : (U integer #f)
                  ; The original values quoted in the Range header:
                  (define-values (a b)
                    (values (car range)
                            (cdr range)))
                  ; a* : integer
                  ; b* : integer
                  ; Convert inclusive end offsets and suffix ranges:
                  (define-values (a* b*)
                    (cond [(not a) (values (- total-file-length b) total-file-length)]
                          [(not b) (values a total-file-length)]
                          [else    (values a (add1 b))]))
                  ; a** : integer
                  ; b** : integer
                  ; Trim to the size of the file:
                  (define-values (a** b**)
                    (values (max 0 (min total-file-length a*))
                            (max 0 (min total-file-length b*))))
                  ; Get rid of zero-length ranges (including ones that are outside the file length):
                  (if (< a** b**)
                      (cons a** b**)
                      #f))
                ranges))
  (if (null? converted)
      (error 'convert-http-ranges "No satisfiable ranges in ~a/~a." ranges total-file-length)
      converted))

;; make-206-response : integer bytes integer integer (alist-of integer integer) bytes -> basic-response
(define (make-206-response modified-seconds maybe-mime-type total-content-length total-file-length converted-ranges boundary)
  (if (= (length converted-ranges) 1)
      (let ([start (caar converted-ranges)]
            [end   (cdar converted-ranges)])
        (response
         206 #"Partial content"
         modified-seconds
         maybe-mime-type
         (list (make-header #"Accept-Ranges" #"bytes")
               (make-content-length-header total-content-length)
               (make-content-range-header start end total-file-length))
         void))
      (response
       206 #"Partial content"
       modified-seconds
       (bytes-append #"multipart/byteranges; boundary=" boundary)
       (list (make-header #"Accept-Ranges" #"bytes")
             (make-content-length-header total-content-length))
       void)))

;; make-200-response : integer bytes integer -> basic-response
(define (make-200-response modified-seconds maybe-mime-type total-content-length)
  (response
   200 #"OK"
   modified-seconds
   maybe-mime-type
   (list (make-header #"Accept-Ranges" #"bytes")
         (make-content-length-header total-content-length))
   void))

;; make-416-response : integer bytes -> basic-response
(define (make-416-response modified-seconds maybe-mime-type)
  (response
   416 #"Invalid range request"
   modified-seconds
   maybe-mime-type
   null
   void))

;; make-content-length-header : integer -> header
(define (make-content-length-header total-content-length)
  (make-header #"Content-Length" (string->bytes/utf-8 (number->string total-content-length))))

;; make-content-range-header : integer integer integer -> header
;; start must be inclusive; end must be exclusive.
(define (make-content-range-header start end total-file-length)
  (make-header #"Content-Range"
               (string->bytes/utf-8
                (format "bytes ~a-~a/~a" start (sub1 end) total-file-length))))
