#lang scribble/doc
@(require "web-server.rkt"
          (for-label racket/random
                     racket/port
                     racket/serialize))

@title[#:tag "http"]{HTTP: Hypertext Transfer Protocol}

@defmodule[web-server/http]

The @web-server implements many HTTP libraries that are provided by this module.

@;
@;                         ;;            ;;
@;                                       ;;
@;   ;; ;  ;;   ; ;; ;;;   ;; ; ;;;   ;;;;;  ;;   ;; ;
@;   ;;;  ;  ;  ;; ;;  ;   ;; ;;  ;  ;   ;; ;  ;  ;;;
@;   ;;   ;  ;  ;; ;;  ;;  ;; ;;  ;; ;   ;; ;  ;  ;;
@;   ;;  ;;;;;; ;; ;;  ;;  ;; ;;  ;;;;   ;;;;;;;; ;;
@;   ;;   ;     ;; ;;  ;;  ;; ;;  ;; ;   ;; ;     ;;
@;   ;;   ;     ;; ;;  ;;  ;; ;;  ;; ;   ;; ;     ;;
@;   ;;    ;;;  ;; ;;  ;;  ;; ;;  ;;  ;;; ;  ;;;  ;;
@;
@;
@;  Please update typed/web-server/http when you update
@;  web-server/http. See https://github.com/racket/typed-racket.
@;

@; ------------------------------------------------------------
@section[#:tag "request-structs"]{Requests}
@(require (for-label web-server/http/request-structs
                     xml
                     racket/promise
                     racket/match))

@defmodule[web-server/http/request-structs]{

@defstruct[header ([field bytes?]
                   [value bytes?])]{
 Represents a header of @racket[field] to @racket[value].
}

@defproc[(headers-assq [id bytes?] [heads (listof header?)])
         (or/c false/c header?)]{
 Returns the header with a field equal to @racket[id] from @racket[heads] or @racket[#f].
}

@defproc[(headers-assq* [id bytes?] [heads (listof header?)])
         (or/c false/c header?)]{
 Returns the header with a field case-insensitively equal to @racket[id] from @racket[heads] or @racket[#f].

 You almost @bold{always} want to use this, rather than @racket[headers-assq] because Web browsers may send headers with arbitrary casing.
}

@defstruct[binding ([id bytes?])]{Represents a binding of @racket[id].}

@defstruct[(binding:form binding) ([value bytes?])]{
 Represents a form binding of @racket[id] to @racket[value].
}

@defstruct[(binding:file binding) ([filename bytes?]
                                   [headers (listof header?)]
                                   [content bytes?])]{

  Represents the uploading of the file @racket[filename] with the id
  @racket[id] and the content @racket[content], where @racket[headers]
  are the additional headers from the MIME envelope the file was in.
  For example, the @racket[#"Content-Type"] header may be included by
  some browsers.

  See also @racket[binding:file/port-in], an alternative interface
  to file uploads that can be significantly more memory efficient.

  @history[#:changed "1.6"
           @elem{Extended to support a port-based representation:
              see @racket[binding:file/port-in].}]
 }

 @deftogether[
 (@defproc[(binding:file/port-in [binding binding:file/port?])
           input-port?]
   @defproc[(binding:file/port? [v any/c]) boolean?]
   @defproc[(make-binding:file/port [id bytes?]
                                    [filename bytes?]
                                    [headers (listof header?)]
                                    [content-in input-port?])
            binding:file/port?])]{

  The web server can avoid storing uploaded files in memory.
  In particular, a @tech[#:doc '(lib "web-server/scribblings/web-server-internal.scrbl")]{
   safety limits} value can instruct this
  library to offload files to disk if they are larger than some threshold.
  Even for file uploads that are not written to disk,
  the web server initially places the content in an input port,
  rather than a byte-string, so that storage need not be retained
  after the content has been read.

  The port-based interface is exposed to programmers,
  and it can be significantly more memory efficient than
  the byte-string--based interface.
  However, the port-based interface is stateful:
  programmers who use it take responsibility for managing the state
  of the input port.
  Read on for details.

  To maintain compatability, the port-based interface uses a private,
  opaque subtype of @racket[binding:file].
  Instances of this extended type are recognized by the predicate
  @racket[binding:file/port?] and are created using
  @racket[make-binding:file/port], which is like @racket[make-binding:file],
  but takes the file content as an input port rather than a byte string.
  Only @racket[binding:file] instances recognized by @racket[binding:file/port?]
  support @racket[binding:file/port-in].
  The web server uses @racket[make-binding:file/port] when reading
  @racket[request] structures, which is the primary way most programs
  encounter @racket[binding:file] instances:
  however, deserialized instances (see below) and instances constructed
  manually using @racket[make-binding:file] do not support the port-based API.

  It is important to be aware of how @racket[binding:file-content]
  works with port-based instances.
  The first time @racket[binding:file-content] is called on a
  port-based instance @racket[v], it consumes the port's remaining content
  as with @racket[(port->bytes (binding:file/port-in v))],
  memoizes the result for future calls to @racket[binding:file-content],
  and closes the input port.
  This behavior means that:
  @itemlist[
 @item{A given byte of input may be either stored in the
    @racket[binding:file-content] field or read directly by
    from the input port, but never both; and
   }
 @item{If the input port has already been closed directly
    when @racket[binding:file-content] is called for the first time,
    @racket[binding:file-content] will raise an exception.
    }]

  Accessing the @racket[binding:file-content] field indirectly,
  such as by using @racket[match], has the same behavior as calling
  @racket[binding:file-content] directly.
  In particular, calling @racket[serialize] on a @racket[binding:file]
  instance implicitly uses @racket[binding:file-content],
  and deserialized instances are effectively constructed using
  @racket[make-binding:file].

  @history[#:added "1.6"]
 }


@defproc[(bindings-assq [id bytes?]
                        [binds (listof binding?)])
         (or/c false/c binding?)]{
 Returns the binding with an id equal to @racket[id] from @racket[binds] or @racket[#f].
}

@defproc[(bindings-assq-all [id bytes?]
                            [binds (listof binding?)])
         (listof binding?)]{
Like @racket[bindings-assq], but returns a list of all bindings matching @racket[id].
}


@defstruct[request ([method bytes?]
                    [uri url?]
                    [headers/raw (listof header?)]
                    [bindings/raw-promise (promise/c (listof binding?))]
                    [post-data/raw (or/c #f bytes?)]
                    [host-ip string?]
                    [host-port number?]
                    [client-ip string?])]{
 An HTTP @racket[method] request to @racket[uri] from @racket[client-ip]
 to the server at @racket[host-ip]:@racket[host-port] with @racket[headers/raw]
 headers, @racket[bindings/raw] GET and POST queries and @racket[post-data/raw]
 POST data.

 You are @bold{unlikely to need to construct} a request struct.

 @history[#:changed "1.6"
          @elem{Fixed to answer @racket[#f] to @racket[serializable?],
             as all @racket[request] instances contain non-serializable pieces.}]
}

@defproc[(request-bindings/raw [r request?])
         (listof binding?)]{
 Forces @racket[(request-bindings/raw-promise r)].
}

Here is an example typical of what you will find in many applications:
@racketblock[
(define (get-number req)
  (match
    (bindings-assq
      #"number"
      (request-bindings/raw req))
    [(? binding:form? b)
     (string->number
      (bytes->string/utf-8
       (binding:form-value b)))]
    [_
     (get-number (request-number))]))
]

}

@; ------------------------------------------------------------
@section[#:tag "bindings"]{Bindings}
@(require (for-label web-server/http/bindings))

@defmodule[web-server/http/bindings]{

These functions, while convenient, could introduce subtle errors into your
application. Examples: that they are case-insensitive could introduce
an error; if the data submitted is not in UTF-8 format, then the conversion
to a string will fail; if an attacker submits a form field as if it were
a file, when it is not, then the @racket[request-bindings] will hold a
@racket[bytes?] object and your program will error; and, for file uploads
you lose the filename. @bold{Therefore, we recommend against their use, but
they are provided for compatibility with old code.}

@defproc[(request-bindings [req request?])
         (listof (or/c (cons/c symbol? string?)
                       (cons/c symbol? bytes?)))]{
 Translates the @racket[request-bindings/raw] of @racket[req] by
 interpreting @racket[bytes?] as @racket[string?]s, except in the case
 of @racket[binding:file] bindings, whose contents are returned as
 bytes. Ids are then translated into lowercase symbols.
}

@defproc[(request-headers [req request?])
         (listof (cons/c symbol? string?))]{
 Translates the @racket[request-headers/raw] of @racket[req] by
 interpreting @racket[bytes?] as @racket[string?]s. Ids are then
 translated into lowercase symbols.
}

@defproc[(extract-binding/single [id symbol?]
                                 [binds (listof (cons/c symbol? string?))])
         string?]{
 Returns the single binding associated with @racket[id] in the a-list @racket[binds]
 if there is exactly one binding. Otherwise raises @racket[exn:fail].
}

@defproc[(extract-bindings [id symbol?]
                           [binds (listof (cons/c symbol? string?))])
         (listof string?)]{
 Returns a list of all the bindings of @racket[id] in the a-list @racket[binds].
}

@defproc[(exists-binding? [id symbol?]
                          [binds (listof (cons/c symbol? string))])
         boolean?]{
 Returns @racket[#t] if @racket[binds] contains a binding for @racket[id].
 Otherwise, @racket[#f].
}

Here is an example typical of what you will find in many applications:
@racketblock[
(define (get-number req)
  (string->number
   (extract-binding/single
    'number
    (request-bindings req))))
]

}

@; ------------------------------------------------------------
@section[#:tag "response-structs"]{Responses}
@(require (for-label web-server/http/response-structs))

@defmodule[web-server/http/response-structs]{

@deftogether[
 (@defstruct*[response
              ([code response-code/c]
               [message bytes?]
               [seconds real?]
               [mime (or/c #f bytes?)]
               [headers (listof header?)]
               [output (output-port? . -> . any)])]
   @defthing[response-code/c flat-contract?
             #:value (integer-in 100 999)])]{

An HTTP response where @racket[output] produces the body by writing to
the output port. @racket[code] is the response code, @racket[message]
the message, @racket[seconds] the generation time, @racket[mime] the
MIME type of the file, and @racket[headers] are the headers.

If @racket[headers] does not include @litchar{Date},
@litchar{Last-Modified}, or @litchar{Server} headers, then the server
will automatically add them, where @litchar{Date} is based on
@racket[current-seconds], @litchar{Last-Modified} is based on
@racket[seconds], and @litchar{Server} is @litchar{Racket}.

If @racket[headers] does not include @litchar{Content-Type} and
@racket[mime] is not @racket[#f], then @racket[mime] is added as a
@litchar{Content-Type} header.

The server will always replace your @litchar{Connection} header if it
needs to ensure the connection will be closed. (Typically with an
HTTP/1.0 client.)

The server will always puts headers it generates before those in the
@racket[response] structure and guarantees that the headers supplied
appear in the output in the order given. (This is relevant if multiple
occurrences of the same header have a different interpretation by the
client, such as with @litchar{Set-Cookie}.)

Examples:
 @racketblock[
  (response
   301 #"OK"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (λ (op) (write-bytes #"<html><body>Hello, World!</body></html>" op)))
  (response
   301 #"Moved Permanently"
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list (make-header #"Location"
                      #"http://racket-lang.org/download"))
   (λ (op) (write-bytes #"Moved" op)))
  (response
   304 #"Not Modified"
   (current-seconds) #f
   (list (make-header #"Location"
                      #"http://racket-lang.org/download"))
   void)
 ]

@history[#:changed "1.3"
         @elem{Added @racket[response-code/c] and made the
            contracts on @racket[code] and @racket[seconds]
            stronger (rather than accepting @racket[number?]).}
         #:changed "1.2"
         @elem{Contract on @racket[output] weakened to allow @racket[any]
               as the result (instead of demanding @racket[void?]).}]
}

@defproc[(response/full [code response-code/c] [message (or/c #f bytes?)]
                        [seconds real?]
                        [mime (or/c #f bytes?)]
                        [headers (listof header?)] [body (listof bytes?)])
         response?]{
 A constructor for responses where @racket[body] is the response body.

 Example:
 @racketblock[
  (response/full
   301 #"Moved Permanently"
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list (make-header #"Location"
                      #"http://racket-lang.org/download"))
   (list #"<html><body><p>"
         #"Please go to <a href=\""
         #"http://racket-lang.org/download"
         #"\">here</a> instead."
         #"</p></body></html>"))
 ]

 If @racket[message] is not supplied or is @racket[#f], a status message will be inferred based on @racket[code]. Status messages will be inferred based on RFCs 7231 (``Hypertext Transfer Protocol (HTTP/1.1): Semantics and Content'') and 7235 (``Hypertext Transfer Protocol (HTTP/1.1): Authentication''). These are the following:

  @tabular[#:sep @hspace[1]
   (list (list @bold{Code} @bold{Message})
   (list "100" "Continue")
   (list "101" "Switching Protocols")

   (list "200" "OK")
   (list "201" "Created")
   (list "202" "Accepted")
   (list "203" "Non-Authoritative Information")
   (list "204" "No Content")
   (list "205" "Reset Content")

   (list "300" "Multiple Choices")
   (list "301" "Moved Permanently")
   (list "302" "Found")
   (list "303" "See Other")
   (list "305" "Use Proxy")
   (list "307" "Temporary Redirect")

   (list "400" "Bad Request")
   (list "401" "Unauthorized")
   (list "402" "Payment Required")
   (list "403" "Forbidden")
   (list "404" "Not Found")
   (list "405" "Method Not Allowed")
   (list "406" "Not Acceptable")
   (list "407" "Proxy Authentication Required")
   (list "408" "Request Timeout")
   (list "409" "Conflict")
   (list "410" "Gone")
   (list "411" "Length Required")
   (list "413" "Payload Too Large")
   (list "414" "URI Too Long")
   (list "415" "Unsupported Media Type")
   (list "417" "Expectation Failed")
   (list "426" "Upgrade Required")

   (list "500" "Internal Server Error")
   (list "501" "Not Implemented")
   (list "502" "Bad Gateway")
   (list "503" "Service Unavailable")
   (list "504" "Gateway Timeout")
   (list "505" "HTTP Version Not Supported"))]

 @history[#:changed "1.3"
          @elem{Updated contracts on @racket[code] and @racket[seconds]
             as with @racket[response].}]
 @history[#:changed "1.4"
         @elem{Contract on @racket[message] relaxed to allow both @racket[#f] and a @racket[bytes?], with a default of @racket[#f]. Previously, @racket[bytes?] was required, and had a default of @racket[#"Okay"].}]
}

@defproc[(response/output [output (-> output-port? any)]
                          [#:code code number? 200]
                          [#:message message (or/c false/c bytes?) #f]
                          [#:seconds seconds number? (current-seconds)]
                          [#:mime-type mime-type (or/c bytes? #f) TEXT/HTML-MIME-TYPE]
                          [#:headers headers (listof header?) '()])
         response?]{
Equivalent to
@racketblock[(response code message seconds mime-type headers output)], with the understanding that if @racket[message] is missing, it will be inferred from @racket[code] using the association between status codes and messages found in RFCs 7231 and 7235. See the documentation for @racket[response/full] for the table of built-in status codes.

@history[#:changed "1.4"
         @elem{Contract on @racket[message] relaxed to allow both @racket[#f] and a @racket[bytes?], with a default of @racket[#f]. Previously, @racket[bytes?] was required, and had a default of @racket[#"Okay"].}
         #:changed "1.3"
         @elem{Updated contracts on @racket[code] and @racket[seconds]
            as with @racket[response].}
         #:changed "1.2"
         @elem{Contract on @racket[output] weakened to allow @racket[any]
               as the result (instead of demanding @racket[void?]).}]
}

@defproc[(response/empty [#:code code number? 200]
                         [#:message message (or/c false/c bytes?) #f]
                         [#:cookies cookies (listof cookie?) '()]
                         [#:seconds seconds number? (current-seconds)]
                         [#:headers headers (listof header?) '()])
         response?]{
Generates a response with an empty body. The usual @tt{Content-Type} header will be absent, unless passed in via @racket[headers]. Equivalent to
@racketblock[(response code message seconds #f headers (λ (o) (write-bytes #"" o)))], with the understanding that if @racket[message] is missing (or @racket[#f]), it will be inferred from @racket[code] using the association between status codes and messages found in RFCs 7231 and 7235. See the documentation for @racket[response/full] for the table of built-in status codes.

@history[#:added "1.6"]
}

@defthing[TEXT/HTML-MIME-TYPE bytes?]{Equivalent to @racket[#"text/html; charset=utf-8"].}

@defthing[APPLICATION/JSON-MIME-TYPE bytes?]{Equivalent to @racket[#"application/json; charset=utf-8"].}

@warning{If you include a Content-Length header in a response that is inaccurate, there @bold{will be an error} in
transmission that the server @bold{will not catch}.}

}

@; ------------------------------------------------------------
@section[#:tag "cookie"]{Placing Cookies}

@(require (for-label (except-in net/cookies/server
                                make-cookie)
                     net/cookies/common
                     web-server/servlet
                     web-server/http/xexpr
                     web-server/http/redirect
                     web-server/http/request-structs
                     web-server/http/response-structs
                     web-server/http/cookie))

@(define rfc6265
   (hyperlink "https://tools.ietf.org/html/rfc6265.html"
              "RFC 6265"))

@defmodule[web-server/http/cookie]{
 This module provides functions to create cookies and responses that set them.

 @defproc[(make-cookie [name cookie-name?]
                       [value cookie-value?]
                       [#:comment comment any/c #f]
                       [#:domain domain (or/c domain-value? #f) #f]
                       [#:max-age max-age (or/c (and/c integer? positive?) #f) #f]
                       [#:path path (or/c path/extension-value? #f) #f]
                       [#:expires expires (or/c date? string? #f) #f]
                       [#:secure? secure? any/c #f]
                       [#:http-only? http-only? any/c #f]
                       [#:extension extension (or/c path/extension-value? #f) #f])
          cookie?]{
   Constructs a cookie with the appropriate fields.

   This is a wrapper around @racket[make-cookie] from @racketmodname[net/cookies/server]
   for backwards compatibility. The @racket[comment] argument is ignored.
   If @racket[expires] is given as a string, it should match
   @link["https://tools.ietf.org/html/rfc7231#section-7.1.1.2"]{RFC 7231, Section 7.1.1.2},
   in which case it will be converted to a @racket[date?] value.
   If conversion fails, an @racket[exn:fail:contract?] is raised.

   @history[
 #:changed "1.3"
 @elem{Added support for @rfc6265 via @racketmodname[net/cookies/server].
    Enforce stronger contracts on string-valued arguments.
    Allow @racket[expires] to be a @racket[date?]
    and allow @racket[secure] to be @racket[any/c]
    (rather than @racket[boolean?]).
    Forbid @racket[0] for @racket[max-age].
    Support @racket[http-only?] and @racket[extension] arguments.
    Ignore @racket[comment].
    }]
 }

 @defproc[(cookie->header [c cookie?]) header?]{
  Constructs a header that sets the cookie.
 }

 Examples:
 @racketblock[
  (define time-cookie
    (make-cookie "time" (number->string (current-seconds))))
  (define id-cookie
    (make-cookie "id" "joseph" #:secure? #t))

  (redirect-to
   "http://localhost/logged-in"
   see-other
   #:headers
   (map cookie->header
        (list time-cookie id-cookie)))

  (send/suspend
   (lambda (k-url)
     (response/xexpr
      #:cookies (list time-cookie id-cookie)
      `(html (head (title "Cookie Example"))
             (body (h1 "You're cookie'd!"))))))
 ]
}

@; ------------------------------------------------------------
@section[#:tag "id-cookie"]{Authenticated Cookies}

@(require (for-label web-server/http/id-cookie))
@defmodule[web-server/http/id-cookie]{

Cookies are useful for storing information of user's browsers and
particularly useful for storing identifying information for
authentication, sessions, etc. However, there are inherent
difficulties when using cookies as authenticators, because cookie data
is fully controlled by the user, and thus cannot be trusted.

This module provides functions for creating and verifying
authenticated cookies that are intrinsically timestamped. It is based
on the algorithm proposed by the
@link["https://pdos.csail.mit.edu/archive/cookies/"]{MIT Cookie Eaters}: if you store
the data @racket[_data] at time @racket[_authored-seconds], then the
user will receive @litchar{digest&authored-seconds&data}, where
@racket[_digest] is an HMAC-SHA1 digest of @racket[_authored-seconds]
and @racket[_data], using an arbitrary secret key. When you receive a
cookie, it will reverify this digest and check that the cookie's
@racket[_authored-seconds] is not after a timeout period, and only
then return the cookie data to the program.

The interface represents the secret key as a byte string.
@bold{For security, this should be created using cryptographic-quality randomness.}
A convenient purely Racket-based option is @racket[make-secret-salt/file],
which is implemented using @racket[crypto-random-bytes].
You can also generate random bytes using something like OpenSSL or @tt{/dev/random}:
 @link["https://www.madboa.com/geek/openssl/#random-data"]{this FAQ} lists a few options.

 @defproc*[([(make-id-cookie
              [name cookie-name?]
              [value cookie-value?]
              [#:key secret-salt bytes?]
              [#:path path (or/c path/extension-value? #f) #f]
              [#:expires expires (or/c date? #f) #f]
              [#:max-age max-age
               (or/c (and/c integer? positive?) #f) #f]
              [#:domain domain (or/c domain-value? #f) #f]
              [#:secure? secure? any/c #f]
              [#:http-only? http-only? any/c #f]
              [#:extension extension
               (or/c path/extension-value? #f) #f])
             cookie?]
            [(make-id-cookie
              [name cookie-name?]
              [secret-salt bytes?]
              [value cookie-value?]
              [#:path path (or/c path/extension-value? #f) #f]
              [#:expires expires (or/c date? #f) #f]
              [#:max-age max-age
               (or/c (and/c integer? positive?) #f) #f]
              [#:domain domain (or/c domain-value? #f) #f]
              [#:secure? secure? any/c #f]
              [#:http-only? http-only? any/c #t]
              [#:extension extension
               (or/c path/extension-value? #f) #f])
             cookie?])]{
  Generates an authenticated cookie named @racket[name] containing @racket[value],
  signed with @racket[secret-salt].

  The calling conventions allow @racket[secret-salt] to be given either as a keyword
  argument (mirroring the style of @racket[make-cookie]) or a by-position argument
  (for compatibility with older versions of this library).

  The other arguments are passed to @racket[make-cookie]; however, note that the
  default value for @racket[http-only?] is @racket[#t]. Users will also likely
  want to set @racket[secure?] to @racket[#t] when using HTTPS.

  @history[
 #:changed "1.3"
 @elem{Added support for @rfc6265 as with @racket[make-cookie],
    including adding the optional arguments
    @racket[expires], @racket[max-age], @racket[domain],
    @racket[secure], @racket[extension],
    and @racket[http-only?] (which is @racket[#true] by default).
    Allowed @racket[secret-salt] to be given with the keyword
    @racket[#:key] instead of by position.}
 #:changed "1.6"
 @elem{Changed to accept any @racket[cookie-name?] or @racket[cookie-value?]
    (rather than only strings) for the @racket[name] and @racket[value] arguments,
    respectively, for consistency with @racket[make-cookie].
    Fixed a bug that had incorrectly truncated cookie signatures:
    note that previous versions of this library will not recognize cookies
    created by the fixed @racket[make-id-cookie] as validly signed, and vice versa.
    }]
 }

 @defproc*[([(request-id-cookie [request request?]
                                [#:name name cookie-name?]
                                [#:key secret-salt bytes?]
                                [#:timeout timeout real? +inf.0]
                                [#:shelf-life shelf-life real? +inf.0])
             (or/c #f (and/c string? cookie-value?))]
            [(request-id-cookie [name cookie-name?]
                                [secret-salt bytes?]
                                [request request?]
                                [#:timeout timeout real? +inf.0]
                                [#:shelf-life shelf-life real? +inf.0])
             (or/c #f (and/c string? cookie-value?))])]{
  Extracts the first authenticated cookie named @racket[name]
  that was previously signed with @racket[secret-salt]
  from @racket[request], with the allowable age of the cookie
  is controlled by @racket[shelf-life] and @racket[timeout] as with
  @racket[valid-id-cookie?].

  If no valid cookie is available, returns @racket[#f].

  @history[#:changed "1.3"
           @elem{Added @racket[shelf-life] argument and
              support for giving @racket[name] and @racket[secret-salt]
              by keyword instead of by position.
              Added support for @rfc6265 as with @racket[make-cookie].}
            #:changed "1.6"
           @elem{Changed @racket[name] argument to accept any @racket[cookie-name?]
              as with @racket[make-id-cookie].
              Corrected the documented contract for the @racket[timeout] argument.
              Fixed a bug that had incorrectly truncated cookie signatures:
              note that the fixed @racket[request-id-cookie] will reject cookies
              created by previous versions of this library, and vice versa.
              }]
 }

@defproc[(valid-id-cookie? [cookie any/c]
                           [#:name name cookie-name?]
                           [#:key secret-salt bytes?]
                           [#:timeout timeout real? +inf.0]
                           [#:shelf-life shelf-life real? +inf.0])
         (or/c #f (and/c string? cookie-value?))]{
  Recognizes authenticated cookies named @racket[name] that were
  previously signed with @racket[secret-salt].
  Values satisfying either @racket[cookie?]
  or @racket[client-cookie?] can be recognized.

  The @racket[shelf-life] specifies the maximum age of the cookie
  in seconds. Cookies created more than @racket[shelf-life] seconds
  ago will not be considered valid.
  The default value, @racket[+inf.0], permits all properly named and
  signed cookies.

  Counterintuitively,
  the @racket[timeout] argument requires that the cookie have been
  created @italic{before} a certain moment in time: in other words,
  it requires that the cookie be @italic{older} than a certain age.
  This is not usually what you want to restrict.
  Specifically, @racket[valid-id-cookie?] tests that
  @racket[(authored . <= . timeout)], where @racket[authored] is the
  value returned by @racket[(current-seconds)] when the cookie was created.
  The default value, @racket[+inf.0], permits all properly named and
  signed cookies.

  @history[#:added "1.3"
           #:changed "1.6"
           @elem{Changed @racket[name] argument to accept any @racket[cookie-name?]
              as with @racket[make-id-cookie].
              Corrected the documented contract for the @racket[timeout] argument.
              Fixed a bug that had incorrectly truncated cookie signatures:
              note that the fixed @racket[valid-id-cookie?] will reject cookies
              created by previous versions of this library, and vice versa.
              }]
 }

 @defproc[(logout-id-cookie [name cookie-name?]
                            [#:path path (or/c #f string?) #f]
                            [#:domain domain (or/c domain-value? #f) #f])
          cookie?]{
  Generates a cookie named @racket[name] that is not validly authenticated
  and expires in the past.

  This will cause non-malicious browsers to overwrite a previously set
  cookie. If you use authenticated cookies for login information, you
  could send this to cause a ``logout.'' However, malicious browsers do
  not need to respect such an overwrite. Therefore, this is not an
  effective way to implement timeouts or protect users on
  public (i.e. possibly compromised) computers. The only way to securely
  logout on the compromised computer is to have server-side state
  keeping track of which cookies (sessions, etc.) are invalid. Depending
  on your application, it may be better to track live sessions or dead
  sessions, or never set cookies to begin with and just use
  (stateful) continuations, which you can revoke with @racket[send/finish].

  @history[#:changed "1.3"
           @elem{Added support for @rfc6265 as with @racket[make-cookie],
              including adding the @racket[domain] argument.}
           #:changed "1.6"
           @elem{Fixed to accept any @racket[cookie-name?] for the @racket[name]
              argument, as was previously documented.}]
 }

 @defproc[(make-secret-salt/file [secret-salt-path path-string?])
          bytes?]{
  Extracts the bytes from @racket[secret-salt-path]. If
  @racket[secret-salt-path] does not exist, then it is created and
  initialized with 128 cryptographic-quality random bytes
  from @racket[crypto-random-bytes].

  @history[#:changed "1.3"
           @elem{Changed to use cryptographic-quality randomness
              to initialize @racket[secret-salt-path].}]
 }
}

@; ------------------------------------------------------------
@section[#:tag "cookie-parse"]{Extracting Cookies}

@(require (for-label web-server/http/cookie-parse
                     web-server/http/xexpr
                     net/url
                     racket/list))
@defmodule[web-server/http/cookie-parse]{
 @defstruct[client-cookie
            ([name (and/c string? cookie-name?)]
             [value (and/c string? cookie-value?)]
             [domain (or/c #f domain-value?)]
             [path (or/c #f path/extension-value?)])]{

  While server cookies are represented with @racket[cookie?]s, cookies
  that come from the client are represented with a
  @racket[client-cookie] structure.
 }

 @defproc[(request-cookies [req request?])
          (listof client-cookie?)]{
  Extracts the cookies from @racket[req]'s headers.

  @history[#:changed "1.3"
           @elem{Added support for @rfc6265 via
              @racketmodname[net/cookies/common].}]
 }

 Examples:
 @racketblock[
  (define (start req)
    (define cookies (request-cookies req))
    (define id-cookie
      (findf (lambda (c)
               (string=? "id" (client-cookie-name c)))
             cookies))
    (if id-cookie
        (hello (client-cookie-value id-cookie))
        (redirect-to
         (url->string (request-uri req))
         see-other
         #:headers
         (list
          (cookie->header (make-cookie "id" "joseph"))))))

   (define (hello who)
     (response/xexpr
      `(html (head (title "Hello!"))
             (body
              (h1 "Hello "
                  ,who)))))
 ]
}

@; ------------------------------------------------------------
@section[#:tag "redirect"]{Redirect}
@(require (for-label web-server/http/redirect
                     web-server/private/util
                     (only-in racket/string non-empty-string?)))

@defmodule[web-server/http/redirect]{

@deftogether[
 (@defproc[(redirect-to [uri non-empty-string?]
                        [status redirection-status? temporarily]
                        [#:headers headers (listof header?) '()])
           response?]
   @defproc[(redirection-status? [v any/c]) boolean?]
   @defthing[temporarily redirection-status?]
   @defthing[temporarily/same-method redirection-status?]
   @defthing[see-other redirection-status?]
   @defthing[permanently redirection-status?])]{
  The function @racket[redirect-to]
  generates an HTTP response that redirects the browser to @racket[uri],
  while including the @racket[headers] in the response.
  The @racket[status] argument is a @deftech{redirection status}
  value, which determines the specific type of HTTP redirect to be used.

  The default @tech{redirection status}, @racket[temporarily],
  is preserved for backwards compatibility:
  new code should usually use either @racket[temporarily/same-method]
  or @racket[see-other], instead.
  The @racket[temporarily] @tech{redirection status} corresponds to
  @hyperlink["https://tools.ietf.org/html/rfc7231#section-6.4.3"]{
   @litchar{302 Found}}.
  Unfortunately, browsers have not implemented this status consistently
  for methods other than @litchar{GET} and (in practice, with all but some
  very old browsers) @litchar{POST}.

  The @racket[temporarily/same-method] @tech{redirection status}
  uses @hyperlink["https://tools.ietf.org/html/rfc7231#section-6.4.7"]{
   @litchar{307 Temporary Redirect}}.
  This redirects the browser to @racket[uri] using the same HTTP method
  as the original request.

  The @racket[see-other] @tech{redirection status} corresponds to
  @hyperlink["https://tools.ietf.org/html/rfc7231#section-6.4.4"]{
   @litchar{303 See Other}}.
  It is most often used to implement the @deftech{Post-Redirect-Get}
  pattern: as a response to a request using @litchar{POST} or
  another HTTP method with side-effects, it causes the browser to
  perform a @litchar{GET} or @litchar{HEAD} request for @racket[uri],
  which gives a response to the original @litchar{POST} request.
  This prevents the @onscreen{Back} and @onscreen{Refresh} buttons
  from duplicating effects, such as making a purchase or
  adding items to a database.
  The web server provides @racket[redirect/get] for added convenience
  with @tech{Post-Redirect-Get}.

  The @racket[permanently] @tech{redirection status} uses the HTTP status
  @hyperlink["https://tools.ietf.org/html/rfc7231#section-6.4.2"]{
   @litchar{301 Moved Permanently}}.
  It is like @racket[temporarily], except that, as the name suggests,
  it signifies that the move is permanent and that search engines,
  for example, should use @racket[uri] instead of the URI of the
  original request.
  Unfortunately, @racket[permanently] is also like @racket[temporarily]
  in that browsers have implemented it inconsistently for
  methods other than @litchar{GET} and @litchar{HEAD}:
  in particular, @hyperlink["https://tools.ietf.org/html/rfc7231#section-6.4.2"]{
   RFC 7231} permits that, ``for historical reasons, a user agent @bold{may}
  change the request method from @litchar{POST} to @litchar{GET} for the subsequent request.
  When it is important to ensure that the request to @racket[uri] use the same method,
  there are some possible alternatives:
  @itemlist[
 @item{RFC 7231 suggests using @litchar{307 Temporary Redirect},
    i.e. @racket[temporarily/same-method].
    This has the disadvantage that search engines and others won't
    update references to the old URI.}
 @item{@hyperlink["https://tools.ietf.org/html/rfc7538"]{RFC 7538}
    specifies a new HTTP status, @litchar{308 Permanent Redirect},
    which forbids changing the request method, analogously to
    @litchar{307 Temporary Redirect}.
    However, the RFC also highlights some important
    @hyperlink["https://tools.ietf.org/html/rfc7538#section-4"]{
     deployment considerations} for this status.
    In particular, older browsers---including, as of this writing,
    some that remain in
    @hyperlink["https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/308#Browser_compatibility"]{
     relatively common use}---do not understand this status and will
    fall back to the semantics of
    @hyperlink["https://tools.ietf.org/html/rfc7231#section-6.4.1"]{
     @litchar{300 Multiple Choices}}, which is often undesirable.}
 @item{The application can note the method of the original request
    and use @racket[permanently] for @litchar{GET} and @litchar{HEAD} requests
    or one of the other alternatives for other methods.}]

  Example:
  @racket[(redirect-to "http://www.add-three-numbers.com" permanently)]
 }

 @history[#:changed "1.3"
          @elem{Added @racket[temporarily/same-method].}]
}

@; ------------------------------------------------------------
@section[#:tag "basic-auth"]{Basic Authentication}
@(require (for-label web-server/http/response-structs
                     web-server/http/basic-auth))

@defmodule[web-server/http/basic-auth]{

An implementation of HTTP Basic Authentication.

@defproc[(make-basic-auth-header [realm string?])
         header?]{
 Returns a header that instructs the Web browser to request a username and password from the client using
 Basic authentication with @racket[realm] as the realm.
}

@defproc[(request->basic-credentials [req request?])
         (or/c false/c (cons/c bytes? bytes?))]{
 Returns a pair of the username and password from the authentication
 header in @racket[req] if they are present, or @racket[#f].
}

Example:
@racketmod[
web-server/insta

(define (start req)
  (match (request->basic-credentials req)
    [(cons user pass)
     (response/xexpr
      `(html (head (title "Basic Auth Test"))
             (body (h1 "User: " ,(bytes->string/utf-8 user))
                   (h1 "Pass: " ,(bytes->string/utf-8 pass)))))]
    [else
     (response
      401 #"Unauthorized" (current-seconds) TEXT/HTML-MIME-TYPE
      (list
       (make-basic-auth-header
        (format "Basic Auth Test: ~a" (gensym))))
      void)]))
]
}

@; ------------------------------------------------------------
@section[#:tag "digest-auth"]{Digest Authentication}
@(require (for-label web-server/http/digest-auth
                     web-server/http/xexpr
                     web-server/http/response-structs
                     racket/pretty))

@defmodule[web-server/http/digest-auth]{

An implementation of HTTP Digest Authentication.

@defproc[(make-digest-auth-header [realm string?] [private-key string?] [opaque string?])
         header?]{
 Returns a header that instructs the Web browser to request a username and password from the client
 using Digest authentication with @racket[realm] as the realm, @racket[private-key] as the server's
 contribution to the nonce, and @racket[opaque] as the opaque data passed through the client.
}

@defproc[(request->digest-credentials [req request?])
         (or/c false/c (listof (cons/c symbol? string?)))]{
 Returns the Digest credentials from @racket[req] (if they appear) as an association list.
}

@defthing[username*realm->password/c contract?]{
 Used to look up the password for a user is a realm.

 Equivalent to @racket[(string? string? . -> . string?)].
}

@defthing[username*realm->digest-HA1/c contract?]{
 Used to compute the user's secret hash.

 Equivalent to @racket[(string? string? . -> . bytes?)].
}

@defproc[(password->digest-HA1 [lookup-password username*realm->password/c])
         username*realm->digest-HA1/c]{
 Uses @racket[lookup-password] to find the password, then computes the
 secret hash of it.
}

@defproc[(make-check-digest-credentials [lookup-HA1 username*realm->digest-HA1/c])
         (string? (listof (cons/c symbol? string?)) . -> . boolean?)]{
 Constructs a function that checks whether particular Digest credentials
 (the second argument of the returned function) are correct given the
 HTTP method provided as the first argument and the secret hash computed
 by @racket[lookup-HA1].

 This is will result in an exception if the Digest credentials are
 missing portions.
}

Example:
@racketmod[
web-server/insta
(require racket/pretty)

(define private-key "private-key")
(define opaque "opaque")

(define (start req)
  (match (request->digest-credentials req)
    [#f
     (response
      401 #"Unauthorized" (current-seconds) TEXT/HTML-MIME-TYPE
      (list (make-digest-auth-header
             (format "Digest Auth Test: ~a" (gensym))
             private-key opaque))
      void)]
    [alist
     (define check
       (make-check-digest-credentials
        (password->digest-HA1 (lambda (username realm) "pass"))))
     (define pass?
       (check "GET" alist))
     (response/xexpr
      `(html (head (title "Digest Auth Test"))
             (body
              (h1 ,(if pass? "Pass!" "No Pass!"))
              (pre ,(pretty-format alist)))))]))
]
}

@; ------------------------------------------------------------
@section[#:tag "xexpr"]{X-expression Support}
@(require (for-label web-server/http/xexpr
                     xml))

@defmodule*/no-declare[(web-server/http/xexpr)]{}

@declare-exporting[web-server/http/xexpr web-server]

@defproc[(response/xexpr [xexpr xexpr/c]
                         [#:code code response-code/c 200]
                         [#:message message (or/c #f bytes?) #f]
                         [#:seconds seconds real? (current-seconds)]
                         [#:mime-type mime-type (or/c #f bytes?) TEXT/HTML-MIME-TYPE]
                         [#:headers headers (listof header?) empty]
                         [#:cookies cookies (listof cookie?) empty]
                         [#:preamble preamble bytes? #""])
         response?]{
 Equivalent to
 @racketblock[
 (response/full
  code message seconds mime-type
  (append headers (map cookie->header cookies))
  (list preamble (string->bytes/utf-8 (xexpr->string xexpr))))
 ]

 This is a viable function to pass to @racket[set-any->response!].

 See the documentation for @racket[response/full] to see how @racket[#f] is handled for @racket[message].

@history[#:changed "1.4"
         @elem{Contract on @racket[message] relaxed to allow both @racket[#f] and @racket[bytes?], with a default of @racket[#f]. Previously, @racket[bytes?] was required, and had a default of @racket[#"Okay"].}
         #:changed "1.3"
          @elem{Updated contracts on @racket[code] and @racket[seconds]
             as with @racket[response].}]
}

@section[#:tag "json"]{JSON Support}
@(require (for-label web-server/http/json
                     json))

@defmodule*/no-declare[(web-server/http/json)]{}

@declare-exporting[web-server/http/json json]

JSON is a widely used data format for the web. Racket's JSON
library meets the web server with @racket[response/jsexpr],
which is for JSON what @racket[response/xexpr] is for XML.

@defproc[(response/jsexpr [jsexpr jsexpr?]
                          [#:code code response-code/c 200]
                          [#:message message (or/c #f bytes?) #f]
                          [#:seconds seconds real? (current-seconds)]
                          [#:mime-type mime-type (or/c #f bytes?) APPLICATION/JSON-MIME-TYPE]
                          [#:headers headers (listof header?) empty]
                          [#:cookies cookies (listof cookie?) empty])
         response?]{
 Equivalent to
 @racketblock[
 (response/full
  code message seconds mime-type
  (append headers (map cookie->header cookies))
  (list (jsexpr->bytes jsexpr)))
 ]

 See the documentation for @racket[response/full] to see how @racket[message], if @racket[#f], is turned into a @racket[bytes?].

@history[#:added "1.5"]
}
