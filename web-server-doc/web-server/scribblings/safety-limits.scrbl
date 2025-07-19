#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "safety-limits"]{Safety Limits}
@defmodule[web-server/safety-limits]

@(require (for-label web-server/private/dispatch-server-unit
                     web-server/private/dispatch-server-sig
                     web-server/private/util
                     web-server/private/connection-manager
                     web-server/web-server
                     web-server/web-server-unit
                     web-server/web-config-sig
                     web-server/safety-limits
                     web-server/lang/stuff-url
                     web-server/http/request-structs
                     net/tcp-sig
                     racket/async-channel
                     racket/tcp
                     web-server/web-server-sig))

@deftogether[
 (@defproc[(safety-limits? [v any/c]) boolean?]
   @defproc[(make-safety-limits
             [#:max-concurrent max-concurrent positive-count/c 10000]
             [#:max-waiting max-waiting exact-nonnegative-integer? 511]
             [#:request-read-timeout request-read-timeout timeout/c 60]
             [#:max-request-line-length max-request-line-length nonnegative-length/c
              (code:line (* 8 1024) (code:comment #,(elem "8 KiB")))]
             [#:max-request-headers max-request-headers nonnegative-length/c 100]
             [#:max-request-header-length max-request-header-length nonnegative-length/c
              (code:line (* 8 1024) (code:comment #,(elem "8 KiB")))]
             [#:max-request-body-length max-request-body-length nonnegative-length/c
              (code:line (* 1 1024 1024) (code:comment #,(elem "1 MiB")))]
             [#:max-form-data-parts max-form-data-parts nonnegative-length/c
              (+ max-form-data-fields max-form-data-files)]
             [#:max-form-data-header-length max-form-data-header-length nonnegative-length/c
              (code:line (* 8 1024) (code:comment #,(elem "8 KiB")))]
             [#:max-form-data-files max-form-data-files nonnegative-length/c 100]
             [#:max-form-data-file-length max-form-data-file-length nonnegative-length/c
              (code:line (* 10 1024 1024) (code:comment #,(elem "10 MiB")))]
             [#:form-data-file-memory-threshold form-data-file-memory-threshold nonnegative-length/c
              (code:line (* 1 1024 1024) (code:comment #,(elem "1 MiB")))]
             [#:max-form-data-fields max-form-data-fields nonnegative-length/c 100]
             [#:max-form-data-field-length max-form-data-field-length nonnegative-length/c
              (code:line (* 8 1024) (code:comment #,(elem "8 KiB")))]
             [#:response-timeout response-timeout timeout/c 60]
             [#:response-send-timeout response-send-timeout timeout/c 60]
             [#:shutdown-grace-period shutdown-grace-period (or/c #f timeout/c) #f])
            safety-limits?]
   @defthing[nonnegative-length/c flat-contract?
             #:value (or/c exact-nonnegative-integer? +inf.0)]
   @defthing[positive-count/c flat-contract?
             #:value (or/c exact-positive-integer? +inf.0)]
   @defthing[timeout/c flat-contract?
             #:value (>=/c 0)])]{
 The web server uses opaque @deftech{safety limits} values, recognized
 by the predicate @racket[safety-limits?], to encapsulate
 policies for protection against misbehaving or malicious clients and servlets.
 Construct @tech{safety limits} values using @racket[make-safety-limits],
 which supplies reasonably safe default policies that should work for most applications.
 See the @elemref["safety-limits-porting"]{compatability note} and
  @racket[make-unlimited-safety-limits] for further details.

 The arguments to @racket[make-safety-limits] are used as follows:
 @itemlist[
 @item{The @racket[max-concurrent] argument limits the number of open
   concurrent connections to the server.  Once the limit is reached, new
   connections are queued at the TCP level (see @racket[max-waiting])
   until existing connections finish or time out.
   }
 @item{The @racket[max-waiting] argument is passed to @racket[tcp-listen]
   to specify the maximum number of client connections that can be waiting for acceptance.
   When @racket[max-waiting] clients are waiting for acceptance, no new client connections can be made.
   }
 @item{The @racket[request-read-timeout] limits how long, in seconds,
   the standard @sigelem[dispatch-server-config*^ read-request] implementation
   (e.g. from @racket[serve] or @racket[web-server@])
   will wait for request data to come in from the client
   before it closes the connection.
   If you need to support large file uploads over slow connections,
   you may need to adjust this value.
   }
 @item{The @racket[max-request-line-length] limits the length (in bytes) of the
   the first line of an HTTP request (the ``request line''),
   which specifies the request method, path, and protocol version.
   Requests with a first line longer than @racket[max-request-line-length]
   are rejected by the standard @sigelem[dispatch-server-config*^ read-request]
   implementation (e.g. from @racket[serve] or @racket[web-server@]).
   Increase this if you have very long URLs, but see also @racket[is-url-too-big?].
   }
 @item{The @racket[max-request-headers] and @racket[max-request-header-length]
   arguments limit the number of headers allowed per HTTP request
   and the length, in bytes, of an individual request header, respectively.
   Requests that exceed these limits are rejected by the standard
   @sigelem[dispatch-server-config*^ read-request]
   implementation (e.g. from @racket[serve] or @racket[web-server@]).
   }
 @item{The @racket[max-request-body-length] limits the size, in bytes,
   of HTTP request bodies---but it does not apply to multipart (file upload)
   requests: see @racket[max-form-data-files] and related limits, below.
   Requests with bodies longer than @racket[max-request-body-length]
   are rejected by the standard @sigelem[dispatch-server-config*^ read-request]
   implementation (e.g. from @racket[serve] or @racket[web-server@]).
   }
 @item{The @racket[max-form-data-files], @racket[max-form-data-fields],
   @racket[max-form-data-file-length], @racket[max-form-data-field-length],
   @racket[max-form-data-parts], @racket[form-data-file-memory-threshold],
   and @racket[max-form-data-header-length] arguments control the handling of
   @tt{multipart/form-data} (file upload) requests by the standard
   @sigelem[dispatch-server-config*^ read-request]
   implementation (e.g. from @racket[serve] or @racket[web-server@]).

   The number of files and non-file ``fields'' per request are limited by
   @racket[max-form-data-files] and @racket[max-form-data-fields], respectively,
   and @racket[max-form-data-file-length] and @racket[max-form-data-field-length]
   limit the length, in bytes, of an individual file or non-file field.
   Additionally, the total number of ``parts,'' which includes both files and fields,
   must not exceed @racket[max-form-data-parts].
   Requests that exceed these limits are rejected.

   Files longer than @racket[request-file-memory-threshold], in bytes,
   are automatically offloaded to disk as temporary files
   to avoid running out of memory.

   The @racket[max-form-data-header-length] argument limits the length of a header
   for an individual part (file or field).
   Since such headers are already tightly constrained by
   @hyperlink["https://tools.ietf.org/html/rfc7578#section-4.8"]{RFC 7578 §4.8.},
   it should be especially rare to need to increase this limit,
   but doing so could allow for exceptionally long file or field names.
   }
 @item{The @racket[response-timeout] and @racket[response-send-timeout]
   arguments limit the time for which individual request handlers
   (as in @sigelem[dispatch-server-config*^ dispatch]) are allowed to run.

   The @racket[response-timeout] specifies the maximum time, in seconds,
   that a handler is allowed to run after the request has been read
   before it writes its first byte of response data.
   If no data is written within this time limit, the connection is killed.

   The @racket[response-send-timeout] specifies the maximum time, in seconds,
   that the server will wait for a chunk of response data.
   Each time a chunk of data is sent to the client, this timeout resets.
   If your application uses streaming responses or long polling,
   either adjust this value or make sure that your request handler sends
   data periodically, such as a no-op, to avoid hitting this limit.
   }
 @item{The @racket[shutdown-grace-period] argument controls how long,
   during shutdown, the server will wait for in-flight requests to
   finish before stopping. If @racket[#f], in-flight requests are killed
   immediately. Otherwise, the server stops accepting new connections
   and waits until either all in-flight requests complete, or the grace
   period passes, at which point it shuts down its custodian.}
 ]


 @elemtag["safety-limits-porting"]{@bold{Compatibility note:}}
 The @tech{safety limits} type may be extended in the future to provide
 additional protections.
 Creating @tech{safety limits} values with @racket[make-safety-limits]
 will allow applications to take advantage of reasonable default values
 for any new limits that are added.
 However, adding new limits does have the potential to break some existing
 applications: as an alternative, the @racket[make-unlimited-safety-limits]
 constructor uses default values that avoid imposing any limits that
 aren't explicitly specified. (In most cases, this means a default of @racket[+inf.0].)
 Of course, applications using @racket[make-unlimited-safety-limits]
 may remain vulnerable to threats which the values from @racket[make-safety-limits]
 would have protected against.

 The @tech{safety limits} type was introduced in version 1.6 of the
 @tt{web-server-lib} package.
 Previous versions of this library only supported the @racket[max-waiting] limit
 and (in some cases) an @racket[initial-connection-timeout] limit,
 which was similar to @racket[request-read-timeout], but had
 @hyperlink["https://github.com/racket/web-server/pull/77"]{some problems}.
 These limits were specified through @racket[dispatch-server-config^], @racket[web-config^],
 and optional arguments to functions like @racket[serve]:
 if values weren't explicitly supplied, the default behavior was closest to using
 @racket[(make-unlimited-safety-limits #:request-read-timeout 60)].

 However, version 1.6 adopted @racket[(make-safety-limits)] as the default,
 as most applications would benefit from using reasonable protections.
 When porting from earlier versions of this library,
 if you think your application may be especially resource-intensive,
 you may prefer to use @racket[make-unlimited-safety-limits] while determining
 limits that work for your application.

 @history[#:added "1.6"]
 @history[#:changed "1.11" @elem{added the @racket[max-concurrent] limit}]
}

@defproc[(make-unlimited-safety-limits
          [#:max-concurrent max-concurrent positive-count/c +inf.0]
          [#:max-waiting max-waiting exact-nonnegative-integer? 511]
          [#:request-read-timeout request-read-timeout timeout/c +inf.0]
          [#:max-request-line-length max-request-line-length nonnegative-length/c +inf.0]
          [#:max-request-headers max-request-headers nonnegative-length/c +inf.0]
          [#:max-request-header-length max-request-header-length nonnegative-length/c +inf.0]
          [#:max-request-body-length max-request-body-length nonnegative-length/c +inf.0]
          [#:max-request-files max-request-files nonnegative-length/c +inf.0]
          [#:max-request-file-length max-request-file-length nonnegative-length/c +inf.0]
          [#:request-file-memory-threshold request-file-memory-threshold nonnegative-length/c +inf.0]
          [#:max-form-data-parts max-form-data-parts nonnegative-length/c +inf.0]
          [#:max-form-data-header-length max-form-data-header-length nonnegative-length/c +inf.0]
          [#:max-form-data-files max-form-data-files nonnegative-length/c +inf.0]
          [#:max-form-data-file-length max-form-data-file-length nonnegative-length/c +inf.0]
          [#:form-data-file-memory-threshold form-data-file-memory-threshold nonnegative-length/c +inf.0]
          [#:max-form-data-fields max-form-data-fields nonnegative-length/c +inf.0]
          [#:max-form-data-field-length max-form-data-field-length nonnegative-length/c +inf.0]
          [#:response-timeout response-timeout timeout/c +inf.0]
          [#:response-send-timeout response-send-timeout timeout/c +inf.0]
          [#:shutdown-grace-period shutdown-grace-period (or/c #f timeout/c) #f])
         safety-limits?]{
 Like @racket[make-safety-limits], but with default values that avoid
 imposing any limits that aren't explicitly specified,
 rather than the safer defaults of @racket[make-safety-limits].
 Think carefully before using @racket[make-unlimited-safety-limits],
 as it may leave your application vulnerable to denial of service attacks
 or other threats that the default values from @racket[make-safety-limits] would mitigate.
 See the @elemref["safety-limits-porting"]{compatability note} for more details.

 Note that the default value for @racket[max-waiting] is @racket[511],
 @italic{not} @racket[+inf.0], due to the contract of @racket[tcp-listen].

 @history[#:added "1.6"]
 @history[#:changed "1.11" @elem{added the @racket[max-concurrent] limit}]
}
