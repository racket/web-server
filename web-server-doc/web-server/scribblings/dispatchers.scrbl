#lang scribble/doc
@(require "web-server.rkt"
          (for-label web-server/http
                     net/url
                     web-server/servlet/setup
                     web-server/configuration/responders
                     web-server/private/servlet
                     racket/date
                     web-server/private/util
                     web-server/private/connection-manager)
          (for-syntax racket/base))

@title[#:tag "dispatchers"
       #:style 'toc]{Dispatchers}

Since the @web-server is really just a particular configuration of a
dispatching server, there are several dispatchers that are defined
to support the @|web-server|. Other dispatching servers may find these useful. In particular, if you want
a peculiar processing pipeline for your @web-server installation, refer to this
documentation.

@local-table-of-contents[]

@; ------------------------------------------------------------
@section[#:tag "dispatch"]{General}
@(require (for-label web-server/dispatchers/dispatch))

@defmodule[web-server/dispatchers/dispatch]{

This module provides a few functions for dispatchers in general.

@defthing[dispatcher/c contract?]{
 Equivalent to @racket[(connection? request? . -> . any)].

  @history[#:changed "1.3"
           @elem{Weakened the range contract to allow @racket[any]}]
}

@defproc[(dispatcher-interface-version/c (any any/c)) boolean?]{
 Equivalent to @racket[(symbols 'v1)]
}

@defstruct[exn:dispatcher ()]{
 An exception thrown to indicate that a dispatcher does not apply to a particular
 request.
}

@defproc[(next-dispatcher) any]{
 Raises a @racket[exn:dispatcher]
}

As the @racket[dispatcher/c] contract suggests, a dispatcher is a function that takes a connection
and request object and does something to them. Mostly likely it will generate
some response and output it on the connection, but it may do something
different. For example, it may apply some test to the request object, perhaps
checking for a valid source IP address, and error if the test is not passed, and call @racket[next-dispatcher]
otherwise.

Consider the following example dispatcher, that captures the essence of URL rewriting:
@racketblock[
 (code:comment "(url? -> url?) dispatcher/c -> dispatcher/c")
 (lambda (rule inner)
   (lambda (conn req)
     (code:comment "Call the inner dispatcher...")
     (inner conn
            (code:comment "with a new request object...")
            (struct-copy request req
                         (code:comment "with a new URL!")
                         [request-uri (rule (request-uri req))]))))
]

}

@; ------------------------------------------------------------
@section[#:tag "filesystem-map"]{Mapping URLs to Paths}
@(require (for-label web-server/dispatchers/filesystem-map))

@defmodule[web-server/dispatchers/filesystem-map]{

This module provides a means of mapping
URLs to paths on the filesystem.

@defthing[url->path/c contract?]{
 This contract is equivalent to @racket[((url?) . ->* . (path? (listof path-piece?)))].
 The returned @racket[path?] is the path on disk. The list is the list of
 path elements that correspond to the path of the URL.}

@defproc[(make-url->path (base path-string?))
         url->path/c]{
 The @racket[url->path/c] returned by this procedure considers the root
 URL to be @racket[base]. It ensures that @racket[".."]s in the URL
 do not escape the @racket[base] and removes them silently otherwise.}

@defproc[(make-url->valid-path (url->path url->path/c))
         url->path/c]{
 Runs the underlying @racket[url->path], but only returns if the path
 refers to a file that actually exists. If it is does not, then the suffix
 elements of the URL are removed until a file is found. If this never occurs,
 then an error is thrown.

 This is primarily useful for dispatchers that allow path information after
 the name of a service to be used for data, but where the service is represented
 by a file. The most prominent example is obviously servlets.}

@defproc[(filter-url->path [regex regexp?]
                                 [url->path url->path/c])
         url->path/c]{
 Runs the underlying @racket[url->path] but will only return if the
 path, when considered as a string, matches the @racket[regex]. This is
 useful to disallow strange files, like GIFs, from being considered
 servlets when using the servlet dispatchers. It will return a
 @racket[exn:fail:filesystem:exists?] exception if the path does not
 match.
}

}

@; ------------------------------------------------------------
@section[#:tag "dispatch-sequencer"]{Sequencing}
@a-dispatcher[web-server/dispatchers/dispatch-sequencer
              @elem{defines a dispatcher constructor
                 that invokes a sequence of dispatchers until one applies.}]{

@defproc[(make (dispatcher dispatcher/c) ...)
         dispatcher/c]{
 Invokes each @racket[dispatcher], invoking the next if the first
 calls @racket[next-dispatcher]. If no @racket[dispatcher] applies,
 then it calls @racket[next-dispatcher] itself.
}}

@; ------------------------------------------------------------
@section[#:tag "dispatch-timeout"]{Timeouts}
@a-dispatcher[web-server/dispatchers/dispatch-timeout
               @elem{defines a dispatcher constructor
                  that changes the timeout on the connection and calls the next
                  dispatcher.}]{

@defproc[(make [new-timeout integer?])
         dispatcher/c]{
 Changes the timeout on the connection with @racket[adjust-connection-timeout!]
 called with @racket[new-timeout].
}}

@; ------------------------------------------------------------
@section[#:tag "dispatch-lift"]{Lifting Procedures}
@a-dispatcher[web-server/dispatchers/dispatch-lift
              @elem{defines a dispatcher constructor.}]{

@defproc[(make (proc (request? . -> . response?)))
         dispatcher/c]{
 Constructs a dispatcher that calls @racket[proc] on the request
 object, and outputs the response to the connection.
}}

@; ------------------------------------------------------------
@section[#:tag "dispatch-filter"]{Filtering Requests by URL}
@a-dispatcher[web-server/dispatchers/dispatch-filter
              @elem{defines a dispatcher constructor
                 that calls an underlying dispatcher
                 with all requests that pass a predicate.}]{

@defproc[(make (regex regexp?) (inner dispatcher/c))
         dispatcher/c]{
 Calls @racket[inner] if the URL path of the request, converted to
 a string, matches @racket[regex]. Otherwise, calls @racket[next-dispatcher].
}}

@; ------------------------------------------------------------
@section[#:tag "dispatch-method"]{Filtering Requests by Method}
@a-dispatcher[web-server/dispatchers/dispatch-method
              @elem{defines a dispatcher constructor
                 that calls an underlying dispatcher
                 provided the request method belongs to a given list.}]{

@defproc[(make (method (or/c symbol? (listof symbol?))) (inner dispatcher/c))
         dispatcher/c]{
 Calls @racket[inner] if the method of the request, converted to
 a string, case-insensitively matches @racket[method] (if method is a symbol, or, if a list, one of the elements of @racket[method]). Otherwise, calls @racket[next-dispatcher].
}}

@; ------------------------------------------------------------
@section[#:tag "dispatch-pathprocedure"]{Procedure Invocation upon Request}
@a-dispatcher[web-server/dispatchers/dispatch-pathprocedure
              @elem{defines a dispatcher constructor
                   for invoking a particular procedure when a request is given to a particular
                   URL path.}]{

@defproc[(make (path string?) (proc (request? . -> . response?)))
         dispatcher/c]{
 Checks if the request URL path as a string is equal to @racket[path]
 and if so, calls @racket[proc] for a response.
}

This is used in the standard @web-server pipeline to provide
a URL that refreshes the password file, servlet cache, etc.}

@; ------------------------------------------------------------
@section[#:tag "dispatch-logresp"]{Logging}
@a-dispatcher[web-server/dispatchers/dispatch-logresp
              @elem{defines a dispatcher constructor
                    for transparent logging of requests and responses.}]{

@defthing[format-reqresp/c contract?]{
 Equivalent to @racket[(or/c (request? . -> . string?) (request? response? . -> . string?))].
}

@defthing[paren-format format-reqresp/c]{
 Formats a request and a response by:
 @racketblock[
  (format
   "~s\n"
   (list 'from (request-client-ip req)
         'to (request-host-ip req)
         'for (url->string (request-uri req))
         'at (date->string
              (seconds->date (current-seconds)) #t)
         'code (response-code resp)))
  ]}

@defthing[extended-format format-reqresp/c]{
 Formats a request and a response by:
 @racketblock[
  (format
   "~s\n"
   `((client-ip ,(request-client-ip req))
     (host-ip ,(request-host-ip req))
     (referer
      ,(let ([R (headers-assq*
                 #"Referer"
                 (request-headers/raw req))])
         (if R
             (header-value R)
             #f)))
     (uri ,(url->string (request-uri req)))
     (time ,(current-seconds))
     (code ,(response-code resp))))
 ]}

@defthing[apache-default-format format-reqresp/c]{
 Formats a request and a response like Apache's default. However, Apache's default
 includes information about the size of the object returned to the client,
 which this function does not have access to, so it defaults the last field
 to @litchar{-}.
}

@defthing[log-format/c contract?]{
 Equivalent to @racket[(symbols 'parenthesized-default 'extended 'apache-default)].
}

@defproc[(log-format->format [id log-format/c])
         format-reqresp/c]{
 Maps @racket['parenthesized-default] to @racket[paren-format],
 @racket['extended] to @racket[extended-format], and
 @racket['apache-default] to @racket[apache-default-format].
}

@defproc[(make [#:format format (or/c log-format/c format-reqresp/c) paren-format]
               [#:log-path log-path (or/c path-string? output-port?) "log"]
               [dispatcher dispatcher/c])
         dispatcher/c]{
 If @racket[dispatcher] is successfully dispatched,
 logs requests and responses (without the @racket[body] information)
 to @racket[log-path], which can be either a filepath or an @racket[output-port?],
 using @racket[format] to format the requests and responses
 (or just requests if @racket[format] only accepts one argument).
 If @racket[format] is a symbol, a log formatter will be tacitly made using @racket[log-format->format].

 @history[#:added "1.12"]
}}

@; ------------------------------------------------------------
@section[#:tag "dispatch-log"]{Basic Logging}
@a-dispatcher[web-server/dispatchers/dispatch-log
              @elem{defines a dispatcher constructor
                    for transparent logging of requests.
                    Consider using the facilities in
                    @secref{dispatch-logresp} instead,
                    as it provides more flexibility.}]{

@defthing[format-req/c contract?]{
 Equivalent to @racket[(request? . -> . string?)].
}

@defthing[paren-format format-req/c]{
 Formats a request like its counterpart in @secref{dispatch-logresp}, but without the response code information.}

@defthing[extended-format format-req/c]{
 Formats a request like its counterpart in @secref{dispatch-logresp}, but without the response code information.}

@defthing[apache-default-format format-req/c]{
 Formats a request like Apache's default. However, Apache's default
 includes information about the response to a request, which this
 function does not have access to, so it defaults the last two fields
 to @litchar{-} and @litchar{-}.

}

@defthing[log-format/c contract?]{
 Equivalent to @racket[(symbols 'parenthesized-default 'extended 'apache-default)].
}

@defproc[(log-format->format [id log-format/c])
         format-req/c]{
 Maps @racket['parenthesized-default] to @racket[paren-format],
 @racket['extended] to @racket[extended-format], and
 @racket['apache-default] to @racket[apache-default-format].
}

@defproc[(make [#:format format (or/c log-format/c format-req/c) paren-format]
               [#:log-path log-path (or/c path-string? output-port?) "log"])
         dispatcher/c]{
 Logs requests to @racket[log-path], which can be either a filepath or an @racket[output-port?],
 using @racket[format] to format the requests.
 If @racket[format] is a symbol,
 a log formatter will be tacitly made using @racket[log-format->format].
 Then invokes @racket[next-dispatcher].

 @history[
   #:changed "1.3"
   @elem{Allow @racket[log-path] to be an @racket[output-port?]}
   #:changed "1.8"
   @elem{Allow @racket[format] to be a symbol (more precisely, a @racket[log-format/c]).}
]
}}

@; ------------------------------------------------------------
@section[#:tag "dispatch-passwords"]{Password Protection}
@a-dispatcher[web-server/dispatchers/dispatch-passwords
              @elem{defines a dispatcher constructor
                    that performs HTTP Basic authentication filtering.}]{

@(require (for-label web-server/http
                     net/url
                     web-server/configuration/responders))

@defthing[denied?/c contract?]{
  Equivalent to @racket[(request? . -> . (or/c false/c string?))].  The
  return is the authentication realm as a string if the request is not
  authorized and @racket[#f] if the request @emph{is} authorized.
}

@defproc[(make [denied? denied?/c]
               [#:authentication-responder
                authentication-responder
                (url? header? . -> . response?)
                (gen-authentication-responder "forbidden.html")])
         dispatcher/c]{
  A dispatcher that checks if the request is denied based on
  @racket[denied?]. If so, then @racket[authentication-responder] is
  called with a @racket[header] that requests credentials. If not, then
  @racket[next-dispatcher] is invoked.
}

@defthing[authorized?/c contract?]{
 Equivalent to
 @racket[(string? (or/c false/c bytes?) (or/c false/c bytes?) . -> . (or/c false/c string?))].
 The input is the URI as a string and the username and passwords as
 bytes.  The return is the authentication realm as a string if the user
 is not authorized and @racket[#f] if the request @emph{is} authorized.
}

@defproc[(make-basic-denied?/path [authorized? authorized?/c])
                                  denied?/c]{
Creates a denied procedure from an authorized procedure.
}

@defproc[(password-file->authorized? [password-file path-string?])
         (values (-> void)
                 authorized?/c)]{
 Creates an authorization procedure based on the given password
 file. The first returned value is a procedure that refreshes the
 password cache used by the authorization procedure.

 @racket[password-file] is parsed as:
 @racketblock[(list ([domain : string?]
                     [path : string?] (code:comment "This string is interpreted as a regex")
                     (list [user : symbol?]
                           [pass : string?])
                     ...)
                    ...)]
 For example:
 @racketblock['(("secret stuff" "/secret(/.*)?" (bubba "bbq") (|Billy| "BoB")))]
}}

@; ------------------------------------------------------------
@section[#:tag "dispatch-host"]{Virtual Hosts}
@a-dispatcher[web-server/dispatchers/dispatch-host
              @elem{defines a dispatcher constructor
                    that calls a different dispatcher based upon the host requested.}]{

@defproc[(make (lookup-dispatcher (symbol? . -> . dispatcher/c)))
         dispatcher/c]{
 Extracts a host from the URL requested, or the Host HTTP header,
 calls @racket[lookup-dispatcher] with the host, and invokes the
 returned dispatcher. If no host can be extracted, then @racket['none]
 is used.
}}

@; ------------------------------------------------------------
@section[#:tag "dispatch-files"]{Serving Files}
@a-dispatcher[web-server/dispatchers/dispatch-files
             @elem{allows files to be served.
                It defines a dispatcher construction procedure.}]{

@defproc[(make [#:url->path url->path url->path/c]
               [#:path->mime-type path->mime-type (path? . -> . (or/c false/c bytes?)) (lambda (path) #f)]
               [#:path->headers path->headers (path? . -> . (listof header?)) (lambda (path) '())]
               [#:indices indices (listof string?) (list "index.html" "index.htm")]
               [#:cache-max-age cache-max-age (or/c false/c (and/c exact-integer? positive?)) #f]
               [#:cache-smaxage cache-smaxage (or/c false/c (and/c exact-integer? positive?)) #f]
               [#:cache-stale-while-revalidate cache-stale-while-revalidate (or/c false/c (and/c exact-integer? positive?)) #f]
               [#:cache-stale-if-error cache-stale-if-error (or/c false/c (and/c exact-integer? positive?)) #f]
               [#:cache-no-cache cache-no-cache boolean? #f]
               [#:cache-no-store cache-no-store boolean? #f]
               [#:cache-no-transform cache-no-transform boolean? #f]
               [#:cache-must-revalidate cache-must-revalidate boolean? #f]
               [#:cache-proxy-revalidate cache-proxy-revalidate boolean? #f]
               [#:cache-must-understand cache-must-understand boolean? #f]
               [#:cache-private cache-private boolean? #f]
               [#:cache-public cache-public boolean? #f]
               [#:cache-immutable cache-immutable boolean? #f])
         dispatcher/c]{
 Uses @racket[url->path] to extract a path from the URL in the request
 object. If this path does not exist, then the dispatcher does not apply and
 @racket[next-dispatcher] is invoked.
 If the path is a directory, then the @racket[indices] are checked in order
 for an index file to serve. In that case, or in the case of a path that is
 a file already, @racket[path->mime-type] is consulted for the MIME
 Type of the path.
 Similarly, @racket[path->headers] is consulted for additional
 headers of the path.
 The file is then streamed out the connection object.

 This dispatcher supports HTTP Range GET requests and HEAD
 requests. If the request's method is neither HEAD nor GET,
 @racket[next-dispatcher] will be called.

 If the path works out to something on disk (either as a
 file, or, if the path refers to directory, one of the
 specified @racket[indices] files in that directory), it
 needs to be readable by the process that is running the web
 server. Existing but unreadable files are handled as
 non-existing files.

 The various keyword arguments that start with @tt{cache-}
 (@racket[cache-public], @racket[cache-max-age] and so on)
 all map straightforwardly to legal values that can appear
 in the standard @tt{Cache-Control} response header. By
 default, all are @racket[#f], which has the effect that
 responses emitted by this dispatcher do not have a
 @tt{Cache-Control} header. If any cache-related keyword has
 a non-@racket[#f] value, a @tt{Cache-Control} header will
 be present in the response. Thus, if
 @racket[cache-immutable] is @racket[#t] and
 @racket[cache-max-age] is @racket[12345], an
 @tt{Cache-Control} header will be present in all responses
 and its value will be @tt{max-age=12345, immutable}. For
 more information see
 @hyperlink["https://www.ietf.org/rfc/rfc2616.html#section-14.9"]{RFC
 2616 section 14.9 “Cache Control”} and the
 @hyperlink["https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cache-Control"]{Mozilla
 Developer Network documentation on
 @tt{Cache-Control}}. Note that some combinations of cache
 headers may lead to unintended behavior. Example: using
 @racket[#t] for both @racket[#:cache-public] and
 @racket[#:cache-private] (those two are effectively
 antonyms). Beyond the contract for each of the keyword
 arguments, no additional checks are made by @racket[make]
 to ensure that the supplied cache-related arguments are a
 meaningful combination or are suitable for your web
 application.}

@history[
  #:changed "1.7"
  @elem{Support for non-{GET,HEAD} requests.}
  #:changed "1.7"
  @elem{Treat unreadable files as non-existing files.}
  #:changed "1.9"
  @elem{Support a number of options for setting a @tt{Cache-Control} response header}
  #:changed "1.10"
  @elem{Support @racket[#:path->headers].}
]

}

@; ------------------------------------------------------------
@include-section["dispatch-servlets.scrbl"]

@; ------------------------------------------------------------
@section[#:tag "dispatch-stat"]{Statistics}
@a-dispatcher[web-server/dispatchers/dispatch-stat
              @elem{provides services related to performance
                    statistics.}]{

@defproc[(make-gc-thread [time integer?])
         thread?]{
 Starts a thread that calls @racket[(collect-garbage)] every @racket[time] seconds.
}

@defproc[(make)
         dispatcher/c]{
 Returns a dispatcher that prints memory usage on every request.
}}

@; ------------------------------------------------------------
@section[#:tag "limit"]{Limiting Requests}
@a-dispatcher[web-server/dispatchers/limit
              @elem{provides a wrapper dispatcher that limits how many requests are serviced at once.}]{

@defproc[(make [limit number?]
               [inner dispatcher/c]
               [#:over-limit over-limit (symbols 'block 'kill-new 'kill-old) 'block])
         dispatcher/c]{
 Returns a dispatcher that defers to @racket[inner] for work, but will forward a maximum of @racket[limit] requests concurrently.

 If there are no additional spaces inside the limit and a new request is received, the @racket[over-limit] option determines what is done.
 The default (@racket['block]) causes the new request to block until an old request is finished being handled.
 If @racket[over-limit] is @racket['kill-new], then the new request handler is killed---a form of load-shedding.
 If @racket[over-limit] is @racket['kill-old], then the oldest request handler is killed---prioritizing new connections over old.
 (This setting is a little dangerous because requests might never finish if there is constant load.)
}}

@(require (for-label
           web-server/web-server
           web-server/http
           (prefix-in limit: web-server/dispatchers/limit)
           (prefix-in filter: web-server/dispatchers/dispatch-filter)
           (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)))

Consider this example:
@racketmod[
 racket

(require web-server/web-server
         web-server/http
         web-server/http/response
         (prefix-in limit: web-server/dispatchers/limit)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer))

(serve #:dispatch
       (sequencer:make
        (filter:make
         #rx"/limited"
         (limit:make
          5
          (lambda (conn req)
            (output-response/method
             conn
             (response/full
              200 #"Okay"
              (current-seconds) TEXT/HTML-MIME-TYPE
              empty
              (list (string->bytes/utf-8
                     (format "hello world ~a"
                            (sort (build-list 100000 (λ x (random 1000)))
                                  <)))))
             (request-method req)))
          #:over-limit 'block))
        (lambda (conn req)
          (output-response/method
           conn
           (response/full 200 #"Okay"
                          (current-seconds) TEXT/HTML-MIME-TYPE
                          empty
                          (list #"<html><body>Unlimited</body></html>"))
           (request-method req))))
       #:port 8080)

(do-not-return)
]

@; ------------------------------------------------------------
@section[#:tag "wrap"]{Wrapping Requests & Responses}
@a-dispatcher[web-server/dispatchers/dispatch-wrap
              @elem{provides a general-purpose wrapping dispatcher that allows one to intercept an incoming request as well as the response returned by other servlets}]{

@defproc[(make [servlet (-> request? response?)]
               [req-trans (-> request? request?)]
               [res-trans (-> response? response?)])
         dispatcher/c]{

Returns a dispatcher that wraps @racket[res-trans] around
@racket[servlet], which itself receives requests transformed
by @racket[req-trans]. Put differently, the servlet
underlying the dispatcher returned by @racket[make] is
equivalent to @racket[(λ (r) (res-trans (servlet (req-trans
r))))].

If you're not interested in transforming requests, pass in
@racket[identity] (the identity function) for
@racket[req-trans]. Similarly, using @racket[identity] for
@racket[res-trans] will cause responses to pass through
unchanged. (Using @racket[identity] for @emph{both}
@racket[req-trans] and @racket[res-trans] is equivalent to
just using @racket[servlet] as-is.)

A typical use case for this dispatcher would be to inject
headers into requests or responses. Similarly, functionally
updating existing headers also fits into this pattern. Since
the entire request -- not just its headers -- is available
to @racket[req-trans] (and similarly for the response and
@racket[res-trans]), arbitrary rewriting of request/response
bodies is possible. Side effects in @racket[req-trans] and
@racket[res-trans] are permitted as long as @racket[make]'s
contracts are adhered to.

}

@history[#:changed "1.9"
         @elem{First version of this dispatcher}]

}
