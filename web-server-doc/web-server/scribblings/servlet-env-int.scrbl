#lang scribble/doc
@(require "web-server.rkt")

@title[]{Simple Single Servlet Servers}
@(require (for-label web-server/servlet-env
                     web-server/servlet-dispatch
                     web-server/http
                     web-server/managers/manager
                     web-server/managers/lru
                     web-server/private/util
                     web-server/dispatchers/dispatch
                     web-server/configuration/configuration-table
                     web-server/configuration/responders
                     web-server/dispatchers/dispatch-log
                     web-server/web-server
                     web-server/safety-limits
                     net/url
                     racket/serialize
                     web-server/stuffers
                     racket/list))

@defmodule[web-server/servlet-dispatch]{

These functions optimize the construction of dispatchers and launching of servers for single servlets and interactive development.

@defproc[(dispatch/servlet 
          [start (request? . -> . response?)]
          [#:regexp regexp regexp? #rx""]
          [#:stateless? stateless? boolean? #f]
          [#:stuffer stuffer (stuffer/c serializable? bytes?) default-stuffer]
          [#:manager manager manager? (make-threshold-LRU-manager #f (* 1024 1024 64))]
          [#:current-directory servlet-current-directory path-string? (current-directory)]
          [#:responders-servlet-loading
           responders-servlet-loading
           (url? any/c . -> . can-be-response?)
           servlet-loading-responder]
          [#:responders-servlet
           responders-servlet
           (url? any/c . -> . can-be-response?)
           servlet-error-responder])
         dispatcher/c]{
 @racket[serve/servlet] starts a server and uses a particular dispatching sequence. For some applications, this
 nails down too much, but users are conflicted, because the interface is so convenient. For those users, @racket[dispatch/servlet]
 does the hardest part of @racket[serve/servlet] and constructs a dispatcher just for the @racket[start] servlet.
 
 The dispatcher responds to requests that match @racket[regexp]. The current directory
 of servlet execution is @racket[servlet-current-directory]. 
 
 If @racket[stateless?] is true, then the servlet is run as a stateless @racketmod[web-server] module and @racket[stuffer] is used
 as the @tech[#:doc '(lib "web-server/scribblings/web-server.scrbl")]{stuffer}.

 The servlet is loaded with @racket[manager] as its continuation manager. (The default manager limits the amount of memory to 64 MB and
 deals with memory pressure as discussed in the @racket[make-threshold-LRU-manager] documentation.)
 
 The servlet is run in the @racket[(current-namespace)].

 If a servlet fails to load, @racket[responders-servlet-loading] is used. If a servlet errors during its operation, @racket[responders-servlet] is used.
}

@defproc[(serve/launch/wait
          [make-dispatcher (semaphore? . -> . dispatcher/c)]
          [#:connection-close? connection-close? boolean? #f]
          [#:launch-path launch-path (or/c #f string?) #f]
          [#:banner? banner? boolean? #f]
          [#:listen-ip listen-ip (or/c #f string?) "127.0.0.1"]
          [#:port port number? 8000]
          [#:ssl-cert ssl-cert (or/c #f path-string?) #f]
          [#:ssl-key ssl-key (or/c #f path-string?) #f]
          [#:max-waiting max-waiting exact-nonnegative-integer? 511]
          [#:safety-limits safety-limits safety-limits?
           (make-safety-limits #:max-waiting max-waiting)])
         any]{
 The other interesting part of @racket[serve/servlet] is its ability to start up a server and immediately
 launch a browser at it. This is provided by @racket[serve/launch/wait].
 
 It starts a server using the result of @racket[make-dispatcher] as the dispatcher.
 The @racket[make-dispatcher] argument is called with
 a semaphore that, if posted, will cause the server to quit.
 
 If @racket[launch-path] is not false, then a browser is launched with that path appended to the URL to the server itself.
 
 If @racket[banner?] is true, then a banner is printed informing the user of the server's URL.
 
 The server listens on @racket[listen-ip] and port @racket[port]. If @racket[listen-ip] is @racket[#f], then the server accepts 
 connections to all of the listening machine's addresses. Otherwise, the server accepts connections only at the interface(s) associated with the given string.
 For example, providing @racket["127.0.0.1"] (the default) as @racket[listen-ip] creates a server that accepts only connections to @racket["127.0.0.1"] (the loopback interface) from the local machine.

 If @racket[ssl-key] and @racket[ssl-cert] are not false, then the server runs in HTTPS mode with @racket[ssl-cert]
 and @racket[ssl-key] as paths to the certificate and private key.    
 
 If @racket[connection-close?] is @racket[#t], then every connection is closed after one
 request. Otherwise, the client decides based on what HTTP version it uses.

  The @racket[safety-limits] argument supplies a @tech{safety limits}
  value specifying the policies to be used while reading and handling requests.

  The @racket[max-waiting] argument is supported for backwards compatability.
  If a @racket[safety-limits] argument is given, the @racket[max-waiting]
  argument is ignored; otherwise, it is passed to @racket[make-safety-limits]
  to construct the @tech{safety limits} value.
  If neither @racket[max-waiting] nor @racket[safety-limits] are given,
  the default @tech{safety limits} value is equivalent to @racket[(make-safety-limits)].
 
  @history[#:changed "1.6"
           @elem{Added the @racket[safety-limits] argument:
              see @elemref["safety-limits-porting"]{compatability note}.}]
}
              
}
