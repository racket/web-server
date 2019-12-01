#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "web-server"]{Launching Servers}

@(require (for-label web-server/web-server
                     web-server/dispatchers/filesystem-map
                     web-server/web-config-unit
                     web-server/web-config-sig
                     web-server/private/dispatch-server-unit
                     web-server/private/dispatch-server-sig
                     web-server/dispatchers/dispatch
                     web-server/safety-limits
                     net/tcp-sig
                     racket/tcp
                     racket/async-channel
                     web-server/configuration/configuration-table)
          (prefix-in raw: (for-label net/tcp-unit))
          (prefix-in files: (for-label web-server/dispatchers/dispatch-files)))

@defmodule[web-server/web-server]{

This module provides functions for launching dispatching servers.

@defproc[(serve [#:dispatch dispatch dispatcher/c]
                [#:confirmation-channel confirmation-channel
                 (or/c #f (async-channel/c
                           (or/c exn? port-number?)))
                 #f]
                [#:connection-close? connection-close? boolean? #f]
                [#:dispatch-server-connect@ dispatch-server-connect@
                                            (unit/c (import)
                                                    (export dispatch-server-connect^))
                                            raw:dispatch-server-connect@]
                [#:tcp@ tcp@ (unit/c (import) (export tcp^)) raw:tcp@]
                [#:port port listen-port-number? 80]
                [#:listen-ip listen-ip (or/c string? #f) #f]
                [#:max-waiting max-waiting exact-nonnegative-integer? 511]
                [#:initial-connection-timeout request-read-timeout timeout/c 60]
                [#:safety-limits safety-limits safety-limits?
                 (make-safety-limits
                  #:max-waiting max-waiting
                  #:request-read-timeout request-read-timeout)])
         (-> any)]{

  Constructs an appropriate @racket[dispatch-server-config*^], invokes the
  @racket[dispatch-server@], and calls its @racket[serve] function.

  If @racket[connection-close?] is @racket[#t], then every connection is
  closed after one request. Otherwise, the client decides based on what
  HTTP version it uses.

  The @racket[dispatch-server-connect@] argument supports the
  conversion of raw connections; for example, @racket[make-ssl-connect@]
  produces a unit to serve SSL by converting raw TCP ports to SSL ports;
  see also @secref["faq:https"].

  The @racket[tcp@] argument supports replacing TCP connections with
  other kinds of connections (and was formerly recommended for SSL
  support). Beware that the server expects the @racket[tcp-accept]
  operation from @racket[tcp@] to be effectively atomic; new connections
  are not accepted while @racket[tcp-accept] is in progress.

  The @racket[safety-limits] argument supplies a @tech{safety limits}
  value specifying the policies to be used while reading and handling requests.
  In the constructed @racket[dispatch-server-config*^], it is used directly
  as the @sigelem[dispatch-server-config*^ safety-limits] value and is also used
  by the @sigelem[dispatch-server-config*^ read-request] implementation.

  The @racket[max-waiting] and @racket[request-read-timeout] arguments
  are supported for backwards compatability.
  If a @racket[safety-limits] argument is given, the @racket[max-waiting] and
  @racket[request-read-timeout] arguments are ignored;
  otherwise, they are passed to @racket[make-safety-limits] to construct
  the @tech{safety limits} value.
  If neither @racket[max-waiting], @racket[request-read-timeout],
  nor @racket[safety-limits] are given, the default @tech{safety limits} value
  is equivalent to @racket[(make-safety-limits)].

Here's an example of a simple web server that serves files
from a given path:

@racketblock[
(define (start-file-server base)
  (serve
   #:dispatch
   (files:make
    #:url->path (make-url->path base)
    #:path->mime-type
    (lambda (path)
      #"application/octet-stream"))
   #:port 8080))
]

  @history[#:changed "1.6"
           @elem{Added the @racket[safety-limits] argument and changed
              to use @racket[dispatch-server-config*^] instead of @racket[dispatch-server-config^]:
              see @elemref["safety-limits-porting"]{compatability note}.
              Corrected documented contracts for the @racket[max-waiting] and
              @racket[request-read-timeout] arguments.}]}


@defproc[(serve/ports [#:dispatch dispatch dispatcher/c]
                      [#:confirmation-channel confirmation-channel
                       (or/c #f (async-channel/c
                                 (or/c exn? port-number?)))
                       #f]
                      [#:connection-close? connection-close? boolean? #f]
                      [#:dispatch-server-connect@ dispatch-server-connect@
                                                  (unit/c (import)
                                                          (export dispatch-server-connect^))
                                                  raw:dispatch-server-connect@]
                      [#:tcp@ tcp@ (unit/c (import) (export tcp^)) raw:tcp@]
                      [#:ports ports (listof listen-port-number?) (list 80)]
                      [#:listen-ip listen-ip (or/c string? #f) #f]
                      [#:max-waiting max-waiting exact-nonnegative-integer? 511]
                      [#:initial-connection-timeout request-read-timeout timeout/c 60]
                      [#:safety-limits safety-limits safety-limits?
                       (make-safety-limits
                        #:max-waiting max-waiting
                        #:request-read-timeout request-read-timeout)])
         (-> any)]{
 Calls @racket[serve] multiple times, once for each @racket[port], and returns
 a function that shuts down all of the server instances.

@history[#:changed "1.6"
         @elem{Added the @racket[safety-limits] argument as with @racket[serve]:
            see @elemref["safety-limits-porting"]{compatability note}.}]}


@defproc[(serve/ips+ports [#:dispatch dispatch dispatcher/c]
                          [#:confirmation-channel confirmation-channel
                           (or/c #f (async-channel/c
                                     (or/c exn? port-number?)))
                           #f]
                          [#:connection-close? connection-close? boolean? #f]
                          [#:dispatch-server-connect@ dispatch-server-connect@
                                                      (unit/c (import)
                                                              (export dispatch-server-connect^))
                                                      raw:dispatch-server-connect@]
                          [#:tcp@ tcp@ (unit/c (import) (export tcp^)) raw:tcp@]
                          [#:ips+ports ips+ports (listof (cons/c (or/c string? #f) (listof listen-port-number?))) (list (cons #f (list 80)))]
                          [#:max-waiting max-waiting exact-nonnegative-integer? 511]
                          [#:initial-connection-timeout request-read-timeout timeout/c 60]
                          [#:safety-limits safety-limits safety-limits?
                           (make-safety-limits
                            #:max-waiting max-waiting
                            #:request-read-timeout request-read-timeout)])
         (-> any)]{
 Calls @racket[serve/ports] multiple times, once for each @racket[ip], and returns
 a function that shuts down all of the server instances.

@history[#:changed "1.6"
         @elem{Added the @racket[safety-limits] argument as with @racket[serve]:
            see @elemref["safety-limits-porting"]{compatability note}.}
         #:changed "1.1"
         @elem{Added the @racket[#:dispatch-server-connect@] argument.}]}

@defproc[(serve/web-config@ [config@ (unit/c (import) (export web-config*^))]
                            [#:dispatch-server-connect@ dispatch-server-connect@
                                                        (unit/c (import)
                                                                (export dispatch-server-connect^))
                                                        raw:dispatch-server-connect@]
                            [#:tcp@ tcp@ (unit/c (import) (export tcp^))
                                    raw:tcp@])
         (-> void)]{
 Starts the @web-server with the settings defined by the given @racket[web-config*^] unit.

 Combine @racket[serve/web-config@] with @racket[configuration-table->web-config@] and @racket[configuration-table-sexpr->web-config@]:

 @racketblock[
  (serve/web-config@
   (configuration-table->web-config@
    default-configuration-table-path))]

@history[#:changed "1.6"
         @elem{Use @racket[web-config*^] rather than @racket[web-config^]:
            see @elemref["safety-limits-porting"]{compatability note}.}
         #:changed "1.1"
         @elem{Added the @racket[#:dispatch-server-connect@] argument.}]}


@defthing[raw:dispatch-server-connect@ (unit/c (import) (export dispatch-server-connect^))]{

A default implementation of the dispatch server's connection-conversion abstraction that performs no conversion.

@history[#:added "1.1"]}


@defproc[(make-ssl-connect@ [server-cert-file path-string?]
                            [server-key-file path-string?])
         (unit/c (import) (export dispatch-server-connect^))]{

Constructs an implementation of the dispatch server's connection-conversion abstraction for OpenSSL.

@history[#:added "1.1"]}


@defproc[(do-not-return) none/c]{
 This function does not return. If you are writing a script to load the @web-server
 you may want to call this functions at the end of your script.
}

}

@include-section["servlet-env-int.scrbl"]
