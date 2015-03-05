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
                     net/tcp-sig
                     racket/tcp
                     racket/async-channel
                     unstable/contract
                     web-server/configuration/configuration-table)
          (prefix-in raw: (for-label net/tcp-unit))
          (prefix-in files: (for-label web-server/dispatchers/dispatch-files)))

@defmodule[web-server/web-server]{

This module provides functions for launching dispatching servers.

@defproc[(serve [#:dispatch dispatch dispatcher/c]
                [#:confirmation-channel confirmation-channel (or/c false/c async-channel?) #f]
                [#:connection-close? connection-close? boolean? #f]
                [#:dispatch-server-connect@ dispatch-server-connect@
                                            (unit/c (import)
                                                    (export dispatch-server-connect^))
                                            raw:dispatch-server-connect@]
                [#:tcp@ tcp@ (unit/c (import) (export tcp^)) raw:tcp@]
                [#:port port tcp-listen-port? 80]
                [#:listen-ip listen-ip (or/c string? false/c) #f]
                [#:max-waiting max-waiting integer? 511]
                [#:initial-connection-timeout initial-connection-timeout integer? 60])
         (-> void)]{
 Constructs an appropriate @racket[dispatch-server-config^], invokes the
 @racket[dispatch-server@], and calls its @racket[serve] function.
 
 If @racket[connection-close?] is @racket[#t], then every connection is closed after one
 request. Otherwise, the client decides based on what HTTP version it uses.
 
 The @racket[#:dispatch-server-connect@] argument supports the conversion of raw connections;
 for example, @racket[make-ssl-connect@] produces a unit to serve SSL by converting
 raw TCP ports to SSL ports; see also @secref["faq:https"].
 The @racket[#:tcp@] argument supports replacing TCP connections with other kinds of connections (and
  was formerly recommended for SLL support). Beware that the server expects the @racket[tcp-accept] operation
  from @racket[tcp@] to be effectively atomic; new connections are not accepted while @racket[tcp-accept]
  is in progress.

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

@history[#:changed "1.1" @elem{Added the @racket[#:dispatch-server-connect@] argument.}]}


@defproc[(serve/ports [#:dispatch dispatch dispatcher/c]
                      [#:confirmation-channel confirmation-channel (or/c false/c async-channel?) #f]
                      [#:connection-close? connection-close? boolean? #f]
                      [#:dispatch-server-connect@ dispatch-server-connect@
                                                  (unit/c (import)
                                                          (export dispatch-server-connect^))
                                                  raw:dispatch-server-connect@]
                      [#:tcp@ tcp@ (unit/c (import) (export tcp^)) raw:tcp@]
                      [#:ports ports (listof tcp-listen-port?) (list 80)]
                      [#:listen-ip listen-ip (or/c string? false/c) #f]
                      [#:max-waiting max-waiting integer? 511]
                      [#:initial-connection-timeout initial-connection-timeout integer? 60])
         (-> void)]{
 Calls @racket[serve] multiple times, once for each @racket[port], and returns
 a function that shuts down all of the server instances.

@history[#:changed "1.1" @elem{Added the @racket[#:dispatch-server-connect@] argument.}]}


@defproc[(serve/ips+ports [#:dispatch dispatch dispatcher/c]
                          [#:confirmation-channel confirmation-channel (or/c false/c async-channel?) #f]
                          [#:connection-close? connection-close? boolean? #f]
                          [#:dispatch-server-connect@ dispatch-server-connect@
                                                      (unit/c (import)
                                                              (export dispatch-server-connect^))
                                                      raw:dispatch-server-connect@]
                          [#:tcp@ tcp@ (unit/c (import) (export tcp^)) raw:tcp@]
                          [#:ips+ports ips+ports (listof (cons/c (or/c string? false/c) (listof tcp-listen-port?))) (list (cons #f (list 80)))]
                          [#:max-waiting max-waiting integer? 511]
                          [#:initial-connection-timeout initial-connection-timeout integer? 60])
         (-> void)]{
 Calls @racket[serve/ports] multiple times, once for each @racket[ip], and returns
 a function that shuts down all of the server instances.

@history[#:changed "1.1" @elem{Added the @racket[#:dispatch-server-connect@] argument.}]}

                  
@defproc[(serve/web-config@ [config@ (unit/c (import) (export web-config^))]
                            [#:dispatch-server-connect@ dispatch-server-connect@
                                                        (unit/c (import)
                                                                (export dispatch-server-connect^))
                                                        raw:dispatch-server-connect@]
                            [#:tcp@ tcp@ (unit/c (import) (export tcp^))
                                    raw:tcp@])
         (-> void)]{
 Starts the @web-server with the settings defined by the given @racket[web-config^] unit.
        
 Combine @racket[serve/web-config@] with @racket[configuration-table->web-config@] and @racket[configuration-table-sexpr->web-config@]:
 
 @racketblock[
  (serve/web-config@
   (configuration-table->web-config@ 
    default-configuration-table-path))]

@history[#:changed "1.1" @elem{Added the @racket[#:dispatch-server-connect@] argument.}]}


@defthing[raw:dispatch-server-connect@ (unit/c (import) (export dispatch-server-connect^))]{

A default implementation of the dispatch server's connection-conversion abstraction that performs no conversion.

@history[#:added "1.1"]}


@defproc[(make-ssl-connect@ [server-cert-file path-string?] 
                            [server-key-file path-string?])
         (unit/c (import) (export dispatch-server-connect^))]{

Constructs an implementation of the dispatch server's connection-conversion abstraction for OpenSSL.

@history[#:added "1.1"]}


@defproc[(do-not-return) void]{
 This function does not return. If you are writing a script to load the @web-server
 you are likely to want to call this functions at the end of your script.
}

}

@include-section["servlet-env-int.scrbl"]
