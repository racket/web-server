#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "dispatch-server-unit"]{Dispatching Server}
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

The @web-server is just a configuration of a dispatching server.

@section{Dispatching Server Signatures}

@defmodule[web-server/private/dispatch-server-sig]{

The @racketmodname[web-server/private/dispatch-server-sig] library
provides the signatures @racket[dispatch-server^], @racket[dispatch-server-connect^],
and @racket[dispatch-server-config*^].

@defsignature[dispatch-server^ ()]{

The @racket[dispatch-server^] signature is an alias for
@racket[web-server^].

 @defproc[(serve [#:confirmation-channel confirmation-ach
                  (or/c #f (async-channel/c
                            (or/c exn? port-number?)))
                  #f])
          (-> any)]{
   Runs the server.
   The confirmation channel, if provided, will be sent an exception if one occurs
   while starting the server or the port number if the server starts successfully.
   
   Calling the returned procedure shuts down the server.
 }

 @defproc[(serve-ports [ip input-port?]
                       [op output-port?])
          any]{
 Asynchronously serves a single connection represented by the ports @racket[ip] and
 @racket[op].
 }
}

@defsignature[dispatch-server-connect^ ()]{

The @racket[dispatch-server-connect^] signature abstracts the conversion of connection
ports (e.g., to implement SSL) as used by the dispatch server.

 @defproc[(port->real-ports [ip input-port?]
                            [op output-port?])
          (values input-port? output-port?)]{
  Converts connection ports as necessary.

  The connection ports are normally TCP ports, but an alternate
  implementation of @racket[tcp^] linked to the dispatcher can supply
  different kinds of ports.
 }
}

@defsignature[dispatch-server-config*^ ()]{

  @history[#:added "1.6"]
   
 @defthing[port listen-port-number?]{
   Specifies the port to serve on.
  }
 @defthing[listen-ip (or/c string? #f)]{
   Passed to @racket[tcp-listen].
  }
 @defproc[(read-request [c connection?]
                        [p listen-port-number?]
                        [port-addresses
                         (input-port? . -> . (values string? string?))])
          (values any/c boolean?)]{
   Defines the way the server reads requests off connections to be passed
   to @sigelem[dispatch-server-config*^ dispatch].
   The @racket[port-addresses] argument should be a procedure
   like @sigelem[tcp^ tcp-addresses].
   
   The first result of @sigelem[dispatch-server-config*^ read-request] is ordinarily a @racket[request] value,
   but that is not a requirement at the dispatch-server level.
   The second result is @racket[#true] if the connection @racket[c] should be closed
   after handling this request, or @racket[#false] if the connection may be reused.
 }
 @defthing[dispatch (-> connection? any/c any)]{
   Used to handle requests.
   The second argument to @sigelem[dispatch-server-config*^ dispatch] is ordinarily a @racket[request] value,
   like the first result of @sigelem[dispatch-server-config*^ read-request],
   but that is not a requirement at the dispatch-server level.
  }
 @defthing[safety-limits safety-limits?]{
   A @tech{safety limits} value specifying the policies to be used
   while reading and handling requests.
  }
}

}

@defsignature[dispatch-server-config^ (dispatch-server-config*^)]{
 @signature-desc[@deprecated[#:what "signature" @racket[dispatch-server-config*^]]]
  
 For backwards compatability, @racket[dispatch-server-config^]
 @racket[extends] @racket[dispatch-server-config*^] and uses @racket[define-values-for-export]
 to define @sigelem[dispatch-server-config*^ safety-limits] as:
 @racketblock[
 (make-safety-limits
  #:max-waiting #,(sigelem dispatch-server-config^ max-waiting)
  #:request-read-timeout #,(sigelem dispatch-server-config^ initial-connection-timeout))]
  
 @history[#:changed "1.6"
          @elem{Deprecated in favor of @racket[dispatch-server-config*^].
            See @elemref["safety-limits-porting"]{compatability note}.}]
 
 @defthing[max-waiting exact-nonnegative-integer?]{
  Passed to @racket[make-safety-limits].
 }
 @defthing[initial-connection-timeout timeout/c]{
  Passed to @racket[make-safety-limits]  as its @racket[#:request-read-timeout] argument.
  @history[#:changed "1.6"
           @elem{Loosened contract for consistency with @racket[make-safety-limits].}]
 }
}



@include-section["safety-limits.scrbl"]



@section{Dispatching Server Unit}

@defmodule[web-server/private/dispatch-server-unit]

The @racketmodname[web-server/private/dispatch-server-unit] module
provides the unit that actually implements a dispatching server.

@defthing[dispatch-server-with-connect@ (unit/c (import tcp^
                                                        dispatch-server-connect^
                                                        dispatch-server-config*^)
                                                (export dispatch-server^))]{
 Runs the dispatching server config in a very basic way, except that it uses
 @secref["connection-manager"] to manage connections.

 @history[#:added "1.1"
          #:changed "1.6"
          @elem{Use @racket[dispatch-server-config*^]
            rather than @racket[dispatch-server-config^].
            See @elemref["safety-limits-porting"]{compatability note}.}]
}


@defthing[dispatch-server@ (unit/c (import tcp^
                                           dispatch-server-config*^)
                                   (export dispatch-server^))]{
 Like @racket[dispatch-server-with-connect@], but using @racket[raw:dispatch-server-connect@].

 @history[#:changed "1.6"
          @elem{Use @racket[dispatch-server-config*^]
            rather than @racket[dispatch-server-config^].
            See @elemref["safety-limits-porting"]{compatability note}.}]
}


@section{Threads and Custodians}

The dispatching server runs in a dedicated thread. Every time a connection is initiated, a new thread is started to handle it.
Connection threads are created inside a dedicated custodian that is a child of the server's custodian. When the server is used to
provide servlets, each servlet also receives a new custodian that is a child of the server's custodian @bold{not} the connection
custodian.
