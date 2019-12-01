#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "web-server-unit"]{Server Units}
@(require (for-label web-server/web-server-sig
                     web-server/web-server-unit
                     racket/async-channel
                     racket/tcp
                     net/tcp-sig
                     web-server/private/dispatch-server-sig
                     web-server/private/dispatch-server-unit
                     web-server/dispatchers/dispatch
                     web-server/web-config-sig
                     web-server/web-config-unit
                     web-server/web-server))

@section[#:tag "ws-sig" #:style 'hidden]{Signature}

@defmodule[web-server/web-server-sig]{

@defsignature[web-server^ ()]{

 @defproc[(serve [#:confirmation-channel confirmation-ach
                  (or/c #f (async-channel/c
                            (or/c exn? port-number?)))
                  #f])
          (-> any)]{
   The same as @sigelem[dispatch-server^ serve] from @racket[dispatch-server^].
   The @racket[dispatch-server^] signature is an alias for
   @racket[web-server^].
 }

 @defproc[(serve-ports [ip input-port?]
                       [op output-port?])
          any]{
 Asynchronously serves a single connection represented by the ports @racket[ip] and
 @racket[op].
 }
}

}

@section[#:tag "ws-unit" #:style 'hidden]{Unit}

@defmodule[web-server/web-server-unit]

@defthing[web-server-with-connect@ (unit/c (web-config*^ tcp^ dispatch-server-connect^)
                                           (web-server^))]{

Uses the @racket[web-config*^] to construct a @racket[dispatcher/c]
function that sets up one virtual host dispatcher, for each virtual
host in the @racket[web-config*^], that sequences the following
operations:

@itemize[
 @item{Logs the incoming request with the given format to the given file}
 @item{Performs HTTP Basic Authentication with the given password file}
 @item{Allows the @racket["/conf/refresh-passwords"] URL to refresh the password file.}
 @item{Allows the @racket["/conf/collect-garbage"] URL to call the garbage collector.}
 @item{Allows the @racket["/conf/refresh-servlets"] URL to refresh the servlets cache.}
 @item{Executes servlets mapping URLs to the given servlet root directory under htdocs.}
 @item{Serves files under the @racket["/"] URL in the given htdocs directory.}
]

 Using this @racket[dispatcher/c], it loads a dispatching server that provides
 @sigelem[web-server^ serve] and @sigelem[web-server^ serve-ports] functions
 that operate as expected.

 @history[#:added "1.1"
          #:changed "1.6"
          @elem{Use @racket[web-config*^] rather than @racket[web-config^].
            See @elemref["safety-limits-porting"]{compatability note}.}]
}


@defthing[web-server@ (unit/c (web-config*^ tcp^)
                              (web-server^))]{

Like @racket[web-server-with-connect@], but using @racket[raw:dispatch-server-connect@].

 @history[#:changed "1.6"
          @elem{Use @racket[web-config*^] rather than @racket[web-config^].
            See @elemref["safety-limits-porting"]{compatability note}.}]
}
