#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "connection-manager"]{Connection Manager}
@(require (for-label web-server/private/connection-manager
                     web-server/private/dispatch-server-unit
                     web-server/safety-limits
                     web-server/private/timer))

@defmodule[web-server/private/connection-manager]{

This module provides functionality for managing pairs of
input and output ports. We have plans to allow a number of different strategies
for doing this.

@defstruct[connection
           ([timer timer?]
            [i-port input-port?] [o-port output-port?] [custodian custodian?]
            [close? boolean?])]{
 A connection is a pair of ports (@racket[i-port] and @racket[o-port]) that is
 ready to close after the current job if @racket[close?] is @racket[#t]. Resources
 associated with the connection should be allocated under @racket[custodian].
 The connection will last until @racket[timer] triggers.

 Construct @racket[connection] instances using @racket[new-connection],
 which cooperates with @tech{connection managers}.
 Constructing connections by other means (e.g. @racket[make-connection]
 or @racket[struct-copy]) may have undesirable consequences,
 such as circumventing @tech{safety limits}.

 @history[#:changed "1.6"
          @elem{Deprecate construction other than via @racket[new-connection].}]
}

 @deftogether[
 (@defproc[(connection-manager? [v any/c]) boolean?]
   @defproc[(start-connection-manager
             [#:safety-limits safety-limits safety-limits? (make-unlimited-safety-limits)])
            connection-manager?])]{

  A @deftech{connection manager} is an opaque value, recognized by the
  predicate @racket[connection-manager?], which cooperates with
  @racket[new-connection] to control the creation and behavior of
  @racket[connection] instances.
  It encapsulates a timer manager (see @secref["timer"]), @tech{safety limits} policies,
  and other internal data.
  Use @racket[start-connection-manager] to create a @tech{connection manager}.

  Note that, if the @racket[safety-limits] argument is not given,
  the default @tech{safety limits} value offers minimal protection against
  malicious or misbehaving clients and servlets: see @racket[make-unlimited-safety-limits].
  Most programs should not not use @racket[start-connection-manager] or
  @racket[new-connection] directly: higher-level interfaces,
  such as @racket[dispatch-server-with-connect@] and @racket[serve],
  incorporate connection management and provide more protections by default.
  The permissive default @tech{safety limits} of @racket[start-connection-manager]
  maximize backwards-compatability for low-level programs that use these functions directly.
  See the @elemref["safety-limits-porting"]{safety limits compatability note}
  for more information.

  @history[#:changed "1.6"
           @elem{Added @racket[safety-limits] argument.}]
}

 @defproc*[([(new-connection [cm connection-manager?]
                             [i-port input-port?]
                             [o-port output-port?]
                             [cust custodian?]
                             [close? boolean?])
             connection?]
            [(new-connection [cm connection-manager?]
                             [timeout number?]
                             [i-port input-port?]
                             [o-port output-port?]
                             [cust custodian?]
                             [close? boolean?])
             connection?])]{
  Cooperates with the @tech{connection manager} @racket[cm] to construct
  a new @racket[connection] instance.
  The connection is created with a timer that effectively calls @racket[kill-connection!].
  The initial timeout is determened by the @tech{safety limits} encapsulated in @racket[cm].

  The six-argument form with a @racket[timeout] argument is provided for
  backwards compatability. In that case, @racket[timeout] is used for the initial
  value of the connection's timer, regardless of @racket[cm]'s @tech{safety limits}.
  Other aspects of the @tech{safety limits} still apply to the resulting connection.

  @history[#:changed "1.6"
           @elem{Added five-argument form using @racket[cm]'s @tech{safety limits}
              instead of a @racket[timeout] argument.}]
 }

@defproc[(kill-connection! [c connection?])
         any]{
 Closes the ports associated with @racket[c], kills the timer, and shuts down
 the custodian.
}

@defproc[(adjust-connection-timeout! [c connection?]
                                     [t number?])
         any]{
 Calls @racket[increment-timer!] with the timer behind @racket[c] with @racket[t].
}

@defproc[(reset-connection-timeout! [c connection?]
                                    [t number?])
         any]{

 Calls @racket[reset-timer!] with the timer behind @racket[c] with @racket[t].

  @history[#:added "1.6"]
}

}
