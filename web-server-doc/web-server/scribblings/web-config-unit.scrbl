#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "web-config-unit" #:tag-prefix "web-config"]{Configuration Units}
@(require (for-label web-server/web-config-unit
                     web-server/configuration/namespace
                     web-server/configuration/configuration-table
                     web-server/configuration/configuration-table-structs
                     web-server/private/util
                     web-server/servlet/setup
                     web-server/safety-limits
                     racket/tcp
                     (prefix-in servlets: web-server/dispatchers/dispatch-servlets)
                     web-server/web-config-sig))

@section[#:style 'hidden]{Signature}

@defmodule[web-server/web-config-sig]{

@defsignature[web-config*^ ()]{

  Contains the following identifiers.

  @history[#:added "1.6"]

@defthing[safety-limits safety-limits?]{
   A @tech{safety limits} value specifying the policies to be used
   while reading and handling requests.
  }
 
@defthing[virtual-hosts (string? . -> . host?)]{
 Contains the configuration of individual virtual hosts.
}

@defthing[port port-number?]{
 Specifies the port to serve HTTP on.
}

@defthing[listen-ip (or/c #f string?)]{
 Passed to @racket[tcp-listen].
}

@defthing[make-servlet-namespace make-servlet-namespace/c]{
 Passed to @racket[servlets:make] through @racket[make-default-path->servlet].
}
 }
                                      
@defsignature[web-config^ (web-config*^)]{
  @signature-desc[@deprecated[#:what "signature" @racket[web-config*^]]]
  
  For backwards compatability, @racket[web-config^] @racket[extends] @racket[web-config*^] and
  uses @racket[define-values-for-export] to define @sigelem[web-config*^ safety-limits] as:
  @racketblock[
 (make-safety-limits
  #:max-waiting #,(sigelem web-config^ max-waiting)
  #:request-read-timeout #,(sigelem web-config^ initial-connection-timeout))]
  
  @history[#:changed "1.6"
           @elem{Deprecated in favor of @racket[web-config*^].
              See @elemref["safety-limits-porting"]{compatability note}.}]

@defthing[max-waiting exact-nonnegative-integer?]{
 Passed to @racket[make-safety-limits].
}
@defthing[initial-connection-timeout timeout/c]{
  Passed to @racket[make-safety-limits] as its @racket[#:request-read-timeout] argument.
  @history[#:changed "1.6"
           @elem{Loosened contract for consistency with @racket[make-safety-limits].}]
}
}
             
}

@section[#:style 'hidden]{Unit}

@defmodule[web-server/web-config-unit]{

@defproc[(configuration-table->web-config@ [path path-string?]
                                           [#:port port (or/c false/c port-number?) #f]
                                           [#:listen-ip listen-ip (or/c false/c string?) #f]
                                           [#:make-servlet-namespace make-servlet-namespace make-servlet-namespace/c (make-make-servlet-namespace)])
         (unit/c (import) (export web-config^))]{
 Reads the S-expression at @racket[path] and calls
 @racket[configuration-table-sexpr->web-config@] appropriately.
}

@defproc[(configuration-table-sexpr->web-config@ [sexpr list?]
                                                 [#:web-server-root web-server-root path-string?
                                                                    (directory-part default-configuration-table-path)]
                                                 [#:port port (or/c false/c port-number?) #f]
                                                 [#:listen-ip listen-ip (or/c false/c string?) #f]
                                                 [#:make-servlet-namespace make-servlet-namespace make-servlet-namespace/c
                                                                           (make-make-servlet-namespace)])
         (unit/c (import) (export web-config^))]{
 Parses @racket[sexpr] as a configuration-table and constructs a @racket[web-config^] unit.
}

}
