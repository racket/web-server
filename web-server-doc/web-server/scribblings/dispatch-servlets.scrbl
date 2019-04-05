#lang scribble/doc
@(require "web-server.rkt"
          (for-label web-server/http
                     web-server/private/servlet
                     web-server/configuration/responders
                     web-server/dispatchers/filesystem-map
                     web-server/servlet/setup
                     net/url
                     web-server/managers/manager))

@title[#:tag "dispatch-servlets"]{Serving Servlets}
@a-dispatcher[web-server/dispatchers/dispatch-servlets
              @elem{defines a dispatcher constructor
                    that runs servlets.}]{
          
@defthing[url->servlet/c contract?]{Equivalent to @racket[(url? . -> . servlet?)]}

@defproc[(make-cached-url->servlet
          [url->path url->path/c]
          [path->serlvet path->servlet/c])
         (values (->* () ((or/c false/c (listof url?)) (-> servlet? void?)) void?)
                 url->servlet/c)]{
 The first return value is a procedure that flushes the cache. If its first
 optional argument is @racket[#f] (the default), all servlet caches are
 flushed. Otherwise, only those servlet caches to which @racket[url->path]
 maps the given URLs are flushed. The second optional argument is a procedure
 which is invoked on each cached value before it is flushed, which can be used
 to finalize servlet resources. Beware that the default value @racket[void]
 performs no finalization. In particular, it does not shut down the servlet's
 custodian, instead allowing the servlet's custodian-managed resources (such
 as threads) to persist.
 
 The second return value is a procedure that uses
 @racket[url->path] to resolve the URL to a path, then uses
 @racket[path->servlet] to resolve that path to a servlet, caching the
 results in an internal table.

 @history[#:changed "1.3" "Added optional argument to first return value for list of URLs."
	  #:changed "1.3" "Added optional argument to first return value for servlet finalizer procedure."]
}

@defproc[(make [url->servlet url->servlet/c]
               [#:responders-servlet-loading
                responders-servlet-loading
                (url? exn? . -> . can-be-response?)
                servlet-loading-responder]
               [#:responders-servlet
                responders-servlet
                (url? exn? . -> . can-be-response?)
                servlet-error-responder])
         dispatcher/c]{
 This dispatcher runs racket servlets, using @racket[url->servlet] to
 resolve URLs to the underlying servlets.  If servlets have errors
 loading, then @racket[responders-servlet-loading] is used. Other errors
 are handled with @racket[responders-servlet]. If a servlet raises calls
 @racket[next-dispatcher], then the signal is propagated by this
 dispatcher.
}

}

@include-section["servlet-setup.scrbl"]

@include-section["namespace.scrbl"]

@section{Internal Servlet Representation}

@defmodule[web-server/private/servlet]{
  @defstruct[servlet ([custodian custodian?]
                      [namespace namespace?]
                      [manager manager?]
                      [directory path-string?]
                      [handler (request? . -> . can-be-response?)])
                     #:mutable]{
   Instances of this structure hold the necessary parts of a servlet:
   the @racket[custodian] responsible for the servlet's resources,
   the @racket[namespace] the servlet is executed within,
   the @racket[manager] responsible for the servlet's continuations,
   the current @racket[directory] of the servlet,
   and the @racket[handler] for all requests to the servlet.
  }
}
