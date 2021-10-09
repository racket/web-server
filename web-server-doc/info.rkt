#lang info

(define collection 'multi)

(define build-deps '("net-doc"
                     "net-cookies-doc"
                     "rackunit-doc"
                     "compatibility-doc"
                     "db-doc"
                     "scribble-doc"
                     "compatibility-lib"
                     "db-lib"
		     "net-lib"
                     "net-cookies-lib"
                     "rackunit-lib"
                     "sandbox-lib"
                     "scribble-lib"
                     "web-server-lib"
                     "racket-doc"))
(define deps '("base"))
(define update-implies '("web-server-lib"))

(define pkg-desc "documentation part of \"web-server\"")

(define pkg-authors '(jay))

(define license
  '(Apache-2.0 OR MIT))
