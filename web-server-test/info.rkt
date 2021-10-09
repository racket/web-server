#lang info

(define collection 'multi)
(define deps '())
(define build-deps '("base"
                     "compatibility-lib"
                     "eli-tester"
                     "htdp-lib"
                     "rackunit-lib"
                     "web-server-lib"
                     "net-cookies"))
(define update-implies '("web-server-lib"))

(define pkg-desc "tests for \"web-server\"")

(define pkg-authors '(jay))

(define license
  '(Apache-2.0 OR MIT))
