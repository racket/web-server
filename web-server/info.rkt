#lang info

(define collection 'multi)

(define deps '("web-server-lib"
               "web-server-doc"))
(define implies '("web-server-lib"
                  "web-server-doc"))

(define pkg-desc "An HTTP server")

(define pkg-authors '(jay))

(define license
  '(Apache-2.0 OR MIT))
