#lang info

(define collection 'multi)

(define deps '("srfi-lite-lib"
               ("base" #:version "6.2.900.15")
               "net-lib"
               "net-cookies-lib"
               "compatibility-lib"
               "scribble-text-lib"
               "parser-tools-lib"))
(define build-deps '("rackunit-lib"))

(define pkg-desc "implementation (no documentation) part of \"web-server\"")

(define pkg-authors '(jay))

(define version "1.6")
