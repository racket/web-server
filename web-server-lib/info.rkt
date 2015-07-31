#lang info

(define collection 'multi)

(define deps '("srfi-lite-lib"
               ("base" #:version "6.2.900.6")
	       "net-lib"
               "compatibility-lib"
               "scribble-text-lib"
               "unstable-contract-lib"
               "parser-tools-lib"))
(define build-deps '("rackunit-lib"))

(define pkg-desc "implementation (no documentation) part of \"web-server\"")

(define pkg-authors '(jay))

(define version "1.1")
