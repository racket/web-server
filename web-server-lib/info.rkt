#lang info

(define collection 'multi)

(define deps '("srfi-lite-lib"
               ("base" #:version "7.4.0.5")
               "net-lib"
               "net-cookies-lib"
               "scribble-text-lib"
               "parser-tools-lib"))
(define build-deps '("rackunit-lib"))

(define pkg-desc "implementation (no documentation) part of \"web-server\"")

(define pkg-authors '(jay))

(define version "1.13")

(define license
  '(Apache-2.0 OR MIT))
