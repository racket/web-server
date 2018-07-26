#lang racket/base
(module+ test
  (require net/url
           tests/eli-tester
           (submod web-server/http/request internal-test))

  (test
   (read-request-line (open-input-bytes #"GET /foo HTTP/1.1"))
   => (values #"GET" (url #f #f #f #f #t (list (path/param "foo" '()))
                          '() #f) 1 1)
   (read-request-line (open-input-bytes #"GET /foo/bar HTTP/1.1"))
   => (values #"GET" (url #f #f #f #f #t (list (path/param "foo" '())
                                               (path/param "bar" '()))
                          '() #f) 1 1)
   (read-request-line (open-input-bytes #"GET //foo/bar HTTP/1.1"))
   => (values #"GET" (url #f #f "" #f #t (list (path/param "" '())
                                               (path/param "foo" '())
                                               (path/param "bar" '()))
                          '() #f) 1 1)))
