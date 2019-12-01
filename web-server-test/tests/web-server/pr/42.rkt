#lang racket/base
(module+ test
  (require net/url
           tests/eli-tester
           (submod web-server/http/request internal-test))

  (define max-request-line-length
    (* 8 1024))

  (test
   (read-request-line (open-input-bytes #"GET /foo HTTP/1.1") max-request-line-length)
   => (values #"GET" (url #f #f #f #f #t (list (path/param "foo" '()))
                          '() #f) 1 1)
   (read-request-line (open-input-bytes #"GET /foo/bar HTTP/1.1") max-request-line-length)
   => (values #"GET" (url #f #f #f #f #t (list (path/param "foo" '())
                                               (path/param "bar" '()))
                          '() #f) 1 1)
   (read-request-line (open-input-bytes #"GET //foo/bar HTTP/1.1") max-request-line-length)
   => (values #"GET" (url #f #f "" #f #t (list (path/param "" '())
                                               (path/param "foo" '())
                                               (path/param "bar" '()))
                          '() #f) 1 1)))
