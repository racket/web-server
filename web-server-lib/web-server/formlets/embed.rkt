#lang racket/base

(require racket/contract
         web-server/http
         web-server/private/xexpr
         "unsafe/embed.rkt"
         "lib.rkt")

(provide
 (contract-out
  [embed-formlet 
   (-> (-> (-> request? any)
           string?)
       formlet*/c
       pretty-xexpr/c)]))
