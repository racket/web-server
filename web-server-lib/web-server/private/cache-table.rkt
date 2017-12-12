#lang racket/base
(require racket/contract)

(define-struct cache-table (hash semaphore) #:mutable)

(define (new-cache-table)
  ; Only eq? tables are not locked
  (make-cache-table (make-hasheq)
                    (make-semaphore 1)))

(define (cache-table-clear! ct [entry-ids #f] [finalize void])
  (call-with-semaphore
   (cache-table-semaphore ct)
   (lambda ()
     (let ([cache-hash (cache-table-hash ct)])
       (if entry-ids
         (for ([entry-id (in-list entry-ids)])
           (when (hash-has-key? cache-hash entry-id)
             (finalize (hash-ref cache-hash entry-id))
             (hash-remove! cache-hash entry-id)))
         (begin
           (for ([entry (in-hash-values cache-hash)])
             (finalize entry))
           (set-cache-table-hash! ct (make-hasheq))))))))

(define (cache-table-lookup! ct entry-id entry-thunk)
  (define ht (cache-table-hash ct))
  (define sema (cache-table-semaphore ct))
  ; Fast lookup
  (hash-ref
   ht entry-id
   (lambda ()
     ; Now lock for relookup and computation
     (call-with-semaphore
      sema
      (lambda ()
        (hash-ref! ht entry-id entry-thunk))))))

(provide/contract
 [rename new-cache-table make-cache-table
         (-> cache-table?)]
 [cache-table-lookup! (cache-table? symbol? (-> any/c) . -> . any/c)]
 [cache-table-clear! ((cache-table?) ((or/c false/c (listof symbol?)) (-> any/c void?)) . ->* . void?)]
 [cache-table? (any/c . -> . boolean?)])
