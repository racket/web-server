#lang web-server/base

(require web-server/http)

; not formlet/c  xexpr-forest/c formlet*/c (contracts)
(provide pure
         cross 
         cross* 
         xml-forest 
         xml
         text 
         tag-xexpr
         formlet-display 
         formlet-process)

; Combinators
(define (id x) x)

(define (const x)
  (λ args x))

; Formlets
(define (pure x)
  (lambda (i)
    (values null (const x) i)))

(define (cross f p)
  (lambda (i)
    (let*-values ([(x1 a1 i) (f i)]
                  [(x2 a2 i) (p i)])
      (values (append x1 x2)
              (lambda (env)
                (call-with-values (lambda () (a2 env)) (a1 env)))
              i))))

;; This is gross because OCaml auto-curries
(define (cross* f . gs)
  (lambda (i)
    (let*-values ([(fx fp fi) (f i)]
                  [(gs-x gs-p gs-i)
                   (let loop ([gs gs]
                              [xs null]
                              [ps null]
                              [i fi])
                     (if (null? gs)
                         (values (reverse xs) (reverse ps) i)
                         (let-values ([(gx gp gi) ((car gs) i)])
                           (loop (cdr gs) (list* gx xs) (list* gp ps) gi))))])
      (values (apply append fx gs-x)
              (lambda (env)
                (let ([fe (fp env)]
                      [gs-e (for/list ([g (in-list gs-p)])
                              (g env))])
                  (apply fe gs-e)))
              gs-i))))

(define (xml-forest x)
  (lambda (i)
    (values x (const id) i)))

(define (xml x)
  (xml-forest (list x)))

(define (text x)
  (xml x))

(define (tag-xexpr t ats f)
  (lambda (i)
    (let-values ([(x p i) (f i)])
      (values (list (list* t ats x)) p i))))

; Helpers
(define (formlet-display f)
  (let-values ([(x p i) (f 0)])
    x))

(define (formlet-process f r)
  (let-values ([(x p i) (f 0)])
    (p (request-bindings/raw r))))
