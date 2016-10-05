#lang web-server/base
(module+ test
  (define (test x a b)
    ((if (= a b) printf eprintf)
     "~a: ~a: ~a should be ~a\n"
     (if (= a b) " OK" "BAD")
     x
     a b))

  ;; Works at the top-level
  (define x 0)
  (set! x (add1 x))
  (test 'top-level x 1)

  ;; Works on let
  (let ([y 0])
    (set! y (add1 y))
    (test 'let y 1))

  ;; Works on letrec
  (letrec ([y 0])
    (set! y (add1 y))
    (test 'letrec y 1))
  
  ;; Works on define that turns into letrec
  (let ()
    (define (f x) (g y))
    (define y 0)
    (define (g x) y)
    (set! y (add1 y))
    (test 'define->letrec y 1))

  ;; Works on lambda arg
  ((λ (y)
     (set! y (add1 y))
     (test 'lambda y 1))
   0)

  ;; Works on define that turns into let
  (let ()
    (define y 0)
    (set! y (add1 y))
    (test 'define->let y 1)))
