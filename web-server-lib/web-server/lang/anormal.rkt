#lang racket/base
(require (for-template racket/base)
         syntax/kerncase
         syntax/id-table
         racket/list
         racket/contract
         racket/match
         "util.rkt")
(provide/contract
 [make-anormal-term ((syntax? . -> . syntax?) . -> . (syntax? . -> . syntax?))])

; A-Normal Form
(struct id-context ()
  #:property prop:procedure
  (λ (ic x) x))
(define the-id-context (id-context))

;; a context is either
;;    frame
;;    (ccompose context frame)

;; a frame is either
;;    w -> target-redex
;;    (listof w) -> target-redex

;; ccompose: (w -> target-expr) (alpha -> target-redex) -> (alpha -> target-expr)
;; compose a context with a frame
(define (ccompose ctxt frame)
  (if (id-context? ctxt)
      frame
      (lambda (val)
        (let-values ([(x ref-to-x) (generate-formal 'x)])
          #`(#%plain-app (#%plain-lambda (#,x) #,(ctxt ref-to-x)) #,(frame val))))))

(define (make-anormal-term elim-letrec-term)
  (define (anormal-term stx)
    (anormal the-id-context stx))

  (define (detect-set!-ids* bm-map stx-l)
    (for ([x (in-list (syntax->list stx-l))])
      (detect-set!-ids bm-map x)))

  (define (detect-set!-ids bm-map stx)
    (kernel-syntax-case
     (disarm stx) (transformer?)     
     [(begin fbe be ...)
      (detect-set!-ids* bm-map #'(fbe be ...))]     
     [(begin0 fbe be ...)
      (detect-set!-ids* bm-map #'(fbe be ...))]
     [(set! v ve)
      (unless (bound-id-table-ref bm-map #'v 'none)
        (bound-id-table-set! bm-map #'v #t))
      (detect-set!-ids* bm-map #'(ve))]                    
     [(let-values ([(v ...) ve] ...) be ...)
      (detect-set!-ids* bm-map #'(ve ... be ...))]
     [(letrec-values ([(v ...) ve] ...) be ...)
      (detect-set!-ids* bm-map #'(ve ... be ...))]     
     [(#%plain-lambda formals be ...)
      (detect-set!-ids* bm-map #'(be ...))]     
     [(case-lambda [formals be ...] ...)
      (detect-set!-ids* bm-map #'(be ... ...))]
     [(if te ce ae)
      (detect-set!-ids* bm-map #'(te ce ae))]
     [(quote datum)
      (void)]
     [(quote-syntax datum)
      (void)]
     [(with-continuation-mark ke me be)
      (detect-set!-ids* bm-map #'(ke me be))]       
     [(#%plain-app fe e ...)
      (detect-set!-ids* bm-map #'(fe e ...))]
     [(#%top . v)
      (void)]
     [(#%variable-reference . v)
      (void)]
     [id (identifier? #'id)
         (void)]
     [(letrec-syntaxes+values ([(sv ...) se] ...)
                              ([(vv ...) ve] ...)
        be ...)
      (detect-set!-ids* bm-map #'(se ... ve ... be ...))]
     [(#%expression d)
      (detect-set!-ids* bm-map #'(d))]
     [_
      (raise-syntax-error 'detect-set!-ids "Dropped through:" stx)]))

  (define (remove-set!-ids* bm-map stx-l)
    (for/list ([x (in-list (syntax->list stx-l))])
      (remove-set!-ids bm-map x)))
  
  (define (remove-set!-ids** bm-map stx-l)
    (for/list ([x (in-list (syntax->list stx-l))])
      (remove-set!-ids* bm-map x)))
  
  (define (remove-set!-ids bm-map stx)
    (rearm
     stx
     (kernel-syntax-case
      (disarm stx) (transformer?)     
      [(begin fbe be ...)
       (with-syntax ([(nfbe nbe ...)
                      (remove-set!-ids* bm-map #'(fbe be ...))])
         (syntax/loc stx
           (begin nfbe nbe ...)))]
      [(begin0 fbe be ...)
       (with-syntax ([(nfbe nbe ...)
                      (remove-set!-ids* bm-map #'(fbe be ...))])
         (syntax/loc stx
           (begin0 nfbe nbe ...)))]
      [(set! v ve)
       (with-syntax ([nve (remove-set!-ids bm-map #'ve)])
         (if (bound-id-table-ref bm-map #'v #f)
           (syntax/loc stx
             (#%plain-app set-box! v nve))
           (syntax/loc stx
             (set! v nve))))]
      [(let-values ([(v ...) ve] ...) be ...)
       (with-syntax ([(nve ...) (remove-set!-ids* bm-map #'(ve ...))]
                     [(nbe ...) (remove-set!-ids* bm-map #'(be ...))])
         (syntax/loc stx
           (let-values ([(v ...) nve] ...) nbe ...)))]
      [(letrec-values ([(v ...) ve] ...) be ...)
       (with-syntax ([(nve ...) (remove-set!-ids* bm-map #'(ve ...))]
                     [(nbe ...) (remove-set!-ids* bm-map #'(be ...))])
         (syntax/loc stx
           (letrec-values ([(v ...) nve] ...) nbe ...)))]     
      [(#%plain-lambda formals be ...)
       (with-syntax ([(nbe ...) (remove-set!-ids* bm-map #'(be ...))])
         (syntax/loc stx
           (#%plain-lambda formals nbe ...)))]
      [(case-lambda [formals be ...] ...)
       (with-syntax ([((nbe ...) ...) (remove-set!-ids** bm-map #'((be ...) ...))])
         (syntax/loc stx
           (case-lambda [formals nbe ...] ...)))]
      [(if te ce ae)
       (with-syntax ([(nte nce nae) (remove-set!-ids* bm-map #'(te ce ae))])
         (syntax/loc stx
           (if nte nce nae)))]
      [(quote datum)
       stx]
      [(quote-syntax datum)
       stx]
      [(with-continuation-mark ke me be)
       (with-syntax ([(nke nme nbe) (remove-set!-ids* bm-map #'(ke me be))])
         (syntax/loc stx
           (with-continuation-mark nke nme nbe)))]       
      [(#%plain-app fe e ...)
       (with-syntax ([(nfe ne ...) (remove-set!-ids* bm-map #'(fe e ...))])
         (syntax/loc stx
           (#%plain-app nfe ne ...)))]
      [(#%top . v)
       stx]
      [(#%variable-reference . v)
       stx]
      [id (identifier? #'id)
          (if (bound-id-table-ref bm-map #'id #f)
            (syntax/loc stx
              (#%plain-app unbox id))
            stx)]
      [(letrec-syntaxes+values ([(sv ...) se] ...)
                               ([(vv ...) ve] ...)
         be ...)
       (with-syntax ([((nse ...)
                       (nve ...)
                       (nbe ...))
                      (remove-set!-ids** bm-map #'((se ...)
                                                   (ve ...)
                                                   (be ...)))])
         (syntax/loc stx
           (letrec-syntaxes+values ([(sv ...) nse] ...)
                                   ([(vv ...) nve] ...)
             nbe ...)))]
      [(#%expression d)
       (with-syntax ([nd (remove-set!-ids bm-map #'d)])
         (syntax/loc stx
           (#%expression nd)))]
      [_
       (raise-syntax-error 'remove-set!-ids "Dropped through:" stx)])))
  
  (define (anormal ctxt stx)
    (rearm
     stx
     (kernel-syntax-case
         (disarm stx) (transformer?)
       [(begin)
        (anormal ctxt (syntax/loc stx (#%plain-app void)))]
       [(begin lbe)
        (anormal ctxt (syntax/loc stx lbe))]
       [(begin fbe be ...)
        (anormal ctxt 
                 (syntax/loc stx 
                   (#%plain-app call-with-values
                                (#%plain-lambda () fbe)
                                (#%plain-lambda throw-away
                                                (begin be ...)))))]
       [(begin0 lbe)
        (anormal ctxt (syntax/loc stx lbe))]
       [(begin0 fbe be ...)
        (let-values ([(save ref-to-save) (generate-formal 'save)])
          (anormal ctxt 
                   (quasisyntax/loc stx 
                     (#%plain-app call-with-values
                                  (#%plain-lambda () fbe)
                                  (#%plain-lambda #,save
                                                  (begin be ... 
                                                         (#%plain-app apply values #,ref-to-save)))))))]
       [(set! v ve)
        (anormal
         (ccompose ctxt
                   (lambda (val)
                     (quasisyntax/loc stx (set! v #,val))))
         #'ve)]
       [(let-values () be)
        (anormal ctxt (syntax/loc stx be))]
       [(let-values ([(v) ve]) be)
        (anormal ctxt
                 (syntax/loc stx 
                   (#%plain-app (#%plain-lambda (v) be)
                                ve)))]
       [(let-values ([(v ...) ve]) be)
        (anormal ctxt
                 (syntax/loc stx 
                   (#%plain-app call-with-values
                                (#%plain-lambda () ve)
                                (#%plain-lambda (v ...) be))))]
       [(let-values ([(fv ...) fve] [(v ...) ve] ...) be)
        (anormal ctxt
                 (syntax/loc stx 
                   (let-values ([(fv ...) fve])
                     (let-values ([(v ...) ve] ...)
                       be))))]
       [(let-values ([(v ...) ve] ...) be ...)
        (anormal ctxt
                 (syntax/loc stx 
                   (let-values ([(v ...) ve] ...)
                     (begin be ...))))]
       [(letrec-values ([(v ...) ve] ...) be ...)
        (anormal ctxt
                 (elim-letrec-term stx))]
       [(#%plain-lambda formals be)
        (let ()
          (define the-formals (formals-list #'formals))
          (define bm-map (make-bound-id-table))
          (for ([f (in-list the-formals)])
            (bound-id-table-set! bm-map f #f))
          (detect-set!-ids bm-map #'be)
          (define set!less-be (remove-set!-ids bm-map #'be))
          (define set!-ids
            (filter (λ (f)
                      (bound-id-table-ref bm-map f #f))
                    the-formals))
          (define set!less+boxed-be
            (with-syntax ([(f ...) set!-ids])
              (quasisyntax/loc stx
                (let-values ([(f) (#%plain-app box f)]
                             ...)
                  #,set!less-be))))
          (with-syntax ([nbe (anormal-term set!less+boxed-be)])
            (ctxt (syntax/loc stx (#%plain-lambda formals nbe)))))]
       [(#%plain-lambda formals be ...)
        (anormal ctxt
                 (syntax/loc stx
                   (#%plain-lambda formals (begin be ...))))]
       [(case-lambda [formals be] ...)
        (with-syntax ([(be ...) (map anormal-term (syntax->list #'(be ...)))])
          (ctxt (syntax/loc stx (case-lambda [formals be] ...))))]
       [(case-lambda [formals be ...] ...)
        (anormal ctxt
                 (syntax/loc stx (case-lambda [formals (begin be ...)] ...)))]
       [(if te ce ae)
        (anormal
         (ccompose ctxt
                   (lambda (val)
                     (quasisyntax/loc stx 
                       (if #,val
                           #,(anormal-term #'ce)
                           #,(anormal-term #'ae)))))
         #'te)]
       [(quote datum)
        (ctxt stx)]
       [(quote-syntax datum)
        (ctxt stx)]
       [(with-continuation-mark ke me be)
        (anormal
         (ccompose ctxt
                   (lambda (kev)
                     (anormal 
                      (lambda (mev)
                        (quasisyntax/loc stx 
                          (with-continuation-mark #,kev #,mev
                            #,(anormal-term #'be))))
                      #'me)))
         #'ke)]       
       [(#%plain-app fe e ...)
        (anormal
         (lambda (val0)
           (anormal*
            (ccompose ctxt
                      (lambda (rest-vals)
                        (quasisyntax/loc stx 
                          (#%plain-app #,val0 #,@rest-vals))))
            (syntax->list #'(e ...))))
         #'fe)]
       [(#%top . v)
        (ctxt stx)]
       [(#%variable-reference . v)
        (ctxt stx)]
       [id (identifier? #'id)
           (ctxt stx)]
       [(letrec-syntaxes+values ([(sv ...) se] ...)
          ([(vv ...) ve] ...)
          be ...)
        (anormal ctxt (elim-letrec-term stx))]
       [(#%expression d)
        (anormal
         (if #t
           ctxt
           ;; d is a variable reference, so it is always obviously an
           ;; expression; furthermore, we have already done expansion,
           ;; so we know that d is definedly an expression (we're
           ;; inside of anormal-term, rather than the various
           ;; define/module forms), so we can get rid of it. This
           ;; ensures that the id-context is preserved and thus
           ;; multi-value returns don't generate a tail-lambda.
           (ccompose ctxt
                     (lambda (d)
                       (quasisyntax/loc stx (#%expression #,d)))))
         #'d)]
       [_
        (raise-syntax-error 'anormal "Dropped through:" stx)])))
  
  ;; anormal*: ((listof w) -> target-expr) (listof source-expr) -> target-expr
  ;; normalize an expression given as a context and list of sub-expressions
  (define (anormal* multi-ctxt exprs)
    (match exprs
      [(list) 
       (multi-ctxt '())]
      [(list-rest fe re)
       (anormal
        (lambda (val)
          (anormal*
           (lambda (rest-vals)
             (multi-ctxt (list* val rest-vals)))
           re))
        fe)]))
  
  anormal-term)
