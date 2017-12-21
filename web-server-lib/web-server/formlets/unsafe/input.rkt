#lang web-server/base

(require web-server/http
         (only-in "lib.rkt"
                  pure
                  cross))

(provide make-input* ; Low-level
         make-input
         ; HTML Spec
         input
         text-input
         password-input
         checkbox
         radio
         radio-group
         checkbox-group
         submit
         reset
         file-upload
         hidden
         img
         button
         multiselect-input
         select-input
         textarea-input
         ; High-level
         required
         default
         to-string
         to-number
         to-symbol
         to-boolean
         ; OLD
         input-string
         input-int
         input-symbol)

;; Convert UTF-8 bytes to string when needed.
(define (coerce-string/utf-8 bstr-or-str)
  (if (bytes? bstr-or-str)
      (bytes->string/utf-8 bstr-or-str)
      bstr-or-str))

; Low-level
(define (next-name i)
  (values (format "input_~a" i) (add1 i)))

(define (make-input*/forest render)
  (lambda (i)
    (let-values ([(w i) (next-name i)])
      (define wb (string->bytes/utf-8 w))
      (values (render w)
              (lambda (env) 
                (for/list ([b (in-list env)]
                           #:when (bytes=? wb (binding-id b)))
                  b))
              i))))

(define (make-input* render)
  (make-input*/forest
   (lambda (w)
     (list (render w)))))

(define (make-input render)
  (lambda (i)
    (let-values ([(w i) (next-name i)])
      (values (list (render w))
              (lambda (env) (bindings-assq (string->bytes/utf-8 w) env))
              i))))

(define binding:form-required
  (pure 
   (lambda (bf)
     (if (binding:form? bf)
         (binding:form-value bf)
         (error 'formlets "Missing required field")))))

(define (binding:form/default default)
  (pure
   (lambda (bf)
     (if (binding:form? bf)
         (binding:form-value bf)
         default))))

; HTML Spec
(define (input
         #:type [type "text"]
         #:value [value #f]
         #:size [size #f]
         #:max-length [max-length #f]
         #:read-only? [read-only? #f]
         #:attributes [attrs null])
  (make-input
   (lambda (n)
     (list 'input
           (list* (list 'name n)
                  (list 'type type)
                  (append
                   (filter list?
                           (list (and value (list 'value (coerce-string/utf-8 value)))
                                 (and size (list 'size (number->string size)))
                                 (and max-length (list 'maxlength (number->string max-length)))
                                 (and read-only? (list 'readonly "true"))))
                   attrs))))))

(define (text-input 
         #:value [value #f]
         #:size [size #f]
         #:max-length [max-length #f]
         #:read-only? [read-only? #f]
         #:attributes [attrs null])
  (input
   #:type "text"
   #:value value
   #:size size
   #:max-length max-length
   #:read-only? read-only?
   #:attributes attrs))

(define (password-input 
         #:value [value #f]
         #:size [size #f]
         #:max-length [max-length #f]
         #:read-only? [read-only? #f]
         #:attributes [attrs null])
  (input
   #:type "password"
   #:value value
   #:size size
   #:max-length max-length
   #:read-only? read-only?
   #:attributes attrs))

(define (checkbox value checked?
                  #:attributes [attrs null])
  (input
   #:type "checkbox"
   #:value value
   #:attributes
   (if checked? (append (list (list 'checked "true")) attrs) attrs)))

(define (radio value checked?
               #:attributes [attrs null])
  (input
   #:type "radio"
   #:value value
   #:attributes
   (if checked? (append (list (list 'checked "true")) attrs) attrs)))

(define (input-group l
                     #:kind kind
                     #:attributes [attrs (λ (x) null)]
                     #:checked? [checked? (λ (x) #f)]
                     #:display [display (λ (x) x)]
                     #:wrap [wrap (λ (x y) (list x y))])
  (define value->element (make-hasheq))
  (define i 0)
  (define (remember! e)
    (define this-i
      (begin0 i (set! i (add1 i))))
    (hash-set! value->element this-i e))
  (define (recall i)
    (hash-ref value->element i
              (λ () (error 'input-group "Invalid selection: ~e" i))))
  (for ([e l])
    (remember! e))
  (define (radio-first l)
    (if (string=? kind "radio")
        (car l)
        l))
  (cross
   (pure
    (lambda (bs)
      (radio-first
       (for/list ([b (in-list bs)])
         (recall (string->number (bytes->string/utf-8 (binding:form-value b))))))))
   (make-input*/forest
    (lambda (name)
      (apply append
             (for/list ([vn (in-range i)])
               (define e (hash-ref value->element vn))
               (define v (number->string vn))
               (wrap 
                `(input ([name ,name]
                         [type ,kind]
                         [value ,v]
                         ,@(if (checked? e)
                               '([checked "true"])
                               null)
                         ,@(attrs e)))
                (display e))))))))

(define (radio-group l 
                     #:attributes [attrs (λ (x) null)]
                     #:checked? [checked? (λ (x) #f)]
                     #:display [display (λ (x) x)]
                     #:wrap [wrap (λ (x y) (list x y))])
  (input-group l
               #:kind "radio"
               #:attributes attrs
               #:checked? checked?
               #:display display
               #:wrap wrap))

(define (checkbox-group l 
                        #:attributes [attrs (λ (x) null)]
                        #:checked? [checked? (λ (x) #f)]
                        #:display [display (λ (x) x)])
  (input-group l
               #:kind "checkbox"
               #:attributes attrs
               #:checked? checked?
               #:display display))

(define (submit value
                #:attributes [attrs null])
  (input
   #:type "submit"
   #:value value
   #:attributes attrs))

(define (reset value
               #:attributes [attrs null])
  (input
   #:type "reset"
   #:value value
   #:attributes attrs))

(define (file-upload #:attributes [attrs null])
  (input
   #:type "file"
   #:attributes attrs))

(define (hidden value #:attributes [attrs null])
  (input
   #:type "hidden"
   #:value value
   #:attributes attrs))

(define (button type text
                #:disabled [disabled #f]
                #:value [value #f]
                #:attributes [attrs null])
  (make-input
   (λ (n)
     (list 'button
           (list* (list 'name n)
                  (list 'type (coerce-string/utf-8 type))
                  (append 
                   (filter list?
                           (list (and disabled (list 'disabled (if disabled "true" "false")))
                                 (and value (list 'value (coerce-string/utf-8 value)))))
                   attrs))
           (coerce-string/utf-8 text)))))

(define (img alt src
             #:height [height #f]
             #:longdesc [ldesc #f]
             #:usemap [map #f]
             #:width [width #f]
             #:attributes [attrs null])
  (make-input
   (λ (n)
     (list 'img
           (list* (list 'name n)
                  (list 'src (coerce-string/utf-8 src))
                  (list 'alt (coerce-string/utf-8 alt))
                  (append
                   (filter list?
                           (list (and height (list 'height (number->string height)))
                                 (and ldesc (list 'longdesc (coerce-string/utf-8 ldesc)))
                                 (and map (list 'usemap (coerce-string/utf-8 map)))
                                 (and width (list 'width (number->string width)))))
                   attrs))))))

(define (multiselect-input l
                           #:attributes [attrs null]
                           #:multiple? [multiple? #t]
                           #:selected? [selected? (λ (x) #f)]
                           #:display [display (λ (x) x)])
  (define value->element (make-hasheq))
  (define i 0)
  (define (remember! e)
    (define this-i
      (begin0 i (set! i (add1 i))))
    (hash-set! value->element this-i e))
  (define (recall i)
    (hash-ref value->element i
              (λ () (error 'input-select* "Invalid selection: ~e" i))))
  (for ([e l])
    (remember! e))
  (cross
   (pure
    (lambda (bs)
      (for/list ([b (in-list bs)])
        (recall (string->number (bytes->string/utf-8 (binding:form-value b)))))))
   (make-input*
    (lambda (name)
      `(select (,@(if multiple? '([multiple "true"]) null)
                [name ,name]
                ,@attrs)
               ,@(for/list ([vn (in-range i)])
                   (define e (hash-ref value->element vn))
                   (define v (number->string vn))
                   `(option ([value ,v]
                             ,@(if (selected? e)
                                   '([selected "true"])
                                   null))
                            ,(display e))))))))

(define (serial-car pr)
  (if (pair? pr) (car pr) #f))

(define (select-input l 
                      #:attributes [attrs null]
                      #:selected? [selected? (λ (x) #f)]
                      #:display [display (λ (x) x)])
  (cross
   (pure serial-car)
   (multiselect-input l
                      #:attributes attrs
                      #:multiple? #f
                      #:selected? selected?
                      #:display display)))

(define (textarea-input
         #:value [value #f]
         #:attributes [attrs null]
         #:rows [rows #f]
         #:cols [cols #f])   
  (make-input
   (lambda (n)
     (list 'textarea
           (list* (list 'name n)
                  (append
                   (filter list?
                           (list (and rows (list 'rows (number->string rows)))
                                 (and cols (list 'cols (number->string cols)))))
                   attrs))
           (if value (coerce-string/utf-8 value) "")))))

; High-level
(define (bytes->string/utf-8* b)
  ;for serialization
  (bytes->string/utf-8 b))
(define (string->number* str)
  ;for serialization
  (string->number str))
(define (string->symbol* str)
  ;for serialization
  (string->symbol str))

(define (required f)
  (cross binding:form-required f))

(define (default d f)
  (cross (binding:form/default d) f))

(define (to-string f)
  (cross (pure bytes->string/utf-8*) f))

(define (to-number f)
  (cross (pure string->number*) f))

(define (to-symbol f)
  (cross (pure string->symbol*) f))

(define (to-boolean f)
  (cross (pure 
          (lambda (b)
            (bytes=? b #"on")))
         f))

; OLD
(define input-string (to-string (required (text-input))))
(define input-int (to-number input-string))
(define input-symbol (to-symbol input-string))


