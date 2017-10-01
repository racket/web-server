#lang web-server/base

(require web-server/formlets
         web-server/formlets/lib
         web-server/test
         web-server/servlet-dispatch
         web-server/managers/none
         web-server/http
         xml/path
         racket/serialize
         rackunit)

(provide stateless-test-suite)

(define-syntax-rule (formlet-serial-thunk expr)
  (λ () (serialize expr)))

(define-syntax check-formlet-serializes 
  (syntax-rules ()
    [(_ expr) (check-not-exn (formlet-serial-thunk expr))]
    [(_ expr msg) (check-not-exn (formlet-serial-thunk expr) msg)]))

(define (serial+ . args)
  (apply + args))

(define serialization-test-suite
  (test-suite
   "serialization"
   (test-case
    "lib"

    (test-case "pure"
               (check-formlet-serializes (pure 5))
               (check-formlet-serializes (pure serial+)))

    (check-formlet-serializes (cross (pure serial+)
                                     (λ (i)
                                       (values '()
                                               (λ (env) (values 1 2))
                                               i)))
                              "cross")

    (check-formlet-serializes (cross* (pure serial+)
                                      (pure 1)
                                      (pure 2))
                              "cross*")

    (check-formlet-serializes (xml-forest '())
                              "xml-forest")

    (check-formlet-serializes (xml `(p "This is an xexpr"))
                              "xml")

    (check-formlet-serializes (text "This is an xexpr")
                              "text")

    (check-formlet-serializes (tag-xexpr 'div null (xml `(p "This is an xexpr")))))

   (test-case
    "input"
    (test-case
     "low-level"
     (check-formlet-serializes (make-input* (λ (n) `(input ([type "text"][name ,n]))))
                               "make-input*")
     (check-formlet-serializes (make-input (λ (n) `(input ([type "text"][name ,n]))))
                               "make-input"))
    (test-case
     "HTML Spec"
     (check-formlet-serializes (input) "input")
     (check-formlet-serializes (text-input) "text-input")
     (check-formlet-serializes (password-input) "password-input")
     (check-formlet-serializes (checkbox "some-value" #t) "checkbox")
     (check-formlet-serializes (radio "another-value" #f) "radio")
     (check-formlet-serializes (radio-group '("a" "b" "c")) "radio-group")
     (check-formlet-serializes (checkbox-group '("a" "b" "c")) "checkbox-group")
     (check-formlet-serializes (submit "Submit") "submit")
     (check-formlet-serializes (reset "Reset") "reset")
     (check-formlet-serializes (file-upload) "file-upload")
     (check-formlet-serializes (hidden "hidden value") "hidden")
     (check-formlet-serializes (img "An image" "http://example.com/foo.png") "img")
     (check-formlet-serializes (button "button-type" "Button Text") "button")
     (check-formlet-serializes (multiselect-input '("a" "b" "c")) "multiselect-input")
     (check-formlet-serializes (select-input '("a" "b" "c")) "select-input")
     (check-formlet-serializes (textarea-input) "textarea-input"))
    (test-case
     "High-level"
     (check-formlet-serializes (required (text-input)) "required")
     (check-formlet-serializes (default #"the default" (text-input)) "default")
     (check-formlet-serializes (to-string (required (text-input))) "to-string")
     (check-formlet-serializes (to-number (to-string (required (text-input)))) "to-number")
     (check-formlet-serializes (to-symbol (to-string (required (text-input)))) "to-symbol")
     (check-formlet-serializes (to-boolean (required (text-input))) "to-boolean"))
    (test-case
     "OLD"
     (check-formlet-serializes input-string "input-string")
     (check-formlet-serializes input-int "input-int")
     (check-formlet-serializes input-symbol "input-symbol")))

   (check-formlet-serializes (formlet (#%# (label "Dividend" ,[=> input-int dividend])
                                           (br)
                                           (label "Divisor" ,[=> input-int divisor]))
                                      (quotient dividend divisor))
                             "formlet (syntax)")

   (check-formlet-serializes (formlet* `(div ,@(for/list ([i (in-range 1 10)])
                                                 `(p ,(number->string i)
                                                     ,((text-input) . =>* . first-names)
                                                     ,((text-input) . =>* . last-names))))
                                       (for/list ([first (in-list first-names)]
                                                  [last (in-list last-names)])
                                         (cons first last)))
                             "formlet* (dyn-syntax)")))

(define (make-servlet-tester/stateless start)
  (make-dispatcher-tester
   (dispatch/servlet start #:stateless? #t #:manager (create-none-manager #f))))

(module servlet-with-contracts web-server/lang/base
  (require web-server/formlets/stateless
           web-server/lang/web
           web-server/http)
  (provide start-with-contracts)
  (define (start-with-contracts request)
    (define base
      (send/formlet (to-number (to-string (required (text-input))))))
    (send/suspend/dispatch
     (λ (embed/url)
       (response/xexpr
        `(html (head (title "Page 2"))
               (body
                ,(embed-formlet
                  embed/url
                  (formlet (p "Add what to " ,(number->string base) "?"
                              (br)
                              ,[=> (to-number (to-string (required (text-input))))
                                   new-num])
                           (response/xexpr
                            `(html (head (title "Page 3"))
                                   (body (p ,(number->string (+ base new-num))))))))
                (br)
                (a ([href ,(embed/url start-with-contracts)]) "Restart"))))))))

(require 'servlet-with-contracts)

(define servlet-with-contracts-test-suite
  (test-suite
   "servlet with contracts"
   (test-begin
    (define tester:with-contracts
      (make-servlet-tester/stateless start-with-contracts))

    (define initial-page
      (tester:with-contracts))

    (check-not-exn (λ () (se-path* '(form #:action) initial-page))
                   "initial-page: should have form with action")

    (check-not-exn (λ () (se-path* '(input #:name) initial-page))
                   "initial-page: should have input with name")

    (define page2
      (tester:with-contracts (se-path* '(form #:action) initial-page)
                             (list (binding:form (string->bytes/utf-8
                                                  (se-path* '(input #:name) initial-page))
                                                 #"1"))))

    (check-not-exn (λ () (se-path* '(form #:action) page2))
                   "page2: should have form with action")

    (check-not-exn (λ () (se-path* '(input #:name) page2))
                   "page2: should have input with name")

    (define page3
      (tester:with-contracts (se-path* '(form #:action) page2)
                             (list (binding:form (string->bytes/utf-8
                                                  (se-path* '(input #:name) page2))
                                                 #"2"))))

    (check-equal? (se-path*/list '(p) page3)
                  '("3")
                  "Result should be 3"))))

(module unsafe-servlet web-server/lang/base
  (require web-server/formlets/unsafe
           web-server/lang/web
           web-server/http)
  (provide start-unsafe-servlet)
  (define (start-unsafe-servlet request)
    (send/formlet
     (formlet
      (#%# ,[=> (to-string (required (text-input)))
                given-string])
      (send/suspend/dispatch
       (λ (embed/url)
         (response/xexpr
          `(html
            (head (title "Page 2"))
            (body (p ,given-string)
                  (div
                   (a ([id "b"]
                       [href ,(embed/url (λ (req)
                                           (response/xexpr
                                            `(html (head (title "Bold"))
                                                   (body (b ,given-string))))))])
                      "Bold")
                   (a ([id "i"]
                       [href ,(embed/url (λ (req)
                                           (response/xexpr
                                            `(html (head (title "Italic"))
                                                   (body (i ,given-string))))))])
                      "Italic")))))))))))

(require 'unsafe-servlet)

(define unsafe-servlet-test-suite
  (test-suite
   "unsafe servlet"
   (test-begin
    (define tester:unsafe-servlet
      (make-servlet-tester/stateless start-unsafe-servlet))

    (define initial-page
      (tester:unsafe-servlet))

    (check-not-exn (λ () (se-path* '(form #:action) initial-page))
                   "initial-page: should have form with action")

    (check-not-exn (λ () (se-path* '(input #:name) initial-page))
                   "initial-page: should have input with name")

    (define (test-interaction str)
      (define page2
        (tester:unsafe-servlet (se-path* '(form #:action) initial-page)
                               (list (binding:form (string->bytes/utf-8
                                                    (se-path* '(input #:name) initial-page))
                                                   (string->bytes/utf-8 str)))))
      (define rslt
        (list str))
      (check-equal? (se-path*/list '(p) page2)
                    rslt
                    "get input back")
      (check-match (se-path*/list '(div) page2)
                   (list-no-order
                    (list-rest 'a
                               (list-no-order `(id "b")
                                              (list 'href bold-href))
                               _)
                    (list-rest 'a
                               (list-no-order `(id "i")
                                              (list 'href italic-href))
                               _))
                   (and (equal? (se-path*/list '(b) (tester:unsafe-servlet bold-href))
                                rslt)
                        (equal? (se-path*/list '(i) (tester:unsafe-servlet italic-href))
                                rslt))))
    (test-interaction "apple")
    (test-interaction "cat"))))

(define stateless-test-suite
  (test-suite "stateless formlets"
              serialization-test-suite
              servlet-with-contracts-test-suite
              unsafe-servlet-test-suite))