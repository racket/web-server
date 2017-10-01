#lang racket/base

(require racket/contract
         "unsafe/input.rkt"
         web-server/http
         web-server/private/xexpr
         (submod "lib.rkt" private)
         (only-in "lib.rkt"
                  xexpr-forest/c
                  formlet/c
                  pure
                  cross))

(provide (contract-out ; Low-level
          [make-input* (-> (-> string? pretty-xexpr/c)
                           (serial-formlet/c (listof binding?)))]
          [make-input (-> (-> string? pretty-xexpr/c)
                          (serial-formlet/c (or/c false/c binding?)))]
          ; HTML Spec
          [input (->* ()
                      (#:type string?
                       #:value (or/c false/c bytes? string?)
                       #:max-length (or/c false/c exact-nonnegative-integer?)
                       #:read-only? boolean?
                       #:attributes (listof (list/c symbol? string?)))
                      (serial-formlet/c (or/c false/c binding?)))]
          [text-input (->* () 
                           (#:value (or/c false/c bytes? string?)
                            #:size (or/c false/c exact-nonnegative-integer?)
                            #:max-length (or/c false/c exact-nonnegative-integer?)
                            #:read-only? boolean?
                            #:attributes (listof (list/c symbol? string?)))
                           (serial-formlet/c (or/c false/c binding?)))]
          [password-input (->* () 
                               (#:value (or/c false/c bytes? string?)
                                #:size (or/c false/c exact-nonnegative-integer?)
                                #:max-length (or/c false/c exact-nonnegative-integer?)
                                #:read-only? boolean?
                                #:attributes (listof (list/c symbol? string?)))
                               (serial-formlet/c (or/c false/c binding?)))]
          [checkbox (->* ((or/c bytes? string?) boolean?)
                         (#:attributes (listof (list/c symbol? string?)))
                         (serial-formlet/c (or/c false/c binding?)))]
          [radio (->* ((or/c bytes? string?) boolean?)
                      (#:attributes (listof (list/c symbol? string?)))
                      (serial-formlet/c (or/c false/c binding?)))]
          [radio-group (->* (sequence?)
                            (#:attributes 
                             (-> any/c (listof (list/c symbol? string?)))
                             #:checked? (any/c . -> . boolean?)
                             #:display (any/c . -> . pretty-xexpr/c)
                             #:wrap (any/c any/c . -> . xexpr-forest/c))
                            (serial-formlet/c any/c))]
          [checkbox-group (->* (sequence?)
                               (#:attributes 
                                (-> any/c (listof (list/c symbol? string?)))
                                #:checked? (any/c . -> . boolean?)
                                #:display (any/c . -> . pretty-xexpr/c))
                               (serial-formlet/c (listof any/c)))]
          [submit (->* ((or/c bytes? string?))
                       (#:attributes (listof (list/c symbol? string?)))
                       (serial-formlet/c (or/c false/c binding?)))]
          [reset (->* ((or/c bytes? string?))
                      (#:attributes (listof (list/c symbol? string?)))
                      (serial-formlet/c (or/c false/c binding?)))]
          [file-upload (->* ()
                            (#:attributes (listof (list/c symbol? string?)))
                            (serial-formlet/c (or/c false/c binding?)))]
          [hidden (->* ((or/c bytes? string?))
                       (#:attributes (listof (list/c symbol? string?)))
                       (serial-formlet/c (or/c false/c binding?)))]
          [img (->* ((or/c bytes? string?) (or/c bytes? string?))
                    (#:height (or/c false/c exact-nonnegative-integer?)
                     #:longdesc (or/c false/c (or/c bytes? string?))
                     #:usemap (or/c false/c (or/c bytes? string?))
                     #:width (or/c false/c exact-nonnegative-integer?)
                     #:attributes (listof (list/c symbol? string?)))
                    (serial-formlet/c (or/c false/c binding?)))]
          [button (->* ((or/c bytes? string?) (or/c bytes? string?))
                       (#:disabled
                        boolean?
                        #:value (or/c false/c (or/c bytes? string?))
                        #:attributes (listof (list/c symbol? string?)))
                       (serial-formlet/c (or/c false/c binding?)))]
          [multiselect-input (->* (sequence?)
                                  (#:attributes 
                                   (listof (list/c symbol? string?))
                                   #:multiple? boolean?
                                   #:selected? (any/c . -> . boolean?)
                                   #:display (any/c . -> . pretty-xexpr/c))
                                  (serial-formlet/c (listof any/c)))]
          [select-input (->* (sequence?)
                             (#:attributes 
                              (listof (list/c symbol? string?))
                              #:selected? (any/c . -> . boolean?)
                              #:display (any/c . -> . pretty-xexpr/c))
                             (serial-formlet/c any/c))]
          [textarea-input (->* ()
                               (#:attributes 
                                (listof (list/c symbol? string?))
                                #:value (or/c false/c (or/c bytes? string?))
                                #:rows number?
                                #:cols number?)
                               (serial-formlet/c (or/c false/c binding?)))]
          ; High-level
          [required (-> (formlet/c (or/c false/c binding?))
                        (serial-formlet/c bytes?))]
          [default (-> bytes? (formlet/c (or/c false/c binding?))
                       (serial-formlet/c bytes?))]
          [to-string (-> (formlet/c bytes?)
                         (serial-formlet/c string?))]
          [to-number (-> (formlet/c string?)
                         (serial-formlet/c number?))]
          [to-symbol (-> (formlet/c string?)
                         (serial-formlet/c symbol?))]
          [to-boolean (-> (formlet/c bytes?)
                          (serial-formlet/c boolean?))]
          ;OLD
          [input-string (serial-formlet/c string?)]
          [input-int (serial-formlet/c number?)]
          [input-symbol (serial-formlet/c symbol?)]))
