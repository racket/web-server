(module util mzscheme
  (provide url-path->path prefix? provide-define-struct)
  (require (lib "list.ss"))
    
  ; url-path->path : (Union 'same 'up String) String -> String
  ; more here - ".." should probably raise an error instead of disappearing.
  (define (url-path->path base p)
    ; spidey can't check build-path's use of only certain symbols
    (apply build-path base
           (foldr (lambda (x acc)
                    (cond
                      [(string=? x "") acc]
                      [(string=? x ".") acc]
                      [(string=? x "..") acc] ; ignore ".." (cons 'up acc)]
                      [else (cons x acc)]))
                  null
                  (chop-string #\/ p))))
  
  ; don't use this as it is wrong for the Macintosh
  '(define (url-path->path base p)
     (build-path base (substring p 1 (string-length p))))
  
  ; chop-string : Char String -> (listof String)
  (define (chop-string separator s)
    (let ([p (open-input-string s)])
      (let extract-parts ()
        (cons (list->string
               (let part ()
                 (let ([char (peek-char p)])
                   (cond
                     [(eof-object? char) null]
                     [else (cond
                             [(eq? separator char) null]
                             [else (read-char p) (cons char (part))])]))))
              (cond
                [(eof-object? (read-char p)) null]
                [else (extract-parts)])))))
  
  ; prefix? : str -> str -> bool
  ; more here - consider moving this to mzlib's string.ss
  (define (prefix? prefix)
    (let* ([len (string-length prefix)]
           [last (string-ref prefix (sub1 len))]
           [ascii (char->integer last)])
      (if (= 255 ascii)
          ; something could be done about this - ab255 -> ac
          ; and all 255's eliminates upper range check
          (error 'prefix? "prefix can't end in the largest character")
          (let ([next (string-append (substring prefix 0 (sub1 len))
                                     (string (integer->char (add1 ascii))))])
            (lambda (x)
              (and (string<=? prefix x) (string<? x next)))))))
  
  ; this should go somewhere that other collections can use it too
  (define-syntax provide-define-struct
    (lambda (stx)
      (syntax-case stx ()
        [(_ (struct-name parent-name) (field ...))
         (syntax (begin (define-struct (struct-name parent-name) (field ...))
                        (provide (struct struct-name (field ...)))))]
        [(_ struct-name (field ...))
         (syntax (begin (define-struct struct-name (field ...))
                        (provide (struct struct-name (field ...)))))]))))