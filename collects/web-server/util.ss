(module util mzscheme
  (require (lib "contract.ss")
           (lib "list.ss")
           (lib "url.ss" "net"))
  
  (provide get-mime-type
           update-params 
           provide-define-struct
           extract-flag
           hash-table-empty?)
  
  (provide/contract 
   [path->list  (path? . -> . (cons/p (union path? (symbols 'up 'same))
                                      (listof (union path? (symbols 'up 'same)))))]
   [url-path->path ((union (symbols 'up 'same) path?) string? . -> . path?)]
   [directory-part (path? . -> . path?)])


  ; prefix? : str -> str -> bool
  ; more here - consider moving this to mzlib's string.ss
  ;; Notes: (GregP)
  ;; 1. What's the significance of char # 255 ???
  ;; 2. 255 isn't an ascii character. ascii is 7-bit
  ;; 3. OK fuck this. It is only used in three places, some of them
  ;;    will involve bytes while the others may involve strings. So
  ;;    I will just use regular expressions and get on with life.
  (define (prefix?-old prefix)
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
  


  ;; get-mime-type: path -> bytes
  ;; determine the mime type based on the filename's suffix
  ;;
  ;; Notes (GregP):
  ;; 1. Can we determine the mime type based on file contents?
  ;; 2. Assuming that 7-bit ASCII is correct for mime-type
  (define get-mime-type
    (let ([file-suffix-regexp (byte-regexp #".*\\.([^\\.]*$)")])
      (lambda (path)
        (let ([sffx (cadr (regexp-match file-suffix-regexp (path->bytes path)))])
          (hash-table-get MIME-TYPE-TABLE
                          (string->symbol (bytes->string/utf-8 sffx))
                          (lambda () DEFAULT-MIME-TYPE))))))
    
  
  (define DEFAULT-MIME-TYPE #"text/plain")
  
  (define MIME-TYPE-TABLE
    (let ([table (make-hash-table)])
      (for-each (lambda (x) (hash-table-put! table (car x) (cdr x)))
                '((htm  . #"text/html")
                  (html . #"text/html")
                  (css  . #"text/css")
                  (txt  . #"text/plain")
                  (hqx  . #"application/mac-binhex40")
                  (doc  . #"application/msword")
                  (plt  . #"application/octet-stream")
                  (w02  . #"application/octet-stream")
                  (w03  . #"application/octet-stream")
                  (exe  . #"application/octet-stream")
                  (bin  . #"application/octet-stream")
                  (pdf  . #"application/pdf")
                  (ps   . #"application/postscript")
                  (rtf  . #"application/rtf")
                  (dvi  . #"application/x-dvi")
                  (tar  . #"application/x-tar")
                  (tex  . #"application/x-tex")
                  (zip  . #"application/zip")
                  (xls  . #"application/msexcel")
                  (ppt  . #"application/powerpoint")
                  (pot  . #"application/powerpoint")
                  (ppf  . #"application/persuasion")
                  (fm   . #"application/filemaker")
                  (pm6  . #"application/pagemaker")
                  (psd  . #"application/x-photoshop")
                  (pdd  . #"application/x-photoshop")
                  (ram  . #"audio/x-pn-realaudio")
                  (ra   . #"audio/x-realaudio")
                  (swf  . #"application/x-shockwave-flash")
                  (aif  . #"audio/aiff")
                  (au   . #"audio/basic")
                  (voc  . #"audio/voice")
                  (wav  . #"audio/wave")
                  (mov  . #"video/quicktime")
                  (mpg  . #"video/mpeg")
                  (png  . #"image/png")
                  (bmp  . #"image/bmp")
                  (gif  . #"image/gif")
                  (jpg  . #"image/jpeg")
                  (tif  . #"image/tiff")
                  (pic  . #"image/x-pict")))
      table))
  
  (define (directory-part path)
    (let-values ([(base name must-be-dir) (split-path path)])
      (cond
        [(eq? 'relative base) (current-directory)]
        [(not base) (error 'directory-part "~a is a top-level directory" path)]
        [(path? base) base])))
  
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
  
  ; update-params : Url (U #f String) -> String
  ; to create a new url just like the old one, but with a different parameter part
  (define (update-params uri params)
    (url->string
     (make-url (url-scheme uri) (url-host uri) (url-port uri) 
               (url-path uri) params (url-query uri) 
               (url-fragment uri))))

  ; to convert a platform dependent path into a listof path parts such that
  ; (forall x (equal? (path->list x) (path->list (apply build-path (path->list x)))))
  (define (path->list p)
    (let loop ([p p] [acc null])
      (let-values ([(base name must-be-dir?) (split-path p)])
        (let ([new-acc (cons name acc)])
          (cond
            [(string? base) (loop base new-acc)]
            [else ; conflate 'relative and #f
             new-acc])))))
  
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
  
  
  ; this should go somewhere that other collections can use it too
  (define-syntax provide-define-struct
    (lambda (stx)
      (syntax-case stx ()
        [(_ (struct-name parent-name) (field ...))
         (syntax (begin (define-struct (struct-name parent-name) (field ...))
                        (provide (struct struct-name (field ...)))))]
        [(_ struct-name (field ...))
         (syntax (begin (define-struct struct-name (field ...))
                        (provide (struct struct-name (field ...)))))])))
  
  ; this is used by launchers
  ; extract-flag : sym (listof (cons sym alpha)) alpha -> alpha
  (define (extract-flag name flags default)
    (let ([x (assq name flags)])
      (if x
          (cdr x)
          default)))
  
  ; hash-table-empty? : hash-table -> bool
  (define (hash-table-empty? table)
    (let/ec out
      (hash-table-for-each table (lambda (k v) (out #f)))
      #t))
  )
