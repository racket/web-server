(module servlet-helpers mzscheme
  (require (lib "list.ss")
           (lib "etc.ss")
           "web-server.ss"
           (lib "servlet-sig.ss" "web-server")
           (lib "xml.ss" "xml"))
  
  (provide extract-binding/single
           extract-bindings
	   exists-binding?
	   extract-user-pass
	   build-suspender
	   make-html-response/incremental
           ;anchor-case
	   redirect-to
           permanently
           temporarily
           see-other)
  
  ; extract-binding/single : sym (listof (cons sym str)) -> str
  (define (extract-binding/single name bindings)
    (let ([lst (extract-bindings name bindings)])
      (cond
        [(null? lst)
         (error 'extract-bindings/single "~a not found in ~a" name bindings)]
        [(null? (cdr lst)) (car lst)]
        [else (error 'extract-bindings/single "~a occurs multiple times in ~a" name bindings)])))
  
  ; extract-bindings : sym (listof (cons sym str)) -> (listof str)
  (define (extract-bindings name bindings)
    (map cdr (filter (lambda (x) (eq? name (car x))) bindings)))
  
  ; exists-binding? : sym (listof (cons sym str)) -> bool
  ; for checkboxes
  (define (exists-binding? name bindings)
    (if (assq name bindings)
        #t
        #f))
  
  ; build-suspender : (listof html) (listof html) [(listof (cons sym str))] [(listof (cons sym str))] -> str -> html
  (define build-suspender
    (opt-lambda (title content [body-attributes '([bgcolor "white"])] [head-attributes null])
      (lambda (k-url)
        `(html (head ,head-attributes
                     (meta ([http-equiv "Pragma"] [content "no-cache"])) ; don't cache in netscape
                     (meta ([http-equiv "expires"] [content "-1"])) ; don't cache in IE
                     ; one site said to use -1, another said to use 0.
                     (title . ,title))
               (body ,body-attributes
                     (form ([action ,k-url] [method "post"])
                           . ,content))))))
  
  ; redirection-status = (make-redirection-status nat str)
  (define-struct redirection-status (code message))
  
  (define permanently (make-redirection-status 301 "Moved Permanently"))
  (define temporarily (make-redirection-status 302 "Moved Temporarily"))
  (define see-other (make-redirection-status 303 "See Other"))
  
  ; : str [redirection-status] -> response
  (define redirect-to
    (opt-lambda (uri [perm/temp permanently])
      (make-response/full (redirection-status-code perm/temp)
                          (redirection-status-message perm/temp)
                          (current-seconds) "text/html"
                          `((location . ,uri)) (list (redirect-page uri)))))
  
  ; : str -> str
  (define (redirect-page url)
    (xexpr->string `(html (head "Redirect to " ,url)
                          (body (p "Redirecting to " (a ([href ,url]) ,url))))))
  
  ; make-html-response/incremental : ((string -> void) -> void) -> response/incremental
  (define (make-html-response/incremental chunk-maker)
    (make-response/incremental
     200 "Okay" (current-seconds) "text/html" '()
     chunk-maker))
  
  (define-syntax anchor-case
    (lambda (stx)
      (syntax-case stx ()
        [(src-anchor-case
          page
          ((anchor-pattern anchor-patterns ...) body bodies ...) ...)
         ; FIX - catching send/suspend from the unit is messy.
         (with-syntax ([send/suspend (syntax (eval 'send/suspend))])
           (syntax (let ([format-href (lambda (k-url id) (format "~a?link=~a" k-url id))])
                     ; Format-href should check that gensym only generates okay characters, but gensym always does.
                     (let ([request
                            (send/suspend
                             (lambda (k-url)
                               ; FIX provide k-url somehow for forms?
                               ;     add an else clause for form submission
                               (let-values ([(anchor-pattern anchor-patterns ...)
                                             (values (format-href k-url 'anchor-pattern)
                                                     (format-href k-url 'anchor-patterns) ...)]
                                            ...)
                                 page)))])
                       (let ([link (string->symbol (extract-binding/single 'link (request-bindings request)))])
                         (case link
                           [(anchor-pattern anchor-patterns ...)
                            body bodies ...]
                           ...
                           [else (error 'src-anchor-case "unmatched response ~s" link)]))))))]))))
