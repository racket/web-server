(module servlet-helpers mzscheme
  (require (lib "list.ss")
           (lib "etc.ss")
           "web-server.ss")
  (provide extract-binding/single extract-bindings extract-user-pass build-suspender)
  
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
  
  ; build-suspender : (listof html) (listof html) [(listof (cons sym str))] [(listof (cons sym str))] -> str -> html
  (define build-suspender
    (opt-lambda (title content [body-attributes '([bgcolor "white"])] [head-attributes null])
      (lambda (k-url)
        `(html (head ,head-attributes (title . ,title))
               (body ,body-attributes
                     (form ([action ,k-url] [method "post"])
                           . ,content)))))))
