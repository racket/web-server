(module servlet mzscheme
  ; FIX - split this into stuff servlets need and stuff the server needs
  (provide gen-send/suspend gen-resume-next-request send/suspend send/finish send/back send/forward add-new-instance
           timeout-error TEXT/HTML-MIME-TYPE purge-table)
  (require "channel.ss"
           "internal-structs.ss"
           "servlet-sig.ss"
           "util.ss"
           (lib "xml.ss" "xml"))
  
  ; : (str -> response) -> request
  (define (send/suspend page-maker)
    (let ([s (current-servlet-stuff)])
      ((gen-send/suspend (servlet-stuff-url s)
                         (servlet-stuff-invoke-id s)
                         (servlet-stuff-instances s)
                         (servlet-stuff-output-page s)
                         (servlet-stuff-resume s))
       page-maker)))
  
  ; : response -> doesn't
  (define (send/finish page)
    ((servlet-stuff-output-page (current-servlet-stuff)) page)
    (exit))
  
  ; : response -> doesn't
  (define (send/back page)
    (let* ([s (current-servlet-stuff)]
           [instances (servlet-stuff-instances s)]
           [invoke-id (servlet-stuff-invoke-id s)])
      ((servlet-stuff-output-page s) page)
      ; more here - factor out hash table lookup (check that it won't mess up the timing.)
      (if (hash-table-empty?
           (servlet-instance-cont-table
            (hash-table-get instances invoke-id (lambda () (exit)))))
          (exit)
          ((servlet-stuff-resume s)
           (hash-table-get instances invoke-id (lambda () (exit)))))))
  
  ; : (str -> response) -> request
  (define (send/forward page-maker)
    (let* ([s (current-servlet-stuff)]
           [instances (servlet-stuff-instances s)]
           [invoke-id (servlet-stuff-invoke-id s)])
      (purge-table (servlet-stuff-method s) (servlet-stuff-url s) instances invoke-id 
                   (lambda (inst) (set-servlet-instance-cont-table! inst (make-hash-table)))))
    (send/suspend page-maker))
  
  ; : method Url instance-table sym (servlet -> void) -> void
  (define (purge-table method uri instances invoke-id clear!)
    (let ([inst (hash-table-get instances invoke-id (lambda () #f))])
      (when inst
        (clear! inst)
        (let loop ()
          (channel-get-available
           (servlet-instance-channel inst)
           (lambda (x)
             (timeout-error method uri (car x))
             (loop)))))))
  
  ; more here - refactor gen-send/suspend, but look at development environment first
  
  ; gen-send/suspend : url sym instance-table (response -> void) (instance -> doesn't) -> (str -> response) -> request
  (define (gen-send/suspend uri invoke-id instances output-page resume-next-request)
    (lambda (page-maker)
      (let/cc k
        (let* ([inst (hash-table-get instances invoke-id)]
               [c (servlet-instance-channel inst)]
               [k-count (add1 (servlet-instance-k-counter inst))]
               [cont-table (servlet-instance-cont-table inst)]
               [k-id (format "k~a-~a" k-count (random 2000000000))])
          (set-servlet-instance-k-counter! inst k-count)
          (hash-table-put! cont-table (string->symbol k-id) k)
          (output-page (page-maker (update-params uri (format "~a*~a" invoke-id k-id))))
          (resume-next-request inst)))))
  
  ; :  (-> void) (channel -> void) -> instance -> doesn't
  (define (gen-resume-next-request update-time! update-channel!)
    (lambda (inst)
      (let ([resume (channel-get (servlet-instance-channel inst))])
        ; set! - modeling things that change over time
        (update-time!)
        ; set! justified - communicating between threads
        (update-channel! (car resume))
        ((cadr resume) (caddr resume)))))
  
  ; add-new-instance : sym instance-table -> void
  (define (add-new-instance invoke-id instances)
    (hash-table-put! instances invoke-id
                     (make-servlet-instance 0 (create-channel) (make-hash-table))))
  
  (define TEXT/HTML-MIME-TYPE "text/html")
  
  (define TIME-OUT-CODE 200)
  (define TIME-OUT-HEADERS null)
  
  ;(define TIME-OUT-HEADERS
  ;  `(("Location: " ,(url-path uri))
  ;    ("Retry-After: 5")))
  
  ; timeout-error : Method Url channel -> void
  ; the code must not be an error because some configurations of IE produce
  ; "Internal Error, please see Microsoft's search engine" for 500 responses
  ; instead of displaying the server's error message.
  (define (timeout-error method uri channel)
    (channel-put
     channel
     (make-response/full
      TIME-OUT-CODE "Timeout" (current-seconds) TEXT/HTML-MIME-TYPE TIME-OUT-HEADERS
      (if (eq? method 'head)
          null
          (list (xexpr->string
                 `(html (head (title "Timeout"))
                        (body
                         (p "The transaction referred to by this url is no longer active."
                            "  Please " (a ((href ,(update-params uri #f))) "restart")
                            " the transaction."))))))))))