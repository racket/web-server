(module servlet mzscheme
  (provide send/suspend send/finish send/back send/forward extract-user-pass)
  (require "servlet-tables.ss"
	   "util.ss"
	   "internal-structs.ss"
	   (lib "base64.ss" "net"))
  
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

  ; Authentication



  (define AUTHENTICATION-REGEXP (regexp "([^:]*):(.*)"))
  (define (match-authentication x) (regexp-match AUTHENTICATION-REGEXP x))
  ;:(define match-authentication (type: (str -> (union false (list str str str)))))

  ; extract-user-pass : (listof (cons sym str)) -> (U #f (cons str str))
  (define (extract-user-pass headers)
    (let ([pass-pair (assq 'authorization headers)])
      (and pass-pair
	   (let ([basic-credentials (cdr pass-pair)])
	     (cond
	      [(and (basic? basic-credentials)
		    (match-authentication (base64-decode (substring basic-credentials 6 (string-length basic-credentials)))))
	       => (lambda (user-pass)
		    (cons (cadr user-pass) (caddr user-pass)))]
	      [else #f])))))

  (define basic? (prefix? "Basic ")))