(module servlet mzscheme
  (provide send/suspend send/finish send/back send/forward)
  (require "servlet-tables.ss"
	   "util.ss"
	   "internal-structs.ss")
  
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
    (send/suspend page-maker)))