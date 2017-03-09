#lang racket/base

(require net/cookies/common
         net/cookies/server
         web-server/http/request-structs
         racket/contract
         racket/match
         racket/date
         )

(provide (contract-out
          [cookie->header (-> cookie?
                              header?)]
          [rename make-cookie* make-cookie
                  (->* (cookie-name?
                        cookie-value?)
                       (#:comment any/c
                        #:domain (or/c domain-value? #f)
                        #:max-age (or/c (and/c integer? positive?) #f)
                        #:path (or/c path/extension-value? #f)
                        #:expires (or/c date? string? #f)
                        #:secure? any/c
                        #:http-only? any/c
                        #:extension (or/c path/extension-value? #f))
                       cookie?)]
          ))

;; cookie->header : cookie -> header
;; gets the header that will set the given cookie
(define (cookie->header cookie)
  (header #"Set-Cookie" (cookie->set-cookie-header cookie)))

(define exp-date-pregexp
  (pregexp (string-append "(\\d\\d)\\s+";day
                          "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\s+";month
                          "(\\d\\d\\d\\d)\\s+";year
                          "(\\d\\d):(\\d\\d):(\\d\\d)" ;hr:min:sec
                          )))

(define (make-cookie* name
                      value
                      #:comment [_ #f]
                      #:domain [domain #f]
                      #:max-age [max-age #f]
                      #:path [path #f]
                      #:expires [exp-date/raw #f]
                      #:secure? [secure? #f]
                      #:http-only? [http-only? #f] 	 	 	 
                      #:extension [extension #f])
  (make-cookie name
               value
               #:domain domain
               #:max-age max-age
               #:path path
               #:secure? (not (not secure?))
               #:http-only? (not (not http-only?))
               #:extension extension
               #:expires (cond [(string? exp-date/raw)
                                (match exp-date/raw
                                  [(pregexp exp-date-pregexp
                                            (list _
                                                  (app string->number day)
                                                  month-str
                                                  (app string->number year)
                                                  (app string->number hour)
                                                  (app string->number min)
                                                  (app string->number sec)))
                                   (with-handlers ([exn:fail? (λ (e) (failure-cont))])
                                     (seconds->date
                                      (find-seconds sec min hour day
                                                    (case month-str
                                                      [("Jan") 1]
                                                      [("Feb") 2]
                                                      [("Mar") 3]
                                                      [("Apr") 4]
                                                      [("May") 5]
                                                      [("Jun") 6]
                                                      [("Jul") 7]
                                                      [("Aug") 8]
                                                      [("Sep") 9]
                                                      [("Oct") 10]
                                                      [("Nov") 11]
                                                      [("Dec") 12])
                                                    year
                                                    #f)
                                      #f))]
                                  [_ (raise-arguments-error
                                      'make-cookie*
                                      "invalid #:expires string"
                                      'expected
                                      "#f, a date?, or a string conforming to RFC 7231 Section 7.1.1.2"
                                      'given exp-date/raw)])]
                               [else exp-date/raw])))




