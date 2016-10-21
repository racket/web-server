#lang racket/base

(require racket/match
         racket/contract
         (only-in racket/list add-between))

(provide
 (contract-out [parse-content-disposition-header
                (-> bytes?
                    (or/c
                     (list/c 'parsefail string?)
                     (list/c bytes?
                             (listof (list/c bytes? string?)))))]))

(struct parsefail exn ())

;; this file parses the Content-Disposition line of HTTP headers

;; from RFC6266:
#|content-disposition = "Content-Disposition" ":"
                            disposition-type *( ";" disposition-parm )

     disposition-type    = "inline" | "attachment" | disp-ext-type
                         ; case-insensitive
     disp-ext-type       = token

     disposition-parm    = filename-parm | disp-ext-parm

     filename-parm       = "filename" "=" value
                         | "filename*" "=" ext-value

     disp-ext-parm       = token "=" value
                         | ext-token "=" ext-value
     ext-token           = <the characters in token, followed by "*">
|#

;; HOWEVER: we're explicitly giving up on RFC5987-style ext-values. If
;; we see a parm or filename-parm whose token ends with a star, we
;; just give up.

;; SUPER-LIGHTWEIGHT PARSER FRAMEWORK:

;; this is about the most lightweight parser framework that I could
;; come up with:

;; a parser returns a list containing a parsed value and a byte string
;; containing the remainder, or it returns false. Only one parsing is
;; possible with this scheme. Also, this parser does not support
;; backtracking; the kleene star insists on eating the rest of the
;; input. This should ensure that every parser created with this framework
;; is nice and fast.

;; it would be fun to convert this to TR, and I think it would work fine.
;; I don't have time to do it right now... :(

;; given a bunch of parsers, use each of them and combine
;; their parsed values in a list. This is basically just the I/O
;; monad. Or, to be more specific, just the "I" monad. Er, with
;; the exception monad mixed in. Kinda.
(define (seq . parsers)
  (cond [(null? parsers) (λ (bstr) (list '() bstr))]
        [else (λ (bstr)
                (match ((car parsers) bstr)
                  [(list yay leftover)
                   ((postproc (apply seq (cdr parsers))
                              (λ (restyay) (cons yay restyay)))
                    leftover)]
                  [#f #f]))]))

;; given two parsers, use the first one that succeeds. No backtracking.
(define (orparse p1 p2)
  (λ (bstr)
    (match (p1 bstr)
      [(list yay leftover) (list yay leftover)]
      [#f (p2 bstr)])))


;; Kleene star: given a parser, parse until you can't parse any more
(define (kstar parser)
  (λ (bstr)
    (match (parser bstr)
      [(list yay leftover)
       ((postproc (kstar parser)
                  (λ (v) (cons yay v)))
        leftover)]
      [#f (list '() bstr)])))


;; given a regexp (good idea for it to start with ^),
;; and a function to apply to the matched bytes before
;; returning, return a parser for that regexp
(define (rx-matcher regexp postproc)
  (λ (bstr)
    (define maybe-matches (regexp-match-positions regexp bstr))
    (match maybe-matches
      [(list (cons 0 end) other ...)
       (list (postproc (subbytes bstr 0 end))
             (subbytes bstr end))]
      [other #f])))


;; given a regexp (good idea for it to start with ^),
;; return a parser for that regexp
(define (rx-matcher/raw regexp)
  (rx-matcher regexp (λ (x) x)))

;; given a regexp (good idea for it to start with ^),
;; and a constant, return an rx parser that just returns
;; the contstant (if it matches)
(define (rx-matcher/const regexp const)
  (rx-matcher regexp (λ (_) const)))

;; given a parser, return a new parser that strips 'v' from the list
;; in the result position (if the whole parse result is #f, just
;; return it). Doesn't recur into sublists.
(define (strip v p)
  (postproc p (λ (l) (filter (λ (elt) (not (equal? elt v))) l))))

;; apply the given 'pp-fun' to the value in the result position
;; of the parser. If the parser fails, just return the fail
(define (postproc parser pp-fun)
  (λ (bstr)
    (match (parser bstr)
      [(list result leftover)
       (list (pp-fun result) leftover)]
      [#f #f])))

;; try to use parser p. if it fails, pretend it succeeded, and use
;; the given value as the result
(define (opt p val)
  (orparse p (λ (bstr) (list val bstr))))

;; linear white space
;; NB: it looks like the request parser actually cleans up line breaks
;; for us... no problem.
(define LWS (rx-matcher/const #px#"^(\r\n)?[ \t]+" 'LWS))
;; optional linear white space
(define OPTLWS (opt LWS 'LWS))
;; optional leading whitespace
;; IMPL: can't just staple OPTLWS on the front of a seq, because
;; then on #""i n a kstar it gets partway through (viz, the optlws) and
;; then thinks it's failed partway through a seq. Grr.
(define (leadingLWS parser)
  (orparse (postproc (seq LWS parser) cadr) parser))

;; a sequence where linear whitespace is allowed (and discarded)
;; before and between every pair of elements
(define (seq/ws . parsers)
  (strip 'LWS (leadingLWS (apply seq (add-between parsers OPTLWS)))))


(define SEMI (rx-matcher/const #px#"^;" 'SEMI))
(define EQ (rx-matcher/const #px#"^=" 'EQ))
(define DQ (rx-matcher/const #px#"^\"" 'DQ))
(define SQ (rx-matcher/const #px"^'" 'SQ))

;; a quoted string. a quote followed by any character from 32-255 not
;; including backslash or quote, but optionally a backslash followed
;; by any char (can only be 0-127), and finally a close quote.
;; IMPL NOTE: you can do all of this with a single regexp, but you
;; wind up doing all the same work over again in cleaning up the
;; string.
(define CLEANCHARSEQ (rx-matcher/raw #px#"^([ -!#-[]|[\\]-\377])+"))
(define QDESCAPED (rx-matcher #px#"^\\\\[\0-\177]"
                              (λ (v) (list 'escaped v))))
(define QTDSTR
  (postproc (seq DQ (kstar (orparse CLEANCHARSEQ QDESCAPED)) DQ)
            (λ (v) (list 'quoted (cadr v)))))
(define TOKEN (rx-matcher/raw #px#"^([-!#-'*-+.0-9A-Z^-z|~]+)"))
(define VALUE (postproc (orparse TOKEN QTDSTR) (λ (x) (list 'val x))))

(define ISO-8859-1-TOKEN (rx-matcher/const #px"^[iI][sS][oO]-8859-1" 'iso-8559-1))
(define UTF-8-TOKEN (rx-matcher/const #px"^[uU][tT][fF]-8" 'utf-8))
(define LANG-TOKEN (rx-matcher/const #px"^[-a-zA-Z0-9]*" 'LANG-TAG))
(define PCT-ENCODED
  (postproc (rx-matcher/raw #px"^%[0-9a-fA-F][0-9a-fA-F]")
            (λ (x) (list 'pct (string->number
                               (bytes->string/utf-8 (subbytes x 1 3))
                               16)))))
(define ATTR-CHARS (rx-matcher/raw #px"^[-A-Za-z0-9!#$&+.^_`|~]+"))
(define EXT-VALUE-CHARS (kstar (orparse PCT-ENCODED ATTR-CHARS)))
(define EXT-VALUE
  (postproc (seq (orparse ISO-8859-1-TOKEN UTF-8-TOKEN)
                 SQ LANG-TOKEN SQ EXT-VALUE-CHARS)
            (λ (x) (list 'extval x))))



;; give up if we see a token ending with a star; these signal
;; RFC5987 ext-values, and we don't handle them correctly.
(define CLAUSE
  (seq/ws TOKEN EQ (orparse VALUE EXT-VALUE)))

(define content-disposition-parser
  (seq/ws TOKEN (kstar (seq/ws SEMI CLAUSE))))


;; given the right-hand-side of a content-disposition header
;; line, return a list containing the content-disposition-type
;; and a list of token/value lists
(define (parse-content-disposition-header rhs)
  (with-handlers ([parsefail?
                   (lambda (pf)
                     (list 'parsefail (exn-message pf)))])
    (match (content-disposition-parser rhs)
      [(list matched (regexp #px#"^[[:space:]]*$" (list _)))
       (match matched
         [(list ty clauses)
          (list ty (for/list ([c (in-list clauses)])
                     (match c
                       [(list 'SEMI (list tok 'EQ val))
                        (clause-postproc tok val)]
                       [other (error
                               'parse-content-disposition-header
                               "internal error, unexpected parse shape: ~e"
                               c)])))]
         [other
          (error 'parse-content-disposition-header
                 "internal error, unexpected parse shape 2: ~e"
                 other)])]
      [other
       (list 'parsefail
             (format
              (string-append
               "expected: byte string matching RFC6266 spec with "
               "no RFC5987 ext-values, got: ~e")
              rhs))])))

;; clean up a clause by undoing escaping and joining strings
(define (clause-postproc token val)
  (define token-ends-with-star?
    (regexp-match? #px"\\*$" token))
  (define cleaned-val
    (match val
      [(list 'extval v)
       (cond [token-ends-with-star? (extval-cleanup v)]
             [else
              (raise
               (parsefail
                "illegal extended value attached to non-asterisk token: ~e"
                token))])]
      [(list 'val v) (val-cleanup v)]))
  (list token cleaned-val))

;; clean up a quoted string by removing the quotes and undoing escaping
(define (val-cleanup val)
  (match val
    [(? bytes? b) b]
    [(list 'quoted l)
     ;; quoted strings are supposed to be interpreted using
     ;; iso-8859-1, often known as latin-1.
     ;; 
     ;; Here's a frightening passage from RFC2612, concerning the
     ;; definition of TEXT, the stuff in between the quotes:
     #|Words
     of *TEXT MAY contain characters from character sets other than ISO-
     8859-1 [22] only when encoded according to the rules of RFC 2047
     [14].|#
     ;; ... which leaves open the possibility that interpreting these
     ;; strictly as ISO-8859-1 strings may be incorrect. However, given
     ;; the existence of ext-values, I think that no provider would
     ;; use this mechanims. Famous last words. Lemme ask.
     (bytes->string/latin-1
      (apply bytes-append
             (for/list ([chunk (in-list l)])
               (match chunk
                 [(? bytes? b) b]
                 [(list 'escaped eseq)
                  (subbytes eseq 1 2)]))))]))

;; clean up an extval by unescaping pct-encoded strings
(define (extval-cleanup extval)
  (match extval
    [(list encoding _ _ _ pieces)
     (define unencoder
       (match encoding
         ['utf-8      bytes->string/utf-8]
         ['iso-8559-1 bytes->string/latin-1]))
     (define bstrs
       (for/list ([p (in-list pieces)])
         (match p
           [(list 'pct n) (bytes n)]
           [other other])))
     (unencoder (apply bytes-append bstrs))]))

(module+ test
  (require rackunit)

  (check-equal? (QDESCAPED #"\\\003 3")
              (list '(escaped #"\\\003") #" 3"))
(check-equal? ((orparse CLEANCHARSEQ QDESCAPED) #"\\\003 3")
              (list '(escaped #"\\\003") #" 3"))
(check-equal? (QTDSTR #"\"abc\\\003\\\"def\"")
              (list '(quoted (#"abc"
                              (escaped #"\\\003")
                              (escaped #"\\\"")
                              #"def"))
                    #""))

    ;; move down later
  (check-equal? (EXT-VALUE #"UTF-8'en-li-SS'abcd")
                '((extval (utf-8 SQ LANG-TAG SQ (#"abcd"))) #""))
  (check-equal? (EXT-VALUE #"UTF-8'en-li-SS'abcd%20%5c")
                '((extval
                   (utf-8 SQ LANG-TAG SQ (#"abcd" (pct #x20) (pct #x5c))))
                  #""))


  
  (check-equal?
   (parse-content-disposition-header
    #"  form-data  ;name=\"abcz\"; filename=\"abc\\\"d\"")
   '(#"form-data"
     ((#"name" "abcz")
      (#"filename" "abc\"d"))))

  ;; try a high latin-1 character:
  (check-equal?
   (parse-content-disposition-header
    #"  form-data;filename=\"ab\330cd\"")
   '(#"form-data"
     ((#"filename" "abØcd"))))

  (check-equal?
   (parse-content-disposition-header
    #" attachment; filename=\"\\\\foo.html\"\n")
   '(#"attachment"
     ((#"filename" "\\foo.html"))))

(check-equal? (TOKEN #"form-data  ;")
              (list #"form-data" #"  ;"))

(check-equal? ((seq LWS TOKEN) #"   form-data  ;")
              (list (list 'LWS #"form-data") #"  ;"))

(check-equal? ((seq/ws TOKEN) #"   form-data  ;")
              (list (list #"form-data") #"  ;"))

(check-equal? (QTDSTR #"\"abcz\"; filename=\"abc\\\"d\"
 zokbar=abc24")
              (list '(quoted (#"abcz"))
                    #"; filename=\"abc\\\"d\"
 zokbar=abc24"))

(check-equal? (QTDSTR #"\"abc\\\"d\"
 ; zokbar=abc24")
              (list '(quoted (#"abc" (escaped #"\\\"") #"d"))
                    #"
 ; zokbar=abc24"))

(check-equal? (content-disposition-parser
               #"  form-data  ;name=\"abcz\"; filename=\"abc\\\"d\"\r
 ; zokbar=abc24")
              (list `(#"form-data"
                      ((SEMI (#"name" EQ (val (quoted (#"abcz")))))
                       (SEMI (#"filename" EQ (val (quoted (#"abc"
                                                           (escaped #"\\\"")
                                                           #"d")))))
                       (SEMI (#"zokbar" EQ (val #"abc24")))))
                    #""))

  (check-equal? (QTDSTR #"\"filename=\"")
                (list '(quoted (#"filename=")) #""))

  

(check-equal?
 (content-disposition-parser
  #"form-data; name=\"filename=\"; zokbar=\"dingo\"; filename=\"wallaby\"")
 (list `(#"form-data"
         ((SEMI (#"name" EQ (val (quoted (#"filename=")))))
          (SEMI (#"zokbar" EQ (val (quoted (#"dingo")))))
          (SEMI (#"filename" EQ (val (quoted (#"wallaby")))))))
       #""))

  (check-equal?
 (content-disposition-parser
  #"  form-data; name=\"filename=\"; zokbar=\"dingo\"; filename=\"wallaby\"")
 (list `(#"form-data"
         ((SEMI (#"name" EQ (val (quoted (#"filename=")))))
          (SEMI (#"zokbar" EQ (val (quoted (#"dingo")))))
          (SEMI (#"filename" EQ (val (quoted (#"wallaby")))))))
       #""))

  (check-match
   (parse-content-disposition-header
    #"form-data; name=\"filen\"ame=\"; zokbar=\"dingo\"; filename=\"wallaby\"")
   (list 'parsefail (regexp #px"expected: byte string matching RFC6266")))

  (check-equal?
   (parse-content-disposition-header
    #" attachment; filename=\"foo-ae.html\"; filename*=UTF-8''foo-%c3%a4.html\n")
   '(#"attachment" (#"filename" "foo-ae.html")
                  (#"filename*" "foo-ä.html")))

  )
  

;; this code was used to generate the regexp for tokens. In principle,
;; you shouldn't need this code unless you need to re-generate this
;; regexp
(module background racket

  (require rackunit)
  
;; from RFC 2616:
#|token          = 1*<any CHAR except CTLs or separators>
       separators     = "(" | ")" | "<" | ">" | "@"
                      | "," | ";" | ":" | "\" | <">
                      | "/" | "[" | "]" | "?" | "="
                      | "{" | "}" | SP | HT
|#

(define separators
  (map (λ (s) (first (string->list s)))
       '("(" ")" "<" ">" "@"
             "," ";" ":" "\\" "\""
             "/" "[" "]" "?" "="
             "{" "}" " " "\t")))

(define CTLs
  (cons #\u007f
  (for/list ([i (in-range 0 32)])
    (integer->char i))))

;; add hyphen because it has to be treated
;; specially in regexps:
(define separators-plus-ctls-plus-hyphen
  (cons #\- (append separators CTLs)))

(define omitted-integers
  (remove-duplicates
   (sort (map char->integer separators-plus-ctls-plus-hyphen) <)))

(define ranges
  (let loop ([range-begin 0]
             [badchars omitted-integers])
    (cond [(null? badchars)
           (cond [(< range-begin 127)
                  (list (list range-begin 126))]
                 [else (list)])]
          [else
           (define nextbad (first badchars))
           (cond [(< range-begin nextbad)
                  (cons (list range-begin (sub1 nextbad))
                        (loop (add1 nextbad) (rest badchars)))]
                 [(= range-begin nextbad)
                  (loop (add1 range-begin)
                        (rest badchars))]
                 [else
                  (error 'impossible-i-thought
                         "~a ~a" range-begin nextbad)])])))

(define token-regexp-bstr
  (string->bytes/utf-8
  (call-with-output-string
   (λ (port)
     ;; adding the hyphen back in here:
     (fprintf port "[-")
     (for/list ([r (in-list ranges)])
       (cond [(equal? (first r) (second r))
              (fprintf port "~a" (string (integer->char (first r))))]
             [else
              (fprintf port "~a~a~a" (string (integer->char (first r))) "-"
                       (string (integer->char (second r))))]))
     (fprintf port "]")))))

;; check that it works:

(for ([i (in-range 0 128)])
  (define ch (integer->char i))
  (cond [(member ch (append separators CTLs))
         (check-pred (λ (ch) (not (regexp-match? token-regexp-bstr
                                                 (string ch))))
                     ch)]
        [else
         (check-pred (λ (ch) (regexp-match? token-regexp-bstr (string ch)))
                     ch)]))


  #|attr-char     = ALPHA / DIGIT
                   / "!" / "#" / "$" / "&" / "+" / "-" / "."
                   / "^" / "_" / "`" / "|" / "~"
                   ; token except ( "*" / "'" / "%" )
|#

  )


  

