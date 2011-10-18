#lang racket

(require "shared.rkt"
         (planet dherman/c:4)
         rackunit)

;; this file contains the definitions of the evaluators for C, most notably
;; the "c-parser-match" evaluator, that tries to guide the student toward 
;; producing code that parses to the same result as the instructor's code.

;; FIXME:
;; - line breaks in user code
;; - extend to handle stmts too
;; - this parser is probably sensitive to the order of, e.g., "const" & "volatile"


(provide any-c-int
         any-c-addition
         c-parser-match
         c-stmt-parser-match)

;; this structure is raised to allow an exception handler to exit with a response
(define-struct abort-with (response))

;; this has to be early:
;; a simple memoizer for non-recursive, one-arg functions:
(define (memoize-one fun)
  (define ht (make-hash))
  (lambda (arg)
    (hash-ref ht arg
              (lambda ()
                (define result (fun arg))
                (hash-set! ht arg result)
                result))))




;; abstract over patterns before trying to do a better job...

;; take the text supplied by the user,
;; check that it parses to an int
(define (any-c-int usertext)
  (with-handlers ([abort-with? abort-with-response])
  (catch-reader-parser-errors
   usertext
   (lambda ()
     (cond [(equal? usertext "") (failure empty-input-msg)]
           [(parses-as-int? usertext) (success)]
           [else (failure (format "~v doesn't parse as an integer"
                                  usertext))])))))

(define (parses-as-int? str)
  (match (parse-expression str)
    [(struct expr:int (src value qualifiers)) #t]
    [other #f]))


;; abstraction needed here...
(define (any-c-addition usertext)
  (with-handlers ([abort-with? abort-with-response])
  (catch-reader-parser-errors usertext
   (lambda ()
     (cond [(equal? usertext "") (failure empty-input-msg)]
           [(parses-as-addition? usertext) (success)]
           [else (failure 
                  (format "~v doesn't parse as the sum of two integers"
                          usertext))])))))

;; does this string parse as (+ (int) (int)) ?
;; what to do on a parse error?
(define (parses-as-addition? str)
  (match (parse-expression str)
    [(struct expr:binop (src_1 (struct expr:int (dc_1 dc_2 dc_3)) 
                               (struct id:op (src2 '+))
                               (struct expr:int (dc_4 dc_5 dc_6)))) 
     #t]
    [other #f]))


;; does the given text match the pattern?
(define (c-parser-match pattern usertext)
  (with-handlers ([abort-with? abort-with-response])
    ;; patterns are just going to be strings, for now.
    ((exp-pattern->matcher pattern) usertext)))

;; does the given statement match the pattern statement?
(define (c-stmt-parser-match pattern usertext)
  (with-handlers ([abort-with? abort-with-response])
    ((stmt-pattern->matcher pattern) usertext)))


(define exp-pattern->matcher
  (memoize-one (lambda (pat) (pattern->matcher parse-expression pat))))

(define stmt-pattern->matcher
  (memoize-one (lambda (pat) (pattern->matcher parse-statement pat))))

;; for now, a "pattern" is just a string containing a parsable C expression
(define (pattern->matcher parser pat)
  (catch-reader-parser-pattern-errors 
   pat
   (lambda ()
     (let ([parsed-pattern (parser pat)])
       (lambda (usertext)
         (catch-reader-parser-errors usertext
                                     (lambda ()
                                       (compare-parsed parsed-pattern usertext parser))))))))



;; catch exn:fail:read and exn:fail:syntax, turn them into failures
(define (catch-reader-parser-errors usertext thunk)
  (with-handlers ([exn:fail:read? 
                   (lambda (exn) 
                     (match (exn:fail:read-srclocs exn)
                       [`() (log-warning "internal error, no source position given (2011-04)")
                            (failure `(div (p ,(exn-message exn))))]
                       [(? list? l)
                        (when (not (= (length l) 1))
                          (log-warning 
                           "more than one source position given 20110411"))
                        ;; we hope there's only one...
                        (define srcloc (first l))
                        (posn-span->fail (srcloc-position srcloc)
                                         (srcloc-span srcloc)
                                         usertext
                                         (exn-message exn))]))]
                  [exn:fail:syntax? 
                   (lambda (exn)
                     (match (exn:fail:syntax-exprs exn)
                       [`() (log-warning 
                             "internal error, no source position given")
                            (failure `(div (p ,(exn-message exn))))]
                       [other
                        (define most-specific (last other))
                        (posn-span->fail (syntax-position most-specific)
                                         (syntax-span most-specific)
                                         usertext
                                         (exn-message exn))]))])
    (thunk)))



;; catch exn:fail:read and exn:fail:syntax, turn them into callerfails (for use on patterns)
(define (catch-reader-parser-pattern-errors patterntext thunk)
  (with-handlers ([exn:fail:read? 
                   (lambda (exn) 
                     (raise
                      (abort-with
                       (callerfail 
                        (format "problem while parsing pattern: ~s on ~s with srclocs ~s" 
                                (exn-message exn)
                                patterntext
                                (exn:fail:read-srclocs exn))))))]
                  [exn:fail:syntax? 
                   (lambda (exn)
                     (raise
                      (abort-with
                       (callerfail 
                        (format "problem while parsing pattern: ~s on ~s at ~s"
                                (exn-message exn)
                                patterntext
                                (map syntax->datum (exn:fail:syntax-exprs exn)))))))])
    (thunk)))

;; given position and span and message, produce a fail message:
(define (posn-span->fail posn span usertext message)
  (match (list posn span)
    [`(,(? number? p) ,(? number? s))
     (fail-msg p (+ p s) usertext #:msg message)]
    ;; oh dear, fallback:
    [other
     (log-warning 
      (format "parse/read error with no source or span: ~s" (list posn span)))
     (failure `(div (p ,message)))]))


;; compare-parsed : correct-parsed user-parsed -> (or/c success? failure?)
(define (compare-parsed correct-parsed usertext parser)
  (cond [(equal? usertext "")
         (failure empty-input-msg)]
        [else
         (define user-parsed (parser usertext))
         (with-handlers ([procedure? (lambda (fail-maker) 
                                       (fail-maker usertext))])
           (begin (unless (struct? user-parsed)
                    (error 'compare-parsed "internal error 20110321-19:44, expected user parsed to be a struct"))
                  (parsed-exp-equal? correct-parsed user-parsed #f)
                  (success)))]))





;; parsed-exp-equal? : parsed parsed src-offset src-offset -> boolean
;; determine whether two parsed structures are the same up to source positions
;; NB: uses a coarse def'n of parseds as either
;; structs whose first element is a source position and whose remaining positions are
;; - parseds, or
;; - #f, or
;; - lists of parseds, or
;; - values comparable with equal?

;; ** okay,right now I'm going off the deep end and special-casing things as they come
;;    up, to give better error messages. We'll see where this ends....

;; the parent-src is used to report error positions for source terms that are not 
;; structs. Because of the data definition we're using, this must be the immediate
;; parent node.
;; EFFECT: uses 'raise' to exit quickly when a difference is detected.
;; INVARIANT: should never *return* anything other than #t. ('raise' otherwise.)
;; NOTE: if 'user-parsed' is a struct, then 'parent-src' is unused (can be #f).
(define (parsed-exp-equal? correct-parsed user-parsed parent-src)
  (cond [(and (struct? user-parsed) (struct? correct-parsed))
         (define user-vec (struct->vector user-parsed))
         (define correct-vec (struct->vector correct-parsed))
         (define user-src (vector-ref user-vec 1))
         (unless (equal? (vector-ref user-vec 0) (vector-ref correct-vec 0))
           (fail-jump user-src #:msg wrong-struct-kind-msg))
         ;; below here we know the two are the same kind of struct.
         
         ;; the fall-through procedure:
         (define (check-subfields)
           (unless (= (vector-length user-vec) (vector-length correct-vec))
             (error 'parsed-exp-equal?
                    "internal error 20110322: two expressions with struct type ~a have different numbers of fields (~a and ~a)"
                    (vector-ref correct-vec 0)
                    (vector-length correct-vec)
                    (vector-length user-vec)))
           
           ;; ignore the struct type & source posn, recur on the rest:
           (for/and ([i (in-range 2 (vector-length user-vec))])
             (parsed-exp-equal?  (vector-ref correct-vec i)
                                 (vector-ref user-vec i)
                                 user-src)))
         
         ;; is the user field missing in this slot? (closed over vectors)
         (define (missing-piece slot)
           (define vector-index (+ 1 slot))
           (and (false? (vector-ref user-vec vector-index))
                (struct? (vector-ref correct-vec vector-index))))
         
         ;; special-cases for better error messages:
         (match (vector-ref correct-vec 0)
           ;; check operator before other elements in binops:
           ['struct:expr:binop
            (unless (eq? (id:op-name (expr:binop-op user-parsed))
                         (id:op-name (expr:binop-op correct-parsed)))
              (fail-jump (id-src (expr:binop-op user-parsed))
                         #:msg wrong-operator-msg))
            (check-subfields)]
           ;; special-cases for missing expressions
           ['struct:stmt:if
            (cond [(missing-piece 3)
                   (fail-jump user-src #:msg missing-if-alt-msg)]
                  [else (check-subfields)])]
           ['struct:stmt:for
            (cond [(missing-piece 1)
                   (fail-jump user-src #:msg missing-for-init-msg)]
                  [(missing-piece 2)
                   (fail-jump user-src #:msg missing-for-test-msg)]
                  [(missing-piece 3)
                   (fail-jump user-src #:msg missing-for-update-msg)]
                  [else (check-subfields)])]
           ['struct:stmt:return
            (cond [(missing-piece 1)
                   (fail-jump user-src #:msg missing-return-exp-msg)]
                  [else (check-subfields)])]           
           [other (check-subfields)])]
        [(and (struct? user-parsed) (false? correct-parsed))
         (fail-jump (vector-ref (struct->vector user-parsed) 1)
                    #:msg should-be-absent-msg)]
        [(and (false? user-parsed) (struct? correct-parsed))
         (error 'parsed-exp-equal?
                "internal error 201110181111: should have been caught higher up")]
        [(or (struct? user-parsed) (struct? correct-parsed))
         (error 'parsed-exp-equal?
                "internal error 201103221829: one struct, one non-struct: ~a and ~a"
                correct-parsed
                user-parsed)]
        ;; handle lists (as e.g. in argument lists)
        [(and (list? user-parsed) (list? correct-parsed))
         (cond [(< (length user-parsed) (length correct-parsed))
                ;; if user-parsed is empty, take source position
                ;; from parent:
                (cond [(empty? user-parsed)
                       (fail-jump parent-src
                                  #:msg missing-elements-msg)]
                      [else 
                       (fail-jump (join-srcs (map expr-or-stmt-src 
                                                  user-parsed))
                                  #:msg missing-elements-msg)])]
               [(< (length correct-parsed) (length user-parsed))
                (fail-jump (join-srcs (map expr-or-stmt-src user-parsed))
                           #:msg extra-elements-msg)]
               [else
                (for/and ([c (in-list correct-parsed)]
                          [u (in-list user-parsed)])
                  (parsed-exp-equal? c u parent-src))])]
        [(or (list? user-parsed) (list? correct-parsed))
         (error 'parsed-exp-equal?
                "internal error 201103221830: one list, one non-list: ~a and ~a"
                correct-parsed
                user-parsed)]
        ;; handle everything else
        [else (fail-wrap (equal? user-parsed correct-parsed) parent-src)]))

;; get the source of either an expr or stmt:
(define (expr-or-stmt-src expr-or-stmt)
  (vector-ref (struct->vector expr-or-stmt) 1))


;; fail-wrap : bool src-posn -> #t
;; EFFECT: uses "raise" to exit if the bool is #f
(define (fail-wrap b src)
  (or b (fail-jump src)))

;; raise a procedure which will take the user text and return a failure
(define (fail-jump src #:msg [fail-message default-error-msg])
  (raise (lambda (usertext)
           (fail-msg (src-start-offset src)
                   (src-end-offset src)
                   usertext
                   #:msg fail-message))))

;; fail-msg : integer integer optional-message -> string - > failure
(define (fail-msg start end usertxt #:msg [message-text default-error-msg])
  (define pre (substring usertxt 0 (- start 1)))
  (define middle (substring usertxt (- start 1) (- end 1)))
  (define post (substring usertxt (- end 1) (string-length usertxt)))
  (failure `(div (p ,message-text) 
                 (p (span
                     (|@| (style "font-family: monospace;"))
                     ,pre 
                     (span (|@| (style "border: 1px solid rgb(50, 50, 50); background-color : rgb(250,200,200);")) ,middle)
                     ,post)))))

;; join-srcs : create a new synthetic source expression that spans a
;; list of existing ones, use #f for the path
(define (join-srcs losrcs)
  (define f (argmin src-start-offset losrcs))
  (define l (argmax src-end-offset losrcs))
  (src (src-start-offset f)
       (src-start-line f)
       (src-start-col f)
       (src-end-offset l)
       (src-end-line l)
       (src-end-col l)
       #f))


(define wrong-struct-kind-msg
  "I wasn't expecting this kind of thing here:")
(define should-be-absent-msg
  "I didn't expect to find anything here. Try taking out this expression or statement:")
(define missing-if-alt-msg
  "This 'if' is missing its 'else' case:")
(define missing-for-init-msg
  "This 'for' is missing its initialization expression:")
(define missing-for-test-msg
  "This 'for' is missing its test expression:")
(define missing-for-update-msg
  "This 'for' is missing its update expression:")
(define missing-return-exp-msg
  "This 'return' is missing its argument:")
(define default-error-msg
  "It looks like you need to fix the boxed part:")
(define wrong-operator-msg
  "I expected a different operator here:")
(define extra-elements-msg
  "I found more elements than I expected, here:")
(define missing-elements-msg 
  "I found fewer elements than I expected, here:")
(define empty-input-msg
  "This box is empty.")
(define parse-fail-msg "I got confused while parsing this token:")









;; TESTING

(check-equal? (fail-wrap #true (src 1 2 3 4 5 6 7)) #t)
(check-exn procedure? 
           (lambda () (fail-wrap #false (src 1 2 3 4 5 6 7))))

(check-equal? (parses-as-addition? "  /* abc */ 3 + // \n 4") #t)
(check-equal? (parses-as-addition? "4 - 6") #f)
(check-equal? (parses-as-addition? "3 + 4 + 6") #f)
(check-equal? (parses-as-addition? "4") #f)
(check-equal? (any-c-addition "234 2987") 
              (fail-msg 5 9 "234 2987"
                        #:msg "parse: unexpected integer literal"))
(check-equal? (any-c-addition "098732")
              (fail-msg 1 7 "098732" #:msg "bad number literal: 098732"))





(check-equal? (parses-as-int? "  34") #t)
(check-equal? (parses-as-int? "  a") #f)
(check-equal? (parses-as-int? "  3 // zappa") #t)
(check-equal? (parses-as-int? "  3.4 // zappa") #f)
(check-equal? (any-c-int "098273")
              (fail-msg 1 7 "098273" #:msg "bad number literal: 098273"))



;; GENERAL PARSER MATCH TESTS

(define (p-test str-a str-b)
  (compare-parsed (parse-expression str-a) str-b parse-expression))

(check-equal? (p-test "234" "  234 /*oth*/") (success))
(check-equal? (p-test "234" "  235 /*oth*/") (fail-msg 3 6 "  235 /*oth*/"))
(check-equal? (p-test "(2342 + 22)" "2342 + 22") (success))
(check-equal? (p-test "(2342 + 22)" "2343 + 22") (fail-msg 1 5 "2343 + 22"))
(check-equal? (p-test "((x + 34) + 22)" "x+34+22") (success))
(check-equal? (p-test "((x + 34) + 22)" "x+(34+22)")
              (fail-msg 1 2 "x+(34+22)" #:msg wrong-struct-kind-msg))
(check-equal? (p-test "((x + 34) + 22)" "y+34+22") (fail-msg 1 2 "y+34+22"))
;; wrong type of expression:
(check-equal? (p-test "3 + 4" "f(x)")
              (fail-msg 1 5 "f(x)" #:msg wrong-struct-kind-msg))
;; check operators first:
(check-equal? (p-test "3 + 4" "5 * 7")
              (fail-msg 3 4 "5 * 7" #:msg wrong-operator-msg))
;; missing arguments in funcalls:
(check-equal? (p-test "f(3,4,6)" "f(x)")
              (fail-msg 3 4 "f(x)" #:msg missing-elements-msg))
;; extra arguments in funcalls:
(check-equal? (p-test "f(3,4)" "f(0,0,0,0)")
              (fail-msg 3 10 "f(0,0,0,0)" #:msg extra-elements-msg))
;; decent error message on empty string:
(check-equal? (p-test "f(3,4)" "")
              (failure empty-input-msg))



;; this level can catch syntactic errors:

(check-equal? ((pattern->matcher parse-expression "((x + 34) + 22)") "x+34+22") (success))
(check-equal? ((pattern->matcher parse-expression "((x + 34) + 22)") "x+35+22") 
              (fail-msg 3 5 "x+35+22"))
(check-equal? ((pattern->matcher parse-expression "((x + 34) + 22)") "x+35+ +22;") 
              (fail-msg 10 11 "x+35+ +22;" #:msg "parse: unexpected semi-colon (`;')"))

;; test the wrapper function:
(check-equal? (c-parser-match "((x + 34) + 22)"
                              "x+34+22")
              (success))
(check-equal? (failure? (c-parser-match "((x + 34) + 22)"
                                        "x+34+029"))
              #t)

;; Parse failures:

(check-equal? 
 (c-parser-match "123413"
                 "234 123")
 (fail-msg 5 8 "234 123"
           #:msg "parse: unexpected integer literal"))

;; read failures

(check-equal? 
 (c-parser-match "abc" "091823740")
 (fail-msg 1 10 "091823740" #:msg "bad number literal: 091823740"))



;; once again on statements:

(define (s-test str-a str-b)
  (compare-parsed (parse-statement str-a) str-b parse-statement))

(check-equal? (s-test "234;" "  234 /*oth*/;") (success))
(check-equal? (s-test "234;" "  235 /*oth*/;") (fail-msg 3 6 "  235 /*oth*/;"))
(check-equal? (s-test "if (3 < 4) { return 4; } else {return 2;}"
                      "if    ((3 < 4)) {return 4;} else {return 2;}") (success))
(check-equal? (s-test "if (3 < 4) { return 4; } else {return 2;}"
                      "if    ((3 < 4)) {return 4+3;} else return 2;") 
              (fail-msg 25 28 
                        "if    ((3 < 4)) {return 4+3;} else return 2;"
                        #:msg wrong-struct-kind-msg))
(check-equal? (s-test "return f(x,15);"
                      "return f();")
              (fail-msg 8 11
                        "return f();"
                        #:msg missing-elements-msg))


(check-equal? (s-test "return;"
                      "return 3;")
              (fail-msg 8 9
                        "return 3;"
                        #:msg should-be-absent-msg))

(check-equal? (s-test "return f(x,15);" "return;")
              (fail-msg 1 8 "return;"
                        #:msg missing-return-exp-msg))
(check-equal? (s-test "if (3) 4; else return 5;" "if (3) 4;")
              (fail-msg 1 10 "if (3) 4;"
                        #:msg missing-if-alt-msg))
(check-equal? (s-test "for (3;4;5) 6;" "for (;4;5) 6;")
              (fail-msg 1 14 "for (;4;5) 6;"
                        #:msg missing-for-init-msg))
(check-equal? (s-test "for (3;4;5) 6;" "for (3;;5) 6;")
              (fail-msg 1 14 "for (3;;5) 6;"
                        #:msg missing-for-test-msg))
(check-equal? (s-test "for (3;4;5) 6;" "for (3;4;) 6;")
              (fail-msg 1 14 "for (3;4;) 6;"
                        #:msg missing-for-update-msg))



(check-equal? (c-stmt-parser-match "if (3 < 4) { return 4; } else {return 2;}"
                                   "if    ((3 < 4)) {return 4+3;} else return 2;")
              (fail-msg 25 28 
                        "if    ((3 < 4)) {return 4+3;} else return 2;"
                        #:msg wrong-struct-kind-msg))
(check-equal? (c-stmt-parser-match "if (3 < 4) { return 4; } else {return 2;}"
                                   "")
              (failure empty-input-msg))

;; another bug:

(check-equal? (s-test "if (3) {4;} else {5;6;}"
                     "if (3) {4;} else {5;}")
             (fail-msg 19 21 "if (3) {4;} else {5;}" #:msg missing-elements-msg))
(check-equal? (s-test "if (3) {4;} else {5;6;}"
                     "if (3) {4;} else {5;6;7;}")
             (fail-msg 19 25 "if (3) {4;} else {5;6;7;}" #:msg extra-elements-msg))


;; bad pattern:

(check-equal? (callerfail? (c-stmt-parser-match "not a c expression"
                                                "1234;"))
              #t)


(check-equal? (c-stmt-parser-match "if (( color == 'B' ) ||  ( color == 'V')) { dark = 'Y'; } else { dark = 'N'; }"
                                   "if (( color == 'B' ) ||  ( color == 'V')) { dark = 'Y'; } else { dark = 'N'; }"
                                   )
              #s(success))



(check-equal? (c-parser-match "foo" "foo")
 #s(success))

(check-equal? 
 (c-parser-match "3 + 4" "6 + 4")
 (fail-msg 1 2 "6 + 4"))


(check-equal? 
 (join-srcs
  (list (src 8 1 7 12 1 11 #f)
        (src 3 1 2 4 1 3 #f)
        (src 5 1 4 6 1 5 #f)))
 (src 3 1 2 12 1 11 #f))

