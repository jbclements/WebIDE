#lang racket

(require "shared.rkt"
         (planet dherman/c:3:2)
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
         c-parser-match)


;; abstract over patterns before trying to do a better job...

;; take the text supplied by the user,
;; check that it parses to an int
(define (any-c-int args texts)
  (run-one-with-matcher
   texts
   (lambda (usertext)
     (catch-reader-parser-errors
      (lambda ()
        (cond [(parses-as-int? usertext) (success)]
              [else (failure (format "~v doesn't parse as an integer"
                                     usertext))]))))))

(define (parses-as-int? str)
  (match (parse-expression str)
    [(struct expr:int (src value qualifiers)) #t]
    [other #f]))


;; abstraction needed here...
(define (any-c-addition args texts)
  (run-one-with-matcher
   texts
   (lambda (usertext)
     (catch-reader-parser-errors
      (lambda ()
        (cond [(parses-as-addition? usertext) (success)]
              [else (failure 
                     (format "~v doesn't parse as the sum of two integers"
                             usertext))]))))))

;; does this string parse as (+ (int) (int)) ?
;; what to do on a parse error?
(define (parses-as-addition? str)
  ;; could be better, wait for abstraction:
  (match (parse-expression str)
    [(struct expr:binop (src_1 (struct expr:int (dc_1 dc_2 dc_3)) 
                               (struct id:op (src2 '+))
                               (struct expr:int (dc_4 dc_5 dc_6)))) 
     #t]
    [other #f]))


;; does the given text match the pattern?
(define (c-parser-match args texts)
  ;; patterns are just going to be strings, for now.
  (define pattern (cdr (assoc 'pattern args)))
  (define matcher (pattern->matcher pattern))
  (run-one-with-matcher texts matcher))

;; extract the one text, run the matcher on it
(define (run-one-with-matcher texts matcher)
  (match texts
    [`((,dc . ,usertext)) 
     (matcher usertext)]
    [other (callerfail 
            (format "lab spec provided wrong number of user fields in ~v" texts))]))

;; for now, a "pattern" is just a string containing a parsable C expression
(define (pattern->matcher pat)
  (let ([parsed-pattern (parse-expression pat)])
    (lambda (usertext)
      (catch-reader-parser-errors
       (lambda ()
         (compare-parsed parsed-pattern usertext))))))


;; catch exn:fail:read and exn:fail:syntax, turn them into failures
;; hopefully, we'll be able to do a better job of this, soon.
(define (catch-reader-parser-errors thunk)
  (with-handlers ([exn:fail:read? 
                   (lambda (exn) 
                     (failure "couldn't break input into tokens"))]
                  [exn:fail:syntax? 
                   (lambda (exn)
                     (failure "couldn't parse input"))])
    (thunk)))


;; compare-parsed : correct-parsed user-parsed -> (or/c success? failure?)
(define (compare-parsed correct-parsed usertext)
  (cond [(equal? usertext "")
         (failure empty-input-msg)]
        [else
         (define user-parsed (parse-expression usertext))
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
;; - lists of parseds, or
;; - values comparable with equal?

;; ** okay,right now I'm going off the deep end and special-casing things as they come
;;    up, to give better error messages . We'll see where this ends....

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
           (fail-jump user-src #:msg wrong-expr-kind-msg))
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
         (match (vector-ref correct-vec 0)
           ['struct:expr:binop
            (unless (eq? (id:op-name (expr:binop-op user-parsed))
                         (id:op-name (expr:binop-op correct-parsed)))
              (fail-jump (id-src (expr:binop-op user-parsed))
                         #:msg wrong-operator-msg))
            (check-subfields)]
           [other (check-subfields)])]
        [(or (struct? user-parsed) (struct? correct-parsed))
         (error 'parsed-exp-equal?
                "internal error 201103221829: one struct, one non-struct: ~a and ~a"
                correct-parsed
                user-parsed)]
        ;; handle lists (as e.g. in argument lists)
        [(and (list? user-parsed) (list? correct-parsed))
         (cond [(< (length user-parsed) (length correct-parsed))
                (fail-jump (join-srcs (map expr-src user-parsed))
                           #:msg missing-elements-msg)]
               [(< (length correct-parsed) (length user-parsed))
                (fail-jump (join-srcs (map expr-src user-parsed))
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


(define wrong-expr-kind-msg
  "I wasn't expecting this kind of expression here:")
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


;; TESTING

(check-equal? (fail-wrap #true (src 1 2 3 4 5 6 7)) #t)
(check-exn procedure? 
           (lambda () (fail-wrap #false (src 1 2 3 4 5 6 7))))

(check-equal? (parses-as-addition? "  /* abc */ 3 + // \n 4") #t)
(check-equal? (parses-as-addition? "4 - 6") #f)
(check-equal? (parses-as-addition? "3 + 4 + 6") #f)
(check-equal? (parses-as-addition? "4") #f)
(check-equal? (any-c-addition '() '((doofy . "234 2987"))) 
              (failure "couldn't parse input"))
(check-equal? (any-c-addition '() '((frog . "098732")))
              (failure "couldn't break input into tokens"))


(check-equal? (parses-as-int? "  34") #t)
(check-equal? (parses-as-int? "  a") #f)
(check-equal? (parses-as-int? "  3 // zappa") #t)
(check-equal? (parses-as-int? "  3.4 // zappa") #f)
(check-equal? (any-c-int '() '((foof . "098273")))
              (failure "couldn't break input into tokens"))


;; GENERAL PARSER MATCH TESTS

(define (p-test str-a str-b)
  (compare-parsed (parse-expression str-a) str-b))

(check-equal? (p-test "234" "  234 /*oth*/") (success))
(check-equal? (p-test "234" "  235 /*oth*/") (fail-msg 3 6 "  235 /*oth*/"))
(check-equal? (p-test "(2342 + 22)" "2342 + 22") (success))
(check-equal? (p-test "(2342 + 22)" "2343 + 22") (fail-msg 1 5 "2343 + 22"))
(check-equal? (p-test "((x + 34) + 22)" "x+34+22") (success))
(check-equal? (p-test "((x + 34) + 22)" "x+(34+22)")
              (fail-msg 1 2 "x+(34+22)" #:msg wrong-expr-kind-msg))
(check-equal? (p-test "((x + 34) + 22)" "y+34+22") (fail-msg 1 2 "y+34+22"))
;; wrong type of expression:
(check-equal? (p-test "3 + 4" "f(x)")
              (fail-msg 1 5 "f(x)" #:msg wrong-expr-kind-msg))
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

(check-equal? ((pattern->matcher "((x + 34) + 22)") "x+34+22") (success))
(check-equal? ((pattern->matcher "((x + 34) + 22)") "x+35+22") 
              (fail-msg 3 5 "x+35+22"))
(check-equal? ((pattern->matcher "((x + 34) + 22)") "x+35+ +22;") 
              (failure "couldn't parse input"))


;; test the wrapper function:
(check-equal? (c-parser-match '((pattern . "((x + 34) + 22)"))
                              '((frog . "x+34+22")))
              (success))
(check-equal? (failure? (c-parser-match '((pattern . "((x + 34) + 22)"))
                                        '((frog . "x+34+029"))))
              #t)




(check-equal? 
 (c-parser-match '((pattern . "foo"))
                 '((blah . "foo")))
 #s(success))

(check-equal? 
 (c-parser-match '((pattern . "3 + 4"))
                 '((blah . "6 + 4")))
 (fail-msg 1 2 "6 + 4"))


(check-equal? 
 (join-srcs
  (list (src 8 1 7 12 1 11 #f)
        (src 3 1 2 4 1 3 #f)
        (src 5 1 4 6 1 5 #f)))
 (src 3 1 2 12 1 11 #f))

;; need test cases for fail-msg, but it's evolving too fast...