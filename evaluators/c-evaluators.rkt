#lang racket

(require "shared.rkt"
         (planet dherman/c:3:2)
         rackunit)

;; this file contains the definitions of the evaluators for C, most notably
;; the "c-parser-match" evaluator, that tries to guide the student toward 
;; producing code that parses to the same result as the instructor's code.

(provide any-c-int
         any-c-addition
         c-parser-match)


;; abstract over patterns before trying to do a better job...

;; take the text supplied by the user,
;; check that it parses to an int
(define (any-c-int args texts)
  (define usertext (extract-1-user-string texts))
  (catch-reader-parser-errors
   (lambda ()
     (cond [(parses-as-int? usertext) (success)]
           [else (failure (format "~v doesn't parse as an integer"
                                  usertext))]))))

(define (parses-as-int? str)
  (match (parse-expression str)
    [(struct expr:int (src value qualifiers)) #t]
    [other #f]))


;; abstraction needed here...
(define (any-c-addition args texts)
  (define usertext (extract-1-user-string texts))
  (catch-reader-parser-errors
   (lambda ()
     (cond [(parses-as-addition? usertext) (success)]
           [else (failure 
                  (format "~v doesn't parse as the sum of two integers"
                          usertext))]))))

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
  (define usertext (extract-1-user-string texts))
  (matcher usertext))

;; for now, a "pattern" is just a string containing a parsable C expression
(define (pattern->matcher pat)
  (let ([parsed-pattern (parse-expression pat)])
    (lambda (usertext)
      (catch-reader-parser-errors
       (lambda ()
         (compare-parsed parsed-pattern (parse-expression usertext)))))))


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

;; return the one user string. If more or less 
;; than one, signal an error
(define (extract-1-user-string texts)
  (match texts
    [`((,dc . ,usertext)) usertext]
    [other (error 'extract-1-user-string 
                  "lab spec provided wrong number of user fields in ~v" texts)]))

;; compare-parsed : correct-parsed user-parsed -> (or/c success? failure?)
(define (compare-parsed correct-parsed user-parsed)
  (with-handlers ([src? 
                   (lambda (src)
                     (fail-msg (src-start-offset src)
                               (src-end-offset src)))])
    (begin (unless (struct? user-parsed)
             (error 'compare-parsed "internal error 20110321-19:44, expected user parsed to be a struct"))
           (parsed-exp-equal? correct-parsed user-parsed #f)
           (success))))

;; parsed-exp-equal? : parsed parsed src-offset src-offset -> boolean
;; determine whether two parsed structures are the same up to source positions
;; NB: uses a coarse def'n of parseds as either
;; structs whose first element is a source position and whose remaining positions
;;   are parseds, or
;; values comparable with equal?
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
         (and (fail-wrap (equal? (vector-ref user-vec 0) (vector-ref correct-vec 0)) user-src)
              (fail-wrap (= (vector-length user-vec) (vector-length correct-vec)) user-src)
              (for/and ([i (in-range 2 (vector-length user-vec))])
                (parsed-exp-equal? (vector-ref correct-vec i) (vector-ref user-vec i) user-src)))]
        [else (fail-wrap (equal? user-parsed correct-parsed) parent-src)]))


;; fail-wrap : bool src-posn -> #t
;; EFFECT: uses "raise" to exit if the bool is #f
(define (fail-wrap b src)
  (or b (raise src)))

;; fail-msg : integer integer -> failure
(define (fail-msg start end)
  (failure (format
            "part of it looks good, but you need to fix characters ~s-~s"
            start end)))

(check-equal? (fail-wrap #true (src 1 2 3 4 5 6 7)) #t)
(check-exn (lambda (raised) (equal? raised (src 1 2 3 4 5 6 7)))
           (lambda () (fail-wrap #false (src 1 2 3 4 5 6 7))))


;; TESTING

(define (pee-test str-a str-b)
  (compare-parsed (parse-expression str-a) (parse-expression str-b)))

(check-equal? (pee-test "234" "  234 /*oth*/") (success))
(check-equal? (pee-test "234" "  235 /*oth*/") (fail-msg 3 6))
(check-equal? (pee-test "(2342 + 22)" "2342 + 22") (success))
(check-equal? (pee-test "(2342 + 22)" "2343 + 22") (fail-msg 1 5))
(check-equal? (pee-test "((x + 34) + 22)" "x+34+22") (success))
(check-equal? (pee-test "((x + 34) + 22)" "x+(34+22)") (fail-msg 1 2))
(check-equal? (pee-test "((x + 34) + 22)" "y+34+22") (fail-msg 1 2))

(check-equal? ((pattern->matcher "((x + 34) + 22)") "x+34+22") (success))
(check-equal? ((pattern->matcher "((x + 34) + 22)") "x+35+22") (fail-msg 3 5))

(check-equal? (c-parser-match '((pattern . "((x + 34) + 22)"))
                              '((frog . "x+34+22")))
              (success))
(check-equal? (failure? (c-parser-match '((pattern . "((x + 34) + 22)"))
                                        '((frog . "x+34+029"))))
              #t)


(check-equal? (extract-1-user-string '((foo . "bar"))) "bar")
(check-exn exn:fail? (lambda ()
                        (extract-1-user-string '((foo . "bar")
                                                 (baz . "quux")))))

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



(check-equal? 
 (c-parser-match '((pattern . "foo"))
                 '((blah . "foo")))
 #s(success))

(check-equal? 
 (c-parser-match '((pattern . "3 + 4"))
                 '((blah . "6 + 4")))
 (failure "part of it looks good, but you need to fix characters 1-2"))
