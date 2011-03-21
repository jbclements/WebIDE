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
  (cond [(parses-as-int? usertext) (success)]
        [else (failure (format "~v doesn't parse as an integer"
                               usertext))]))

(define (parses-as-int? str)
  (catch-reader-parser-errors
   (lambda ()
     (match (parse-expression str)
       [(struct expr:int (src value qualifiers)) #t]
       [other #f]))))


;; abstraction needed here...
(define (any-c-addition args texts)
  (define usertext (extract-1-user-string texts))
  (cond [(parses-as-addition? usertext) (success)]
        [else (failure 
               (format "~v doesn't parse as the sum of two integers"
                       usertext))]))

;; does this string parse as (+ (int) (int)) ?
;; what to do on a parse error?
(define (parses-as-addition? str)
  ;; could be better, wait for abstraction:
  (catch-reader-parser-errors
   (lambda ()
     (match (parse-expression str)
       [(struct expr:binop (src_1 (struct expr:int (dc_1 dc_2 dc_3)) 
                                  (struct id:op (src2 '+))
                                  (struct expr:int (dc_4 dc_5 dc_6)))) 
        #t]
       [other #f]))))


;; does the given text match the pattern?
(define (c-parser-match args texts)
  ;; patterns are just going to be strings, for now.
  (define pattern (cdr (assoc 'pattern args)))
  (define matcher (pattern->matcher pattern))
  (define usertext (extract-1-user-string texts))
  (cond [(matcher usertext) (success)]
        [else (failure (format "~s doesn't match pattern ~s"
                               usertext
                               pattern))]))

;; for now, a "pattern" is just a string containing a parsable C expression
(define (pattern->matcher pat)
  (let ([parsed-pattern (parse-expression pat)])
    (lambda (usertext)
      (catch-reader-parser-errors
       (lambda ()
         (parsed-exp-equal? parsed-pattern (parse-expression usertext)))))))


;; catch exn:fail:read and exn:fail:syntax, turn them into #f.
;; hopefully, we'll be able to do a better job of this, soon.
(define (catch-reader-parser-errors thunk)
  (with-handlers ([exn:fail:read? (lambda (exn) #f)]
                  [exn:fail:syntax? (lambda (exn) #f)])
    (thunk)))

;; return the one user string. If more or less 
;; than one, signal an error
(define (extract-1-user-string texts)
  (match texts
    [`((,dc . ,usertext)) usertext]
    [other (error 'extract-1-user-string 
                  "lab spec provided wrong number of user fields in ~v" texts)]))

;; parsed-exp-equal? : parsed parsed -> boolean
;; determine whether two parsed structures are the same up to source positions
;; NB: uses a coarse def'n of parseds as either
;; structs whose first element is a source position and whose remaining positions
;;   are parseds, or
;; values comparable with equal?
(define (parsed-exp-equal? parsed-a parsed-b)
  (cond [(and (struct? parsed-a) (struct? parsed-b))
         (define a-vec (struct->vector parsed-a))
         (define b-vec (struct->vector parsed-b))
         (and (equal? (vector-ref a-vec 0) (vector-ref b-vec 0))
              (= (vector-length a-vec) (vector-length b-vec))
              (for/and ([i (in-range 2 (vector-length a-vec))])
                (parsed-exp-equal? (vector-ref a-vec i) (vector-ref b-vec i))))]
        [else (equal? parsed-a parsed-b)]))


;; TESTING

(define (pee-test str-a str-b)
  (parsed-exp-equal? (parse-expression str-a) (parse-expression str-b)))

(check-equal? (pee-test "234" "  234 /*oth*/") #t)
(check-equal? (pee-test "234" "  235 /*oth*/") #f)
(check-equal? (pee-test "(2342 + 22)" "2342 + 22") #t)
(check-equal? (pee-test "(2342 + 22)" "2343 + 22") #f)
(check-equal? (pee-test "((x + 34) + 22)" "x+34+22") #t)
(check-equal? (pee-test "((x + 34) + 22)" "x+(34+22)") #f)
(check-equal? (pee-test "((x + 34) + 22)" "y+34+22") #f)

(check-equal? ((pattern->matcher "((x + 34) + 22)") "x+34+22") #t)
(check-equal? ((pattern->matcher "((x + 34) + 22)") "x+35+22") #f)

(check-equal? (c-parser-match '((pattern . "((x + 34) + 22)"))
                              '((frog . "x+34+22")))
              (success))
(check-equal? (failure? (c-parser-match '((pattern . "((x + 34) + 22)"))
                                        '((frog . "x+34+022"))))
              #t)


(check-equal? (extract-1-user-string '((foo . "bar"))) "bar")
(check-exn exn:fail? (lambda ()
                        (extract-1-user-string '((foo . "bar")
                                                 (baz . "quux")))))

(check-equal? (parses-as-addition? "  /* abc */ 3 + // \n 4") #t)
(check-equal? (parses-as-addition? "4 - 6") #f)
(check-equal? (parses-as-addition? "3 + 4 + 6") #f)
(check-equal? (parses-as-addition? "4") #f)
(check-equal? (parses-as-addition? "234 2987") #f)
(check-equal? (parses-as-addition? "09872") #f)
(check-equal? (any-c-addition '() '((frog . "098732")))
              (failure "\"098732\" doesn't parse as the sum of two integers"))


(check-equal? (parses-as-int? "  34") #t)
(check-equal? (parses-as-int? "  a") #f)
(check-equal? (parses-as-int? "  3 // zappa") #t)
(check-equal? (parses-as-int? "  3.4 // zappa") #f)
(check-equal? (parses-as-int? "098273") #f)
