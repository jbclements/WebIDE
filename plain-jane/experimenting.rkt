#lang at-exp racket

(require web-server/servlet-env)

(require "page-displayer.rkt"
         #;(file "/Users/clements/trac-webide/labs/validate-lib.rkt"))


;; FIXME: 
;; - paragraph wrapping
;; - actual evaluators :)
;; - add tags to rnc
;; - better 'code' tag for inline code

(define (the-lab) 
  (list '*TOP* 
    '(|@| (*NAMESPACES* (w1 "http://www.web-ide.org/namespaces/labs/2")))
     @lab["First C Lab"]{
     @step["Integers"]{ 
 
 The first step in any programming task is to think about the kinds of 
 data that you need, and the first step in learning a new programming 
 language is to understand the kinds of data that it can represent.
 
 These are called "values."
 
 In C, the most basic set of values are the whole numbers, or "integers."
 13 is an integer, and so is 0, and so is -146.
 
 Try entering an integer in this box:
 @box&button[intbox]
 
 If you like, you can try a bunch of different integers. Are there
 things that you expect to be integers that aren't? If you're unsure
 about whether something is an integer in C, you can come back and use
 this box.
 
 @h3{Arithmetic operations}
 
 The C language has a variety of boolean operations, including addition (+),
 subtraction (-), multiplication (*), and division (/).  Make sure you can 
 find all of these on the keyboard.

 These operators behave more or less as you might expect, with a few gotchas.
 
 To add @code{3} and @code{4} together, for instance, you can write @code{3 + 4} .
 
 Write an arithmetic expression that uses addition to add two numbers:
 @box&button[plusbox]
 
 So, what are the gotchas? The biggest one has to do with integer division.
 In C, dividing an integer by an integer produces another integer.  This works
 as you might expect for quotients like @code{4 / 2}, but when the denominator 
 does not evenly divide the numerator, 
 C rounds toward zero. (What if one is negative? Don't ask. Please.)
 
 Okay, quiz time: fill in the integers that result from the following division operations:
 
 @buttonregion[
 @table[
 (list (code "10 / 5") (box 10/5box))
 (list (code "9 / 4") (box 9/4box))
 (list (code "3 / 12") (box 3/12box))]]
 
 
 @h3{Nested Operations}
 
 Like normal mathematical notation, multiple elements may be joined together using
 arithmetic operators.  For instance @code{3 + 4 + 5} is a fine expression, and 
 so is @code{20 * 14 * 3}.
 
 Write the C expression that adds fifteen, sixteen, and twenty-three:
 @box&button[15+16+23box]
 
 What about operators such as division and subtraction? These operators are not 
 associative. That is, @code{(20 / 19) / 19} does not produce the same value as 
 @code{20 / (19 / 19)}.  In this case, C's default is to be "left-associative";
 that is, the leftmost operator binds more tightly.  So the expression 
 @code{20 / 19 / 19} is evaluated as @code{(20 / 19) / 19}.
 
 What if that's not what you want?  Well, you can use parentheses--as in the examples
 above--to get the meaning you want. 
 
 Combining different operators is also possible, and gives rise to similar questions.
 For instance, the expression @code{3 + 4 * 5} might evaluate either to 35 or to 23.
 
 In this case, C uses rules of "precedence" to determine which operators "bind more
 tightly".  C follows the standard convention that operators such as multiplication and
 division have higher precedence than addition, so @code{3 + 4 * 5} evaluates
 to 23.  
 
 Precedence rules and associativity are complex; in general, the best advice is simply
 to parenthesize whenever you're unsure.  
 
 Translate the given English into a C expression, and then enter the expected integer result:
 @buttonregion[
 @table[
 (list "ten plus the result of 4 divided by 2" (box 10+4/2box) (box (intbox 12)))
 (list "the result of four plus five, divided by three" (box 4+5/3box) (box (intbox 3)))
 (list "four times the result of five minus two, divided by six" (box 4*5-2/6box) (box (intbox 2)))]]
 }}))

(define (intbox str)
  (only-regexp "[+-]?[0-9]+"))

;; wrap a string with whitespace padding to make a whole-line matcher
(define (only-regexp pxstr)
  (lambda (str)
    (regexp-match (pregexp (string-append "^\\s*" pxstr "\\s*$")) str)))

(define (lab name . content) 
  `(w1:lab (|@| (name ,name)) ,@content))
(define (step name . content)
  `(w1:step (|@| (name ,name)) ,@content))
(define (h3 . content) (cons 'w1:h3 content))

;; a short-cut for defining tag-like functions:
(define-syntax (define-tag-helper stx)
  (syntax-case stx ()
    [(_ id)
     #`(define (id . content)
         `(#,(string->symbol
              (string-append "w1:" (symbol->string (syntax->datum #'id))))
           ,@content))]))

(define-tag-helper p)
(define-tag-helper code)

(define intbox 13)

;; a problem consists of a ... an evaluator only?

;; an evaluator contains a ... well, what if we just put a function in there, for now?

;; in that case, "box" should just emit an evaluator element to the collector, and
;; spit out a fresh textfield. This only works for single-box evaluators.
(define (box eval-fun)
  (let ([new-segid (next-segid)])
    ((current-eval-collector) 'add `(evaluator "evaluator://racketfun" 
                                               ((evalfun ,eval-fun))
                                               (new-segid)))
    `(textfield ,new-segid)))


;; each evaluator must be declared in a buttonregion, which collects the evaluators 
;; defined therein. 
(define-syntax (buttonregion stx)
  (syntax-case stx ()
    [(_ . content)
     #'(buttonregion-helper (lambda () (list . content)))]))

(define (buttonregion-helper thunk)
  (define new-eval-collector (make-eval-collector))
  (parameterize ([current-eval-collector new-eval-collector])
    (cons 'w1:div (append (thunk)
                          (list `(button ,@(new-eval-collector 'get)))))))

(define current-eval-collector (make-parameter #f))

(define (make-eval-collector)
  (let ([l `()])
    (lambda args
      (match args
        [(list 'add new-collector) (set! l (cons new-collector l))]
        [(list 'get) (reverse l)]))))

(let ([m (make-eval-collector)])
  (check-equal? (m 'get) '())
  (m 'add 14)
  (check-equal? (m 'get) '(14))
  (check-equal? (m 'get) '(14))
  (m 'add "foo")
  (check-equal? (m 'get) '(14 "foo")))


(check-equal?
 (buttonregion "abc" ()))



(require (file "/Users/clements/trac-webide/labs/validate-lib.rkt"))

(validate-sxml the-lab)


#;(define (start request)
  (run-lab the-lab))

#;(serve/servlet start)