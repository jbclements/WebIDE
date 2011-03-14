#lang at-exp racket

(require web-server/servlet-env
         rackunit)

(require "page-displayer.rkt"
         "evaluator-collector.rkt"
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
     @step["Integers" #:dependencies `()]{ 
 
 The first step in any programming task is to think about the kinds of 
 data that you need, and the first step in learning a new programming 
 language is to understand the kinds of data that it can represent.
 
 These are called "values."
 
 In C, the most basic set of values are the whole numbers, or "integers."
 13 is an integer, and so is 0, and so is -146.
 
 Try entering an integer in this box:
 @box&button[intbox]
 
 @;{HOLD OFF ON THIS UNTIL YOU'VE GOT A REAL PARSER...
    If you like, you can try a bunch of different integers. Are there
 things that you expect to be integers that aren't? If you're unsure
 about whether something is an integer in C, you can come back and use
 this box.}
 
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
 (list "ten plus the result of 4 divided by 2" (box 10+4/2box) (box (litbox 12)))
 (list "the result of four plus five, divided by three" (box 4+5/3box) (box (litbox 3)))
 (list "four times the result of five minus two, divided by six" (box 4*5-2/6box) (box (intbox 2)))]]
 }}))


;; accepts everything; FOR TESTING ONLY...
(define (bogus str) #t)

(define (intbox str)
  (only-regexp "[+-]?[0-9]+"))

(define plusbox bogus)
(define 4*5-2/6box bogus)
(define 4+5/3box bogus)
(define 10/5box bogus)
(define 9/4box bogus)
(define 3/12box bogus)
(define 15+16+23box bogus)
(define 10+4/2box bogus)

(define (litbox lit) bogus)

;; given a list of lists of cell contents, produce a table:
(define (table . rows)
  (cons 'w1:table
        (for/list ([r (in-list rows)])
          `(w1:tr ,@(for/list ([c (in-list r)])
                      `(w1:td ,c))))))

(check-equal? (table (list 3 4) (list 5 6))
              `(w1:table (w1:tr (w1:td 3) (w1:td 4))
                         (w1:tr (w1:td 5) (w1:td 6))))


;; wrap a string with whitespace padding to make a whole-line matcher
(define (only-regexp pxstr)
  (lambda (str)
    (regexp-match (pregexp (string-append "^\\s*" pxstr "\\s*$")) str)))

(define (lab name . content) 
  `(w1:lab (|@| (name ,name))
           ,@content))
(define (step name 
             #:dependencies [dependencies `()] 
             #:evaluators [evaluators `()]
             . content)
  `(pre-step (|@| (name ,name)) ,@content))
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

;; We'll clean these up in a pass over the whole body
(define (buttonregion #:label [label "go"] . elts) 
  `(buttonregion (|@| (label ,label)) ,@elts))

;; a problem consists of a ... an evaluator only?

;; an evaluator contains a ... well, what if we just put a function in there, for now?

;; in that case, "box" should just emit an evaluator element to the collector, and
;; spit out a fresh textfield. This only works for single-box evaluators.
(define (box eval-fun)
  (let ([new-textfield-id (next-textfield-id)]
        [new-evaluator-id (next-evaluator-id)])
    `(w1:div
      (w1:evaluator (|@| (href "evaluator://racketfun")
                         #;(evalfun  "TMP-FUN" #;,eval-fun)
                         (name ,new-evaluator-id))
                 (w1:userfieldArg (w1:name "userText") 
                                  (w1:value ,new-textfield-id)))
      (w1:userfield (|@| (id ,new-textfield-id))))))

;; a box with a button right next to it:
(define (box&button eval-fun)
  (buttonregion (box eval-fun)))

;; GENERATE FRESH TEXTFIELD-IDs and EVALUATOR-IDs

(define textfield-ctr 0)
(define (next-textfield-id)
  (set! textfield-ctr (+ textfield-ctr 1))
  (format "text-field-~v" textfield-ctr))

(define evaluator-ctr 0)
(define (next-evaluator-id)
  (set! evaluator-ctr (+ evaluator-ctr 1))
  (format "evaluator-~v" evaluator-ctr))



;; POST-PASS TO COLLECT EVALUATORS
(define collected 
  (collect-evaluators (the-lab)))




(require (file "/Users/clements/trac-webide/labs/validate-lib.rkt"))

(validate-sxml collected)

(define (start request)
  (run-lab collected))

(serve/servlet start)