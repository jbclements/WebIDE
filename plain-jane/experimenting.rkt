#lang at-exp racket

(require web-server/servlet-env
         "page-displayer.rkt"
         "lab-definition-utils.rkt"
         #;(file "/Users/clements/trac-webide/labs/validate-lib.rkt"))

(define eval-server-url-prefix 
  ;; remote host:
  #;"http://brinckerhoff.org:8025/"
  ;; intra-process
  "evaluator://"
  )


;; FIXME: 
;; - paragraph wrapping

(define (the-lab) 
  (lab 
   "First C Lab"
   @step["Integers" #:dependencies `()]{ 
 
 The first step in any programming task is to think about the kinds of 
 data that you need, and the first step in learning a new programming 
 language is to understand the kinds of data that it can represent.
 
 These are called "values."
 
 In C, the most basic set of values are the whole numbers, or "integers."
 13 is an integer, and so is 0, and so is -146.
 
 Try entering an integer in this box:
 @buttonregion[(box (url "any-c-int") '())]
 
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
 @buttonregion[(box (url "any-c-addition") '())]
 
 So, what are the gotchas? The biggest one has to do with integer division.
 In C, dividing an integer by an integer produces another integer.  This works
 as you might expect for quotients like @code{4 / 2}, but when the denominator 
 does not evenly divide the numerator, 
 C rounds toward zero. (What if one is negative? Don't ask. Please.)
 
 Okay, quiz time: fill in the integers that result from the following division operations:
 
 @buttonregion[
 @table[
 (list (code "10 / 5") (c-parser-box "2"))
 (list (code "9 / 4") (c-parser-box "2"))
 (list (code "3 / 12") (c-parser-box "0"))]]
 
 
 @h3{Nested Operations}
 
 Like normal mathematical notation, multiple elements may be joined together using
 arithmetic operators.  For instance @code{3 + 4 + 5} is a fine expression, and 
 so is @code{20 * 14 * 3}.
 
 Write the C expression that adds fifteen, sixteen, and twenty-three:
 @buttonregion[(c-parser-box "15+16+23")]
 
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
 (list "ten plus the result of 4 divided by 2" 
       (c-parser-box "10+(4/2)") 
       (c-parser-box "12"))
 (list "the result of four plus five, divided by three" 
       (c-parser-box "(4+5)/3")
       (c-parser-box "3"))
 (list "four times the result of five minus two, divided by six"
       (c-parser-box "(4*(5-2))/6")
       (c-parser-box "2"))]]}))



#;(require (file "/Users/clements/trac-webide/labs/validate-lib.rkt"))
#;(validate-sxml (the-lab))



;; a box that should match a c-parsed representation
(define (c-parser-box expected)
  (box (url "c-parser-match")
       `((pattern ,expected))))

(define (url x) (string-append eval-server-url-prefix x))




;; try-one-evaluator : evaluator -> 
(define (try-one-evaluator eval)
  (lab 
   "testing a single evaluator lab"
   (step "testing a single evaluator step"
         ""
         "Here's the evaluator you wanted to test:"
         (buttonregion eval))))

;; run the lab, using the plain-jane WebIDE agent:
(define (go lab)
  (serve/servlet 
   (lambda (request)
     (run-lab lab))))


(define show-off-example
  (try-one-evaluator (c-parser-box "f((3+4)/7,234.3)")))


;; WHAT SHOULD WE ACTUALLY RUN?
(go 
 show-off-example
 #;(the-lab))




