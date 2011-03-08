#lang at-exp racket

(require web-server/servlet-env)

(require "page-displayer.rkt"
         #;(file "/Users/clements/trac-webide/labs/validate-lib.rkt"))



(define (lab name . content) 
  `(w1:lab (|@| (name ,name)) ,@content))
(define (step name . content)
  `(w1:step (|@| (name ,name)) ,@content))
(define (h3 . content) (cons 'w1:h3 content))

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

(define (box . content) 
  `(w1:))

(define the-lab 
  `(*TOP* 
    (|@| (*NAMESPACES* (n "http://www.web-ide.org/namespaces/labs/1")))
    , @lab["First C Lab"]{
    , @step{
 @h3{Integers}
 @p{
 The first step in any programming task is to think about the kinds of 
 data that you need, and the first step in learning a new programming 
 language is to understand the kinds of data that it can represent.}
 
 @p{These are called "values."}
 
 @p{In C, the most basic set of values are the whole numbers, or "integers."
 13 is an integer, and so is 0, and so is -146.}
 
 @p{Try entering an integer in this box:
 @box[intbox]}
 
 @p{If you like, you can try a bunch of different integers. Are there
 things that you expect to be integers that aren't? If you're unsure
 about whether something is an integer in C, you can come back and use
 this box.}
 
 @h3{Arithmetic operations}
 
 @p{The C language has a variety of boolean operations, including addition (+),
 subtraction (-), multiplication (*), and division (/).  Make sure you can 
 find all of these on the keyboard.}

 @p{These operators behave more or less as you might expect, with a few gotchas.}
 
 @p{To add @code{3} and @code{4} together, for instance, you can write @code{3 + 4} .}
 }}))


(define (start request)
  (run-lab the-lab))

(serve/servlet start)