#lang at-exp racket

;; copyright 2010-2014 John Clements (clements@racket-lang.org)

(require web-server/servlet-env
         "page-displayer.rkt"
         "lab-definition-utils.rkt"
         "validate-lib.rkt")

(define eval-server-url-prefix 
  ;; remote host:
  #;"http://localhost:8025/"
  "http://brinckerhoff.org:8025/"
  ;; intra-process
  #;"evaluator://"
  )

;; FIXME: 
;; - paragraph wrapping

(define (the-lab) 
  (lab 
   "An Example with Statements"
   @step["Statement Example" #:dependencies `()]{
 @h3{A Statement Evaluator Example}
  
  This step is an example of a step asking a student to formulate a statement.
  
  Develop the statement that returns the result of calling the function 'f' with the arguments 'x' and 15:
  
  @buttonregion[(c-stmt-parser-box "return f(x,15);")]
}))



;; a box that should match a c-parsed representation
(define (c-stmt-parser-box expected)
  (box (url "c-stmt-parser-match")
       `((pattern ,expected))))

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
   #:listen-ip #f
   #:port 7980
   #:servlet-regexp #px"" ;; intercept all requests
   (lambda (request)
     (run-lab lab))))


(define show-off-example
  (try-one-evaluator (c-parser-box "f((3+4)/7,234.3)")))


;; WHAT SHOULD WE ACTUALLY RUN?
(go 
 #;show-off-example
 (the-lab))


#;(validate-sxml (the-lab))
