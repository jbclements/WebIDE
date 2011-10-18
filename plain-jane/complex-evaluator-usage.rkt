#lang at-exp racket

(require web-server/servlet-env
         "page-displayer.rkt"
         "lab-definition-utils.rkt"
         "sxml-static-checking.rkt"
         #;(file "/Users/clements/trac-webide/labs/validate-lib.rkt"))

(define eval-server-url-prefix 
  ;; remote host:
  #;"http://localhost:8025/"
  "http://brinckerhoff.org:8025/"
  ;; intra-process
  #;"evaluator://"
  )


;; this lab has all of the things that should be challenging to render.

;; leaving out dependencies, for now....

;; should the lab or the step be the top-level elements?

;; can there be inconsistent evaluators?

;; static checks for non-overlapping box/evaluator names?
;; oops! also for names that don't refer to anything.

(define (the-lab) 
  (lab 
   "An Example with Statements"
   @step["No buttons at all"]{
   This page doesn't have any boxes or buttons on it at all.
   }
   @step["One box, no buttons"]{
   This page has a box on it, but it doesn't matter what you put in it.
   Type something!
   
   Or not.
   
   @named-userfield["solobox" 20 4]}
   
   @step["One box, one button"]{
   Type the word "ribbon" in this box:
   
   @named-userfield["boxwithbutton" 20 4]
   
   @button['("ribbonEval")]{Ribbon!}
   
   What a strange word. It looks a bit like "gibbon", doesn't it?}
   
   @step["Two boxes, two buttons"]{
 
   In the first box, type the word "splat":
   
   @named-userfield["splatbox" 20 4]
   
   @button['("splatEval")]{Splat!}
   
   In the second box, type the word "wham":
   
   @named-userfield["whambox" 20 4]
   
   @button['("whamEval")]{Wham!}}
   
   @step["One button, one evaluator, multiple boxes"]{
 
   Type the word "beep" in the first box:
   
   @named-userfield["beepbox" 20 4]
   
   Type the word "boop" in the second box:
   
   @named-userfield["boopbox" 20 4]
   
   @button['("beepboopEval")]{Beep/Boop}}
   
   @step["All kinds of crazy stuff"]{
   
   Box 1:
   @named-userfield["crazy1" 20 4]
    
   Box 2:
   @named-userfield["crazy2" 20 4]
   
   Box 3:
   @named-userfield["crazy3" 20 4]
   
   Box 4:
   @named-userfield["crazy4" 20 4]
   
   Evaluators: box2hasA, alwaysSucceed, box3hasB, boxes234haveC
   
   @button['("box2hasA")]{First box has an 'a' in it}
   @button['("box3hasB")]{Third box has a 'b' in it}
   @button['("box3hasB")]{Third box has a 'b' in it (again)}
   @button['("box2hasA" "alwaysSucceed" "boxes234haveC")]{Box 2 has an 'a', boxes 2, 3, and 4 have a 'c', + a secret}
   @button['()]{absolutely nothing}
   
   }))


(define (named-userfield name width height)
  `(w1:userfield (|@| (id ,name)
                      (width ,(number->string width))
                      (height ,(number->string height)))))

(define (button evaluator-names label)
  `(w1:button (|@| (label ,label)) 
              ,(map (lambda (name) `(w1:evaluatorName ,name))
                    evaluator-names)))

;; a box that should match a c-parsed representation
(define (c-stmt-parser-box expected)
  (box (url "c-stmt-parser-match")
       `((pattern ,expected))))

(define (c-parser-box expected)
  (box (url "c-parser-match")
       `((pattern ,expected))))

(define (url x) (string-append eval-server-url-prefix x))




;; run the lab, using the plain-jane WebIDE agent:
(define (go lab)
  (serve/servlet 
   #:listen-ip #f
   #:port 7980
   #:servlet-regexp #px"" ;; intercept all requests
   (lambda (request)
     (run-lab lab))))


(check-names (the-lab))

;; WHAT SHOULD WE ACTUALLY RUN?
#;(go 
 #;show-off-example
 (the-lab))


#;(require (file "/Users/clements/trac-webide/labs/validate-lib.rkt"))
#;(validate-sxml (the-lab))
