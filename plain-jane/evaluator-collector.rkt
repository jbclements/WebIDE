#lang racket

(require (planet clements/sxml2)
         "apat.rkt"
         rackunit)

(provide collect-evaluators)

;; collect evaluators; put the names in the corresponding buttons, and the
;; steps at the top level within the evaluators.

;; turn a step into html
(define (collect-evaluators content)
  (pre-post-order content evaluator-lifter-stylesheet))

;; collect all of the evaluators occurring in a step, lift them 
;; to the top level
(define evaluator-lifter-stylesheet
  (list 
   ;; before processing the step's elements, pull up all of 
   ;; the evaluators.
   `(pre-step *macro* . 
             ,(standardize/shallow
               (lambda (tag attrs . elts)
                 `(w1:step ,attrs 
                           ,@(apply append (map extract-evaluators elts))
                           ,@elts))))
   ;; before processing the button's elements, pull up all of the
   ;; evaluators' names, and add a button.
   `(buttonregion *macro* .
                  ,(standardize/shallow
                    (lambda (tag attrs . elts)
                      `(w1:div
                        ,@elts
                        (w1:button ,attrs 
                                   ,@(apply
                                      append
                                      (map extract-evaluator-names elts)))))))
   ;; discard the evaluator; it's already been gathered
   `(w1:evaluator . ,(lambda elts ""))
   
   ;; leave attributes and other elements alone:
   `(@ *preorder* . ,(lambda args args))
   `(*text* . ,(lambda (t a) a))
   `(*default* . ,(lambda (tag . elts) 
                    (cons tag elts)))))

;; return all of the evaluators contained in an element
(define (extract-evaluators elt)
  ((sxpath '(// w1:evaluator) '()) elt))


(define (extract-evaluator-names elt)
  (map evaluator->evaluatorName
       ((sxpath '(// w1:evaluator) '()) elt)))

(define (evaluator->evaluatorName elt)
  (match elt
    [`(w1:evaluator (@ . ,attrs) . ,dc)
     `(w1:evaluatorName ,(cadr (assoc 'name attrs)))]))



(check-equal? 
 (extract-evaluator-names
  `(foo (w1:evaluator (@ (name "boo")))
        (zoo (w1:evaluator (@ (name "zoo")))
             (w1:evaluator (@ (name "goo"))))))
 `((w1:evaluatorName "boo")
   (w1:evaluatorName "zoo")
   (w1:evaluatorName "goo")))

(check-equal? 
 (evaluator->evaluatorName `(w1:evaluator (@ (foo "zig") (name "brock") )))
 `(w1:evaluatorName "brock"))



(check-equal? (extract-evaluators `(oogy (w1:evaluator 3 4 5)
                                         (woogy (w1:evaluator 6 7 8))))
              `((w1:evaluator 3 4 5)
                (w1:evaluator 6 7 8)))

