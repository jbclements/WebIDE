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
                           (w1:content ,@elts)))))
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
   `(pre-evaluator . ,(lambda elts ""))
   
   ;; leave attributes and other elements alone:
   `(@ *preorder* . ,(lambda args args))
   `(*text* . ,(lambda (t a) a))
   `(*default* . ,(lambda (tag . elts) 
                    (cons tag elts)))))

;; return all of the evaluators contained in an element
(define (extract-evaluators elt)
  (map pre-evaluator->evaluator ((sxpath '(// pre-evaluator) '()) `(*TOP* ,elt))))

;; rename a pre-evaluator as an evaluator
(define (pre-evaluator->evaluator elt)
  (match elt
    [`(pre-evaluator (@ . ,attrs) . ,contents)
     `(w1:evaluator (@ . ,attrs) . ,contents)]))

(define (extract-evaluator-names elt)
  (map evaluator->evaluatorName
       ((sxpath '(// pre-evaluator) '()) `(*TOP* ,elt))))

(define (evaluator->evaluatorName elt)
  (match elt
    [`(pre-evaluator (@ . ,attrs) . ,dc)
     `(w1:evaluatorName ,(cadr (assoc 'name attrs)))]))


(check-equal? (extract-evaluator-names '(pre-evaluator (|@| (name "boo"))3 12))
              `((w1:evaluatorName "boo")))


(check-equal? 
 (extract-evaluator-names
  `(foo (pre-evaluator (@ (name "boo")))
        (zoo (pre-evaluator (@ (name "zoo")))
             (pre-evaluator (@ (name "goo"))))))
 `((w1:evaluatorName "boo")
   (w1:evaluatorName "zoo")
   (w1:evaluatorName "goo")))

(check-equal? 
 (evaluator->evaluatorName `(pre-evaluator (@ (foo "zig") (name "brock") )))
 `(w1:evaluatorName "brock"))



(check-equal? (extract-evaluators `(oogy (pre-evaluator (|@| (name "e1"))3 4 5)
                                         (woogy (pre-evaluator
                                                 (|@| (name "e2"))
                                                 6 7 8))))
              `((w1:evaluator (|@| (name "e1")) 3 4 5)
                (w1:evaluator (|@| (name "e2")) 6 7 8)))

(check-equal? (collect-evaluators
               `(*TOP* 
                 (|@|)
                 (w1:lab (pre-step 
                          (buttonregion
                           (pre-evaluator (|@| (name "ec"))))))))
              
               `(*TOP*
                 (|@|)
                 (w1:lab (w1:step (|@|)
                          (w1:evaluator (|@| (name "ec")))
                          (w1:content
                           (w1:div
                            ""
                            (w1:button (|@|)
                                       (w1:evaluatorName "ec"))))))))


(check-equal? (collect-evaluators
               `(*TOP* 
                 (|@|)
                 (w1:lab (pre-step 
                          (foo (buttonregion
                                (zoo (goo (pre-evaluator (|@| (name "ea")) 4 5))
                                     (pre-evaluator (|@| (name "eb")))
                                     (ploo))))
                          (buttonregion
                           (pre-evaluator (|@| (name "ec")))))
                         (pre-step 
                          (buttonregion
                           (pre-evaluator (|@| (name "ed"))))
                          (foo (buttonregion
                                (zoo (goo (pre-evaluator (|@| (name "ef")) 4 5))
                                     (pre-evaluator (|@| (name "eg")))
                                     (ploo))))))))
              
               `(*TOP*
                 (|@|)
                 (w1:lab (w1:step (|@|)
                          (w1:evaluator (|@| (name "ea")) 4 5)
                          (w1:evaluator (|@| (name "eb")))
                          (w1:evaluator (|@| (name "ec")))
                          (w1:content
                           (foo (w1:div
                                 (zoo (goo "")
                                      ""
                                      (ploo))
                                 (w1:button (|@|)
                                            (w1:evaluatorName "ea")
                                            (w1:evaluatorName "eb"))))
                           (w1:div
                            ""
                            (w1:button (|@|)
                                       (w1:evaluatorName "ec")))))
                         (w1:step (|@|)
                          (w1:evaluator (|@| (name "ed")))
                          (w1:evaluator (|@| (name "ef")) 4 5)
                          (w1:evaluator (|@| (name "eg")))
                          (w1:content
                          (w1:div
                           ""
                           (w1:button (|@|)
                                      (w1:evaluatorName "ed")))
                          (foo (w1:div
                                (zoo (goo "")
                                     ""
                                     (ploo))
                                (w1:button (|@|)
                                            (w1:evaluatorName "ef")
                                            (w1:evaluatorName "eg")))))))))




