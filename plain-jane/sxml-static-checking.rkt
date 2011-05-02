#lang racket

(require (planet clements/sxml2)
         rackunit)

;; verify that buttons refer to real evaluators, and that evaluators refer to real
;; userfields.
(provide check-names)


(define (check-names sxml)
  (define provided-evals (all-provided-evaluator-names sxml))
  (define required-evals (all-required-evaluator-names sxml))
  (define provided-boxes (all-provided-userfield-names sxml))
  (define required-boxes (all-required-userfield-names sxml))
  (define undefined-evaluators 
    (remove* provided-evals required-evals))
  (define unrequired-evaluators
    (remove* required-evals provided-evals))
  (define undefined-userfields
    (remove* provided-boxes required-boxes))
  (define unrequired-userfields
    (remove* required-boxes provided-boxes))
  (unless (empty? undefined-evaluators)
    (error 'check-names "undefined evaluators: ~s" undefined-evaluators))
  (unless (empty? unrequired-evaluators)
    (printf (current-error-port)
            "warning: evaluators ~s defined but not used."
            unrequired-evaluators))
  (unless (empty? undefined-userfields)
    (error 'check-names "undefined userfields: ~s" undefined-evaluators))
  (unless (empty? unrequired-userfields)
    (printf (current-error-port)
            "warning: userfields ~s defined but not used."
            unrequired-userfields)))


(define all-required-evaluator-names
  (sxpath '(// w1:evaluatorName)))

;; haven't defined these yet...
(define (all-provided-evaluator-names sxml) '())
(define (all-required-userfield-names sxml) '())
(define (all-provided-userfield-names sxml) '())

(check-equal? (all-required-evaluator-names '(*TOP* (foo (bar (w1:evaluatorName "zebra"))
                                                         (w1:evaluatorName "bog"))))
              '("zebra" "bog"))