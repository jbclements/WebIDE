#lang racket


(require rackunit
         "evaluator-collector.rkt")

;; utility functions for defining labs:

(provide (all-defined-out))

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

;; string content ... -> sxml-document
(define (lab name . content) 
  (collect-evaluators
   `(*TOP*
     (|@| (*NAMESPACES* (w1 "http://www.web-ide.org/namespaces/labs/2")))
     (w1:lab (|@| (name ,name))
             ,@content))))

(define (step name 
             #:dependencies [dependencies `()]
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



(define (intbox str)
  (only-regexp "[+-]?[0-9]+"))



(define-tag-helper p)
(define-tag-helper code)

;; We'll clean these up in a pass over the whole body
(define (buttonregion #:label [label "go"] . elts) 
  `(buttonregion (|@| (label ,label)) ,@elts))

;; in that case, "box" should just emit an evaluator element to the collector, and
;; spit out a fresh textfield. This only works for single-box evaluators.
(define (box url args)
  (let ([new-textfield-id (next-textfield-id)]
        [new-evaluator-id (next-evaluator-id)])
    `(w1:div
      (pre-evaluator 
       (|@| (href ,url)
            (name ,new-evaluator-id))
       ,@(map arg->eval-arg args)
       (w1:userfieldArg (w1:name "userText") 
                        (w1:value ,new-textfield-id)))
      (w1:userfield (|@| (id ,new-textfield-id))))))

(define (arg->eval-arg pr)
  (match pr
    [(list a b)
     `(w1:fixedArg (w1:name ,(symbol->string a)) (w1:value ,b))]))

;; GENERATE FRESH TEXTFIELD-IDs and EVALUATOR-IDs

(define textfield-ctr 0)
(define (next-textfield-id)
  (set! textfield-ctr (+ textfield-ctr 1))
  (format "text-field-~v" textfield-ctr))

(define evaluator-ctr 0)
(define (next-evaluator-id)
  (set! evaluator-ctr (+ evaluator-ctr 1))
  (format "evaluator-~v" evaluator-ctr))

(define bogusbox 
  (box "this is not a url" '()))

;; a box with a button right next to it:
(define (box&button url args)
  (buttonregion (box url args)))

