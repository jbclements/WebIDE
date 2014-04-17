#lang racket

;; copyright 2010-2014 John Clements (clements@racket-lang.org)

;; work in progress, apparently.

#;(
(require rackunit
         "evaluator-collector-v1.rkt")

(define namespace-identifier "http://www.web-ide.org/namespaces/labs/1")

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
     (|@| (*NAMESPACES* (w1 ,namespace-identifier)))
     (w1:lab (|@| (name ,name))
             ,@content))))

(define (step name 
             #:dependencies [dependencies `()]
             . content)
  `(pre-step (|@| (name ,name)) ,@(par-break content)))
(define (h3 . content) (cons 'w1:h3 content))

;; break a step into paragraphs at blank lines:
(define (par-break content-list)
  (double-nl->break content-list))


(define (double-nl->break l)
  (let loop ([remaining l])
    (match remaining
      [`() (list (reverse so-far))]
      [`("\n" "\n" . ,r) (cons '(br) "\n" '(br) "\n" (loop r))]
      [`(,f . ,r) (cons f (loop r))])))


;; remove empty lists from a list of lists
(define (discard-empties l)
  (filter (lambda (x) (not (empty? x))) l))

;; prepend the symbol 'w1:p to a list
(define (par-wrap elts) (cons 'w1:p elts))


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
            (name ,new-evaluator-id)
            ;; added for v1
            (labid "bogo"))
       ,@(map arg->eval-arg args)
       (w1:segid (w1:id ,new-textfield-id)))
      (w1:segment (|@| (id ,new-textfield-id))))))

(define (arg->eval-arg pr)
  (match pr
    [(list a b)
     `(w1:arg (w1:name ,(symbol->string a)) (w1:value ,b))]))

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


;; TESTING


;; blurg... the existing behavior is probably good enough, but I can't write 
;; a test case for it in good conscience...
#;(check-equal? (split-at-double-newlines '(a "\n" b "\n" "\n" d "\n" e "\n" "\n" d "\n" "\n" "\n" e
                                            "\n" "\n" "\n"))
              '((a "\n" b "\n") (d "\n" e "\n") (d "\n") () ("\n" e "\n") () ()))

(check-equal? (par-break 
               '("\n" "abc" "\n" "def" "\n" "\n" "ghi" "\n" "\n" "\n" "jkl" "\n" "\n"))
              '("\n" "abc"  "\n" "def" "\n" "\n" "ghi" "\n" "\n" "\n" "jkl" "\n" "\n")))
