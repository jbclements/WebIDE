#lang racket

(require (for-syntax syntax/parse)
         "toy-java-lexer.rkt"
         rackunit)

(provide birth-year-example)

;; type-name? : string -> boolean
;; given a string, determines whether it's the name of a Java type. 
;; ** totally incomplete **
(define (type-name? t)
  (member t '("int" "double" "String"))) ;; ... incomplete

;; LET'S DO IT ALL OVER WITH BETTER ABSTRACTION:

(define (make-java-header-checker first-line-spec)
  (match first-line-spec
    [(list return-ty fun-name arg-specs)
     (lambda (first-line)
       (match/fail* (arg-checker arg-specs (split-at-commas args))
         [(tokenize-string first-line) 
          `("public" ,rest ...) 
          "This function signature must begin with the word \"public\"."]
         [rest `(,(? type-name? ty) ,rest ...) "After the word public, you need a type name."]
         [ty (? (equal?to return-ty)) (format "This function's return type must be \"~a\"." return-ty)]
         [rest `(,pre ... "(" ,args ... ")" ,leftover ...) 
               "A function header must contain a pair of parentheses around the argument list."]
         [leftover `() "There shouldn't be anything after the right-paren (\")\")."]
         [pre `(,name) "The function name (a single word) comes between the type of the function and the argument list."]
         [name (? (equal?to fun-name))
               (format "The name of the function should be \"~a\"." fun-name)]))]))

(define ((equal?to a) b)
  (equal? a b))

(define birth-year-example
  (make-java-header-checker `("int"
                              "getApproxAge"
                              (("int" "birthYear")
                               ("int" "curYear")))))

;; arg-checker : (listof (list/c string? string?)) (listof (listof string?)) -> (or/c 'success string?)
;; check all of the given arguments, to make sure they match the arguments specified.
(define (arg-checker specs/in args/in)
  (let loop ([specs specs/in] [args args/in] [index 1])
    (match (list specs args)
      [`(() ()) 'success]
      [`(() (,arg ...)) (format "This function requires only ~s arguments; you provided ~s"
                                (length specs/in)
                                (length args/in))]
      [`((,spec ...) ()) (format "This function requires ~s arguments; you provided only ~s"
                                 (length specs/in)
                                 (length args/in))]
      [`(((,spec-ty ,spec-argname) ,spec-rest ...) ((,arg ...) ,arg-rest ...))
       (let ([nth-word (nth-name index)])
         (match/fail* (loop spec-rest arg-rest (+ index 1))
           [arg `(,arg1ty ,arg1name) 
                (format "The ~a argument should consist of a type and a name (exactly two words)."
                        nth-word)]
           [arg1ty (? type-name? _) (format "The first part of the ~a argument must be a type."
                                            nth-word)]
           [arg1ty (? (equal?to spec-ty)) 
                   (format "The ~a argument should be of type \"~a\"."
                           nth-word
                           spec-ty)]
           [arg1name (? (equal?to spec-argname)) 
                     (format "The name of the ~a argument should be \"~a\"."
                             nth-word
                             spec-argname)]))])))

(define (split-at-commas token-list)
  (match token-list
    [`(,(? not-comma? pre) ... "," ,post ...) (cons pre (split-at-commas post))]
    [other (list other)]))

(define (not-comma? a)
  (not (string=? a ",")))

(define (nth-name idx)
  (format "~s~a"
          idx
          (case (modulo idx 10)
            [(0 4 5 6 7 8 9) "th"]
            [(1) "st"]
            [(2) "nd"]
            [(3) "rd"])))

(check-equal? (nth-name 43) "43rd")
(check-equal? (nth-name 6) "6th")

(check-equal? (split-at-commas (list "a")) (list (list "a")))
(check-equal? (split-at-commas (list "a" "b")) (list (list "a" "b")))
(check-equal? (split-at-commas (list ",")) (list (list) (list)))
(check-equal? (split-at-commas (list "a" "," "b" "c" "," "d")) (list (list "a")
                                                                     (list "b" "c")
                                                                     (list "d")))

;; the match/fail macro.  
;; A use of match/fail contains a single 'success' value
;; followed by a bunch of fail clauses.  Each fail clause
;; matches a value against a pattern and signals the given
;; error if it fails.  If it succeeds, it goes on to the 
;; next clause.  Note that pattern variables bound in each
;; pattern may be used in the remaining clauses.
(define-syntax (match/fail* stx)
  (define-syntax-class match/fail-clause
    #:description "match/fail clause"
    ;; pat:expr *can't* be right here...
    (pattern (val:expr pat:expr fail:expr)))
  
  (syntax-parse stx
    [(_ retval) #'retval]
    [(_ retval:expr clause:match/fail-clause more-clauses ...)
     #`(match clause.val 
         [clause.pat (match/fail* retval more-clauses ...)]
         [fail clause.fail])]))



;; sample interactions, now used as regression tests:
(check-equal? (birth-year-example "int (3 )")
              "This function signature must begin with the word \"public\".")
(check-equal? (birth-year-example "public int(3)")
              "The function name (a single word) comes between the type of the function and the argument list.")
(check-equal? (birth-year-example "public int zebra goes_bananas42 ( 3)")
              "The function name (a single word) comes between the type of the function and the argument list.")
(check-equal? (birth-year-example "public int zebra (3 )")
              "The name of the function should be \"getApproxAge\".")
(check-equal? (birth-year-example "public int getApproxAge( 3 )")
              "The 1st argument should consist of a type and a name (exactly two words).")
(check-equal? (birth-year-example "public int getApproxAge( 3,)")
              "The 1st argument should consist of a type and a name (exactly two words).")
(check-equal? (birth-year-example "public int getApproxAge ( 3 , )")
              "The 1st argument should consist of a type and a name (exactly two words).")
(check-equal? (birth-year-example "public int getApproxAge ( 3 4 , )")
              "The first part of the 1st argument must be a type.")
(check-equal? (birth-year-example "public int getApproxAge ( double 4 , )")
              "The 1st argument should be of type \"int\".")
(check-equal? (birth-year-example "public int getApproxAge ( int 4, )")
              "The name of the 1st argument should be \"birthYear\".")
(check-equal? (birth-year-example "public int getApproxAge ( int birthYear , )")
              "The 2nd argument should consist of a type and a name (exactly two words).")
(check-equal? (birth-year-example "public int getApproxAge ( int birthYear , double )")
              "The 2nd argument should consist of a type and a name (exactly two words).")
(check-equal? (birth-year-example "public int getApproxAge ( int birthYear , double zeb )")
              "The 2nd argument should be of type \"int\".")
(check-equal? (birth-year-example "public int getApproxAge ( int birthYear , int zeb )")
              "The name of the 2nd argument should be \"curYear\".")
(check-equal? (birth-year-example "public int getApproxAge ( int birthYear , int curYear )")
              'success)

