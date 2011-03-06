#lang racket

(require (planet dherman/java/syntax/ast)
         (planet dherman/java/syntax/parser)
         rackunit)


(define expr-wrap-strings
  (list "class BogusClass{
  int bogusMethod(){ return 
""
;
}}
"))

(define stmt-wrap-strings
  (list "class BogusClass{
  int bogusMethod(){
""
}}
"))

;; parsed file -> parsed method-decl
(define (strip-off-class parsed-file)
  (match (compilation-unit-classes parsed-file)
  [(list the-class-decl)
   (match (type-decl-body the-class-decl)
     [(list the-method-decl)
      the-method-decl])]))

;; parsed method decl -> stmt list
(define (strip-off-method parsed-method)
  (block-stmt-body 
   (behavior-decl-body parsed-method)))

;; stmt list -> expr
(define (strip-off-return parsed-stmt-list)
  (match parsed-stmt-list
    [(list the-return-stmt)
     (return-stmt-value the-return-stmt)]))

(define (expr-string->parsed str)
  (strip-off-return
   (strip-off-method
    (strip-off-class
     (parse-string (string-append (first expr-wrap-strings)
                                  str
                                  (second expr-wrap-strings)))))))

(match (expr-string->parsed "14 + 34")
  [(struct binary-expr (_1 _2 '+ (struct integer-literal (_3 14))
                           (struct integer-literal (_4 34))))
   #t])
