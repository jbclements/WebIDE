#lang racket

(require "shared.rkt"
         (planet dherman/c:3:2)
         rackunit)


(provide any-c-int)


;; take the text supplied by the user,
;; check that it parses to an int
(define (any-c-int args texts)
  (match texts
    [`((,dc . ,usertext)) 
     (cond [(parses-as-int? usertext) (success)]
           [else (failure (format "~v doesn't parse as an integer"))])]))

(define (parses-as-int? str)
  (match (parse-expression str)
    [(struct expr:int (src value qualifiers)) #t]
    [other #f]))



;; test cases :

(check-equal? (parses-as-int? "  34") #t)
(check-equal? (parses-as-int? "  a") #f)
(check-equal? (parses-as-int? "  3 // zappa") #t)
(check-equal? (parses-as-int? "  3.4 // zappa") #f)

