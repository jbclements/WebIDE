#lang racket

(require "shared.rkt"
         (planet dherman/c:3:2)
         rackunit)


(provide any-c-int
         any-c-addition)


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


;; abstraction needed here...
(define (any-c-addition args texts)
  (match texts
    [`((,dc . ,usertext)) 
     (cond [(parses-as-addition? usertext) (success)]
           [else (failure (format "~v doesn't parse as the sum of two integers"))])]))

;; does this string parse as (+ (int) (int)) ?
;; what to do on a parse error?
(define (parses-as-addition? str)
  (match (parse-expression str)
    [(struct expr:binop (src_1 (struct expr:int (dc_1 dc_2 dc_3)) 
                               (struct id:op (src2 '+))
                               (struct expr:int (dc_4 dc_5 dc_6)))) 
     #t]
    [other #f]))

(check-equal? (parses-as-addition? "  /* abc */ 3 + // \n 4") #t)
(check-equal? (parses-as-addition? "4 - 6") #f)
(check-equal? (parses-as-addition? "3 + 4 + 6") #f)
(check-equal? (parses-as-addition? "4") #f)



;; test cases :

(check-equal? (parses-as-int? "  34") #t)
(check-equal? (parses-as-int? "  a") #f)
(check-equal? (parses-as-int? "  3 // zappa") #t)
(check-equal? (parses-as-int? "  3.4 // zappa") #f)

