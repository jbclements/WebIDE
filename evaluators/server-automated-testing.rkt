#lang racket

(require "transport.rkt"
         "shared.rkt"
         rackunit)


;; TESTING:

;; JAVA EVALUATOR EXAMPLE:

(define test-success-msg "success message\"\r\n\r\n\r\n  \" htns")

(define sample-args 
  `((smessage . ,test-success-msg)
    (functionCall . "assignGroup(18)")
    (expectedOutput . "'C'")
    (function . "public char assignGroup(int age) { char group = 'x'; @groupC return group;} ")))

(define success-response
  (make-immutable-hasheq `((status . "success") (message . ,test-success-msg))))
(define (fail-response? r)
  (equal? (hash-ref r 'status) "failure"))

(define amazon-evaluator
    "http://184.73.238.21/webide/evaluators/JavaEvaluator/JavaEvaluator.php")

(define (amazon-success-equal? args textfields)
  (check-equal? (remote-evaluator-call amazon-evaluator args textfields)
                (success)))

(define (check-amazon-fail? args textfields)
  (check-equal? (failure? (remote-evaluator-call amazon-evaluator args textfields))
                #true))

(amazon-success-equal? sample-args '((groupC . "group = 'C';")))
(check-amazon-fail? sample-args '((groupC . "234;")))



(check-equal? (url-alive? "http://www.berkeley.edu/ohhoeuntesuth") #f)

(check-equal? (not (not (url-alive? amazon-evaluator))) #t)

(check-equal? (url-alive? "http://bogo-host-that-doesnt-exist.com/") #f)

(define l-u "http://localhost:8025/")

(check-equal? (url-alive? l-u) #t)
(check-equal? (remote-evaluator-call (string-append l-u "alwaysSucceed") '() '())
              (success))
(check-equal? (remote-evaluator-call (string-append l-u "alwaysSucceed")
                                     '((glorp . "glorg"))
                                     '((frotzle . "dingdong")
                                       (zigzay . "blotz")))
              (success))

;; REGRESSION TESTING ON JAVA HEADER EVALUATOR:
(check-equal? (remote-evaluator-call (string-append l-u "getApproxAgeHeader") '() '())
              #s(serverfail "request must have exactly one text field"))

(check-equal? (remote-evaluator-call (string-append l-u "getApproxAgeHeader") 
                                     '() '((glorple . "foober")))
              #s(failure "This function signature must begin with the word \"public\"."))



#|
(define (pee-test str-a str-b)
  (parsed-exp-equal? (parse-expression str-a) (parse-expression str-b)))

(check-equal? (pee-test "234" "  234 /*oth*/") #t)
(check-equal? (pee-test "234" "  235 /*oth*/") #f)
(check-equal? (pee-test "(2342 + 22)" "2342 + 22") #t)
(check-equal? (pee-test "(2342 + 22)" "2343 + 22") #f)
(check-equal? (pee-test "((x + 34) + 22)" "x+34+22") #t)
(check-equal? (pee-test "((x + 34) + 22)" "x+(34+22)") #f)
(check-equal? (pee-test "((x + 34) + 22)" "y+34+22") #f)

(check-equal? ((pattern->matcher "((x + 34) + 22)") "x+34+22") #t)
(check-equal? ((pattern->matcher "((x + 34) + 22)") "x+35+22") #f)

(check-equal? (c-parser-match '((pattern . "((x + 34) + 22)"))
                              '((frog . "x+34+22")))
              (success))
(check-equal? (failure? (c-parser-match '((pattern . "((x + 34) + 22)"))
                                        '((frog . "x+34+022"))))
              #t)


(check-equal? (extract-1-user-string '((foo . "bar"))) "bar")
(check-exn exn:fail? (lambda ()
                        (extract-1-user-string '((foo . "bar")
                                                 (baz . "quux")))))

(check-equal? (parses-as-addition? "  /* abc */ 3 + // \n 4") #t)
(check-equal? (parses-as-addition? "4 - 6") #f)
(check-equal? (parses-as-addition? "3 + 4 + 6") #f)
(check-equal? (parses-as-addition? "4") #f)
(check-equal? (parses-as-addition? "234 2987") #f)
(check-equal? (parses-as-addition? "09872") #f)
(check-equal? (any-c-addition '() '((frog . "098732")))
              (failure "\"098732\" doesn't parse as the sum of two integers"))


(check-equal? (parses-as-int? "  34") #t)
(check-equal? (parses-as-int? "  a") #f)
(check-equal? (parses-as-int? "  3 // zappa") #t)
(check-equal? (parses-as-int? "  3.4 // zappa") #f)
(check-equal? (parses-as-int? "098273") #f)
|#

(check-equal? (remote-evaluator-call (string-append l-u "any-c-int") 
                                     '() 
                                     '((dc . "224")))
              #s(success))
(check-equal? (remote-evaluator-call (string-append l-u "any-c-int") 
                                     '() 
                                     '((dc . "  224 /* comment */")))
              #s(success))
(check-equal? (remote-evaluator-call (string-append l-u "any-c-int") 
                                     '() 
                                     '((dc . "  224 123")))
              #s(failure "\"  224 123\" doesn't parse as an integer"))





