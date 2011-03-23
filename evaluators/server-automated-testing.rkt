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

;; RACKET EVALUATORS

(define l-u 
  #;"http://localhost:8025"
  #;"http://brinckerhoff.org:8025/")


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
              #s(failure (div "This function signature must begin with the word \"public\".")))




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
              #s(failure (div "couldn't parse input")))

(check-equal? (remote-evaluator-call (string-append l-u "any-c-addition")
                                     '()
                                     '((dc . " 234 /* foo */ + 224")))
              #s(success))
(check-equal? (remote-evaluator-call (string-append l-u "any-c-addition")
                                     '()
                                     '((dc . " 234 /* foo */ + - 224")))
              #s(failure (div "\" 234 /* foo */ + - 224\" doesn't parse as the sum of two integers")))

(check-equal? (remote-evaluator-call (string-append l-u "c-parser-match")
                                     '((pattern . "234"))
                                     '((dc . "234")))
              #s(success))

(check-equal? (remote-evaluator-call (string-append l-u "c-parser-match")
                                     '((pattern . "234"))
                                     '((dc . "2234")))
              #s(failure (div (div (p "It looks like you need to fix the boxed part:") (p (span (@ (style "font-family: monospace;")) (span (@ (style "border: 1px solid rgb(50, 50, 50); background-color : rgb(250,200,200);")) "2234")))))))


(check-equal? (remote-evaluator-call (string-append l-u "c-parser-match")
                                     '((pattern . "f(x)"))
                                     '((dc . "")))
              #s(failure (div "This box is empty.")))

(check-equal? (remote-evaluator-call (string-append l-u "any-c-int")
                                     '()
                                     '((dc . "")))
              #s(failure (div "This box is empty.")))








