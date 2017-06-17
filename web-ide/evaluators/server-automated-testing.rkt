#lang racket

(require "transport.rkt"
         "shared.rkt"
         rackunit
         rackunit/text-ui)


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
  #;"http://localhost:59475/"
  "http://184.73.238.21/webide/evaluators/JavaEvaluator/JavaEvaluator.php")

(define (amazon-success-equal? args textfields)
  (check-equal? (remote-evaluator-call amazon-evaluator args textfields)
                (success)))

(define (check-amazon-fail? args textfields)
  (check-equal? (failure? (remote-evaluator-call amazon-evaluator args textfields))
                #true))

;; looks like amazon evaluators are not working??
#;(time (amazon-success-equal? sample-args '((groupC . "group = 'C';"))))
#;(time (check-amazon-fail? sample-args '((groupC . "234;"))))


(define (n-times n thunk)
  (define times
    (for/list ([i (in-range n)])
      (let-values ([(result cpu total gc) (time-apply thunk '())])
        total)))
  (list (mean times)
        (sort times <)))


(define (mean l) (/ (apply + l) (length l)))

#;(n-times 10 
         (lambda ()
           (amazon-success-equal? sample-args '((groupC . "group = 'C';")))))

(run-tests
 (test-suite
  "amazon tests"
(check-equal? (url-alive? "http://www.berkeley.edu/ohhoeuntesuth") #f)

(test-equal?
 "amazon evaluator alive"
 (time (not (not (url-alive? amazon-evaluator)))) #t)

(check-equal? (url-alive? "http://bogo-host-that-doesnt-exist.com/") #f)))

(printf "finished testing Java evaluators\n")

;; RACKET EVALUATORS

(run-tests
(test-suite
 "racket evaluator tests"
 (let ()
   (define l-u 
     ;; test locally:
     #;"http://localhost:8025"
     ;; test brinckerhoff.org (whatever it points to)
     "http://brinckerhoff.org:8025/")

   (printf "running tests against host: ~v\n"
           l-u)

   



(check-equal? (url-alive? l-u) #t)
(check-equal? (time (remote-evaluator-call (string-append l-u "alwaysSucceed") '() '()))
              (success))
(check-equal? (time (remote-evaluator-call (string-append l-u "alwaysSucceed")
                                     '((glorp . "glorg"))
                                     '((frotzle . "dingdong")
                                       (zigzay . "blotz"))))
              (success))

;; REGRESSION TESTING ON JAVA HEADER EVALUATOR:
(check-equal? (time (remote-evaluator-call (string-append l-u "getApproxAgeHeader") '() '()))
              #s(callerfail "bad arguments or user texts for path \"getApproxAgeHeader\" in: () and ()"))

(check-equal? (remote-evaluator-call (string-append l-u "getApproxAgeHeader") 
                                     '() '((glorple . "foober")))
              #s(failure (div "This function signature must begin with the word \"public\".")))




(check-equal? (remote-evaluator-call (string-append l-u "any-c-int") 
                                     '() 
                                     '((dc . "224")))
              (success))
(check-equal? (remote-evaluator-call (string-append l-u "any-c-int") 
                                     '() 
                                     '((dc . "  224 /* comment */")))
              (success))
(check-equal? (remote-evaluator-call (string-append l-u "any-c-int") 
                                     '() 
                                     '((dc . "  224 123")))
              #s(failure (div (div (p "#f:1:6: parse: unexpected integer literal\n  in: 123") (p (span (@ (style "font-family: monospace;")) "  224 " (span (@ (style "border: 1px solid rgb(50, 50, 50); background-color : rgb(250,200,200);")) "123")))))))

(check-equal? (remote-evaluator-call (string-append l-u "any-c-addition")
                                     '()
                                     '((dc . " 234 /* foo */ + 224")))
              (success))
(check-equal? (remote-evaluator-call (string-append l-u "any-c-addition")
                                     '()
                                     '((dc . " 234 /* foo */ + - 224")))
              #s(failure (div "\" 234 /* foo */ + - 224\" doesn't parse as the sum of two integers")))

(check-equal? (remote-evaluator-call (string-append l-u "c-parser-match")
                                     '((pattern . "234"))
                                     '((dc . "234")))
              (success))

(check-equal? (remote-evaluator-call (string-append l-u "c-parser-match")
                                     '((pattern . "234"))
                                     '((dc . "2234")))
              #s(failure (div (div (p "It looks like you need to fix the boxed part:") (p (span (@ (style "font-family: monospace;")) (span (@ (style "border: 1px solid rgb(50, 50, 50); background-color : rgb(250,200,200);")) "2234")))))))


(check-equal? (remote-evaluator-call (string-append l-u "c-parser-match")
                                     '((pattern . "f(x)"))
                                     '((dc . "")))
              #s(failure (div "This box is empty.")))


(check-equal? (remote-evaluator-call (string-append l-u "c-stmt-parser-match")
                                     '((pattern . "if (3 < 4) { return 4; } else {return 2;}"))
                                     '((frog . "if    ((3 < 4)) {return 4;} else {return 2;}"))) 
              (success))


(check-equal? (remote-evaluator-call (string-append l-u "c-stmt-parser-match")
                                     '((pattern . "for (3;4;5) 6;"))
                                     '((frog . "for (3;;5) 6;")))
              #s(failure (div (div (p "This 'for' is missing its test expression:")
                                   (p (span (@ (style "font-family: monospace;")) 
                                            (span 
                                             (@ (style "border: 1px solid rgb(50, 50, 50); background-color : rgb(250,200,200);"))
                                             "for (3;;5) 6;")))))))




(check-equal? (remote-evaluator-call (string-append l-u "any-c-int")
                                     '()
                                     '((dc . "")))
              #s(failure (div "This box is empty.")))


;; what happens on unparsable patterns?

(check-equal? (remote-evaluator-call (string-append l-u "c-stmt-parser-match")
                                     '((pattern . "definitely not a legal c program"))
                                     '((frog . "324")))
              #s(callerfail "problem while parsing pattern: \"#f:1:11: parse: unexpected identifier (perhaps missing a typedef declaration?)\\n  in: not\" on \"definitely not a legal c program\" at (not)"))


(check-equal? (remote-evaluator-call (string-append l-u "c-parser-match")
                                     '((pattern . "3 || 4"))
                                     '((frog . "(3 || 4)")))
              #s(success)))))


