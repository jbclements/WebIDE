#lang racket

;; initial code by Bill Hess, 2011-01-03

(require "cdiff.rkt"
         (planet dherman/c:3:2)
         rackunit)

(check-equal? (same-struct? (parse-expression "4 * 2 ") (parse-expression "4 *2")) #t)
(check-equal? (same-struct? (parse-expression "4 * 2 ") (parse-expression "4")) #f)
(check-equal? (get-src (parse-expression "4 * 2 ")) '#s(src 1 1 0 6 1 5 #f))
(check-equal? (get-src (parse-statement "main();")) '#s(src 1 1 0 8 1 7 #f))
(check-equal? (get-src (car (parse-program "int i = 5;"))) '#s(src 1 1 0 11 1 10 #f))


(check-equal? (c-expr-diff " 4" "4") '())
(check-equal? (c-expr-diff "4.5" " 4.5") '())
(check-equal? (c-expr-diff "'c'" "  'c'") '())
(check-equal? (c-expr-diff "\"hello\"" "\"hello\"") '())
(check-equal? (c-expr-diff " main (2,       6)" "main(2, 6)") '())
(check-equal? (c-expr-diff " myfunc ( 2 /3, x) + 2" "myfunc(2/3, x)+2") '())
(check-equal? (c-expr-diff "stuff.thing" "stuff.thing") '())
(check-equal? (c-expr-diff "x++" "x++") '())
(check-equal? (c-expr-diff "++y" "++y") '())
(check-equal? (c-expr-diff "(int)x" "(int)x") '()) 
(check-equal? (c-expr-diff "(const int)x" "(const int)x") '()) 
(check-equal? (c-expr-diff "(const char*)x" "(const char*)x") '())
(check-equal? (c-expr-diff "(const char[])x" "(const char[])x") '())
;(check-equal? (c-expr-diff "(float (*)(int[]))x" "(float (*)(int[]))x") '()) func ptr needs decls
(check-equal? (c-expr-diff "sizeof(x)" "sizeof(x)") '())
(check-equal? (c-expr-diff "-x" "-x") '())
(check-equal? (c-expr-diff "x*3" " x * 3") '())
(check-equal? (c-expr-diff "x*3+2" "(x*3)+2") '())
(check-equal? (c-expr-diff "x=2" "x=2") '())
(check-equal? (c-expr-diff "doin(), it()" "doin(), it()") '())
(check-equal? (c-expr-diff "x = 3 > 4 ? y : z" "x = 3 > 4 ? y : z") '())
(check-equal? (c-expr-diff "doin(1, 2)" "doin(1, 2, 3)") '(#s(src 12 1 11 13 1 12 #f)))

(check-equal? (c-stmt-diff "x = 5;" "x = 5;") '())
(check-equal? (c-stmt-diff "label: x = 5;" "label: x = 5;") '())
(check-equal? (c-stmt-diff "case 3: x = 5;" "case 3: x = 5;") '())
(check-equal? (c-stmt-diff "default: x = 5;" "default: x = 5;") '())
(check-equal? (c-stmt-diff "{ x = 5; y = 2; }" "{ x = 5; y = 2; }") '())
(check-equal? (c-stmt-diff "if (true) { y = 2.2; } else { y = 1.5; }" 
                           "if (true) { y = 2.2; } else { y = 1.5; }") '())
(check-equal? (c-stmt-diff "switch (x) { case 1: x = 5; break; default: x = 2; }" 
                           "switch (x) { case 1: x = 5; break; default: x = 2; }") '())
(check-equal? (c-stmt-diff "while (doit()) { doin(); it(); }" 
                           "while (doit()) { doin(); it(); }" ) '())
(check-equal? (c-stmt-diff "for (i = 0; i < 10; i++) { print(i); }" 
                           "for (i = 0; i < 10; i++) { print(i); }" ) '())
(check-equal? (c-stmt-diff "{ loop: print(\"hi\"); goto loop; }" 
                           "{ loop: print(\"hi\"); goto loop; }" ) '())
(check-equal? (c-stmt-diff "while (doit()) { doin(); continue; }" 
                           "while (doit()) { doin(); continue; }" ) '())
(check-equal? (c-stmt-diff "return x;" "return x;") '())
(check-equal? (c-stmt-diff ";" ";") '())

(check-equal? (c-program-diff "typedef int time_t;" "typedef int time_t;") '())
(check-equal? (c-program-diff "int x;" "int x;") '())
(check-equal? (c-program-diff "int x = 5;" "int x = 5;") '())
(check-equal? (c-program-diff "int myfunc(int x) { return x*x; }"
                              "int myfunc(int x) { return x*x; }") '())

(check-equal? (ast-diff (parse-expression "4*3") (parse-expression "4*2")) '(#s(src 3 1 2 4 1 3 #f)))
(check-equal? (ast-diff (parse-expression "4  *  3   ") (parse-expression "4*2")) '(#s(src 3 1 2 4 1 3 #f)))

