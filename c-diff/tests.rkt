#lang racket

;; initial code by Bill Hess, 2011-01-03

(require "cdiff.rkt"
         rackunit)

;(check-expect (c-expr-diff " 4" "4") '())
;(check-expect (c-expr-diff "4.5" " 4.5") '())
;(check-expect (c-expr-diff "'c'" "  'c'") '())
;(check-expect (c-expr-diff "\"hello\"" "\"hello\"") '())
;(check-expect (c-expr-diff " main (2,       6)" "main(2, 6)") '())
;(check-expect (c-expr-diff " myfunc ( 2 /3, x) + 2" "myfunc(2/3, x)+2") '())
;(check-expect (c-expr-diff "stuff.thing" "stuff.thing") '())
;(check-expect (c-expr-diff "x++" "x++") '())
;(check-expect (c-expr-diff "++y" "++y") '())
;(check-expect (c-expr-diff "(int)x" "(int)x") '()) 
;(check-expect (c-expr-diff "(const int)x" "(const int)x") '()) 
;(check-expect (c-expr-diff "(const char*)x" "(const char*)x") '())
;(check-expect (c-expr-diff "(const char[])x" "(const char[])x") '())
;;(check-expect (c-expr-diff "(float (*)(int[]))x" "(float (*)(int[]))x") '()) func ptr needs decls
;(check-expect (c-expr-diff "sizeof(x)" "sizeof(x)") '())
;(check-expect (c-expr-diff "-x" "-x") '())
;(check-expect (c-expr-diff "x*3" " x * 3") '())
;(check-expect (c-expr-diff "x*3+2" "(x*3)+2") '())
;(check-expect (c-expr-diff "x=2" "x=2") '())
;(check-expect (c-expr-diff "doin(), it()" "doin(), it()") '())
;(check-expect (c-expr-diff "x = 3 > 4 ? y : z" "x = 3 > 4 ? y : z") '())
;(check-expect (c-expr-diff "doin(1, 2)" "doin(1, 2, 3)") '(#s(src 12 1 11 13 1 12 #f)))
;
;(check-expect (c-stmt-diff "x = 5;" "x = 5;") '())
;(check-expect (c-stmt-diff "label: x = 5;" "label: x = 5;") '())
;(check-expect (c-stmt-diff "case 3: x = 5;" "case 3: x = 5;") '())
;(check-expect (c-stmt-diff "default: x = 5;" "default: x = 5;") '())
;(check-expect (c-stmt-diff "{ x = 5; y = 2; }" "{ x = 5; y = 2; }") '())
;(check-expect (c-stmt-diff "if (true) { y = 2.2; } else { y = 1.5; }" 
;                           "if (true) { y = 2.2; } else { y = 1.5; }") '())
;(check-expect (c-stmt-diff "switch (x) { case 1: x = 5; break; default: x = 2; }" 
;                           "switch (x) { case 1: x = 5; break; default: x = 2; }") '())
;(check-expect (c-stmt-diff "while (doit()) { doin(); it(); }" 
;                           "while (doit()) { doin(); it(); }" ) '())
;(check-expect (c-stmt-diff "for (i = 0; i < 10; i++) { print(i); }" 
;                           "for (i = 0; i < 10; i++) { print(i); }" ) '())
;(check-expect (c-stmt-diff "{ loop: print(\"hi\"); goto loop; }" 
;                           "{ loop: print(\"hi\"); goto loop; }" ) '())
;(check-expect (c-stmt-diff "while (doit()) { doin(); continue; }" 
;                           "while (doit()) { doin(); continue; }" ) '())
;(check-expect (c-stmt-diff "return x;" "return x;") '())
;(check-expect (c-stmt-diff ";" ";") '())
;
;(check-expect (c-program-diff "typedef int time_t;" "typedef int time_t;") '())
;(check-expect (c-program-diff "int x;" "int x;") '())
;(check-expect (c-program-diff "int x = 5;" "int x = 5;") '())
;(check-expect (c-program-diff "int myfunc(int x) { return x*x; }"
;                              "int myfunc(int x) { return x*x; }") '())

;(check-expect (ast-diff (parse-expression "4*3") (parse-expression "4*2")) '(#s(src 3 1 2 4 1 3 #f)))
;(check-expect (ast-diff (parse-expression "4  *  3   ") (parse-expression "4*2")) '(#s(src 3 1 2 4 1 3 #f)))

;(test)