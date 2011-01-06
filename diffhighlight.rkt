#lang racket


;; initial code by Bill Hess, 2011-01-03

(require "cdiff.rkt")

(define (print-code code start end highlight)
  (cond 
    [highlight (printf "$~a$" (substring code (- start 1) (- end 1)))]
    [else      (printf "~a"   (substring code (- start 1) (- end 1)))]))

(define (highlight-segment pos code errors)
  (cond [(empty? errors) (print-code code pos (+ (string-length code) 1) #f)]
        [(missing-code? (car errors))
         (let ([start-pos (missing-code-start (car errors))]
               [end-pos (missing-code-end (car errors))])
           (if (= pos start-pos)
               (begin (printf " <missing> ")
                      (highlight-segment end-pos code (cdr errors)))
               (begin (print-code code pos start-pos #f)
                      (highlight-segment start-pos code errors))))]
        [(src? (car errors)) 
         (let ([start-pos (src-start-offset (car errors))]
               [end-pos (src-end-offset (car errors))])
           (if (= pos start-pos)
               (begin (print-code code pos end-pos #t)
                      (highlight-segment end-pos code (cdr errors)))
               (begin (print-code code pos start-pos #f)
                      (highlight-segment start-pos code errors))))]
        [else (begin (printf "???") (highlight-segment pos code (cdr errors)))]
        ))

(define (highlight-diff one two)
  (begin (highlight-segment 1 two (c-diff one two))
         (printf "~n")))


(define problem1 "dist*dist + 4")

(printf "-- ~a~n" problem1)
(highlight-diff problem1 "(dist * dist) + 4")
(highlight-diff problem1 "dist^2 + 4")
(highlight-diff problem1 "dist**2 + 4")
(highlight-diff problem1 "pow(dist, 2) + 4")
(highlight-diff problem1 "dits*dist + 4")
(highlight-diff problem1 "dist*dist + four")
(highlight-diff problem1 "dist*dist")


(define problem2 (list "g(6, 9) + 1" "1 + g(6, 9)"))

(printf "~n-- ~a~n" problem2)
(highlight-diff problem2 "g (6 , 9)+1")
(highlight-diff problem2 "g(6 9) + 1")
(highlight-diff problem2 "g69 + 1")
(highlight-diff problem2 "g(6, 9)")
(highlight-diff problem2 "g(6, 9)++")
(highlight-diff problem2 "g(6, 9) ++ 1")
(highlight-diff problem2 "1 + g(6, 9)")


(define problem3 (list "j = j + 1" "j++" "++j" "j = j + 1;" "j++;" "++j;" "j += 1;"))

(printf "~n-- ~a~n" problem3)
(highlight-diff problem3 "j++")
(highlight-diff problem3 "j++;")
(highlight-diff problem3 "j = j + 1")
(highlight-diff problem3 "j = j + 1;")
(highlight-diff problem3 "++j")
(highlight-diff problem3 "++j;")
(highlight-diff problem3 "j += 1")
(highlight-diff problem3 "j + 1")
(highlight-diff problem3 "j = 1;")
(highlight-diff problem3 "j ++ 1;")
(highlight-diff problem3 "j =+ 1;")
(highlight-diff problem3 "j ==+ 1;")

; Make sure garbage doesn't make anything blow up
(highlight-diff problem3 "stuff /* asdasd")
(highlight-diff problem3 "stuff \" asdasd")

;(highlight-diff "int i = 5;" 
;                "char i = 6;")
;(highlight-diff "x * y" 
;                "x + (x + y)")
;(highlight-diff "int myFunc (x) { return x+x; }" 
;                "int myFunc(x){return x; }")
;(highlight-diff "int func(x) { while(true) { print(x*2); } }"
;                "char func(x) { while(false) { print(x*2); } }")
;
;(highlight-diff-program "
;int var = 6;
;
;int main()
;{
;  print(\"hello\");
;  for (int i = 0; i < var; i++) {
;    print(i);
;  }
;  
;  return 0;
;}
;" 
;"
;int var = 6;
;
;int main()
;{
;  print(\"hello\");
;  for (int i = 0; i < var; i++) {
;    print(i);
;  }
;  
;  return 0;
;}
;")