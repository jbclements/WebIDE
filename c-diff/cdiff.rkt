#lang racket

;; initial code by Bill Hess, 2011-01-03

(require (planet dherman/c:3:2)
         "lex.rkt")

(provide c-program-diff
         c-expr-diff
         c-stmt-diff
         ast-diff
         src-start-offset
         src-end-offset
         src?
         ;; added to allow tests:
         same-struct?
         get-src)

(provide/contract 
 [c-diff (->
          (or/c string? (listof string?))
          string?
          any/c)])

(provide c-lex-diff 
         missing-code
         missing-code?
         missing-code-start
         missing-code-end)

(define same-struct? (lambda (a b)
  (equal? (let-values ([(type c) (struct-info a)]) type)
          (let-values ([(type c) (struct-info b)]) type))))


(define get-src (lambda (ast)
  (match ast
    [(struct id (src)) src]
    [(struct expr (src)) src]
    [(struct stmt (src)) src]
    [(struct decl (src)) src]
    [(struct type (src)) src]
    [(struct init (src)) src]
    [_ 'missing-get-src])))

(define ast-diff (lambda (one two)
  (cond [(and (false? one) (false? two)) '()]
        [(and (false? one) (not (false? two))) (list (get-src two))]
        [(and (false? two) (not (false? one))) '(#f)] ; signal to upper call we have no source for this diff
        [(not (same-struct? one two)) (list (get-src two))]
        [(id? one) (id-diff one two)]
        [(expr? one) (expr-diff one two)]
        [(type? one) (type-diff one two)]
        [(stmt? one) (stmt-diff one two)]
        [(decl? one) (decl-diff one two)]
        [(init? one) (init-diff one two)]
        [else '(1)])))

(define multimap (lambda (func list1 list2)
  (cond
    [(or (and (false? list1) (false? list2)) (and (empty? list1) (empty? list2))) '()]
    [(or (false? list1) (empty? list1)) (map (lambda (x) (func #f x)) list2)]
    [(or (false? list2) (empty? list2)) (map (lambda (x) (func x #f)) list1)]
    [else (cons (func (car list1) (car list2)) (multimap func (cdr list1) (cdr list2)))])))

(define ast-diff-list (lambda (list1 list2) ; when using this function, check for getting a #f from ast-diff
  (if (or list1 list2) (foldl append '() (multimap ast-diff list1 list2)) '())))

(define id-diff (lambda (one two)
  (let ([src-neq (lambda (x y) (if (equal? x y) '() (list (get-src two))))])
    (match (list one two)
      [(list (struct id:var (_ name1)) (struct id:var (_ name2))) (src-neq name1 name2)]
      [(list (struct id:label (_ name1)) (struct id:label (_ name2))) (src-neq name1 name2)]
      [(list (struct id:qualifier (_ name1)) (struct id:qualifier (_ name2))) (src-neq name1 name2)]
      [(list (struct id:op (_ name1)) (struct id:op (_ name2))) (src-neq name1 name2)]
      [(list (struct id:storage (_ class1)) (struct id:storage (_ class2))) (src-neq class1 class2)]
      [_ '(3)]))))

(define expr-diff (lambda (one two)
  (let ([src-neq (lambda (x y) (if (equal? x y) '() (list (get-src two))))]
        [check-list (lambda (x) (if (member #f x) (list (get-src two)) x))])
    (match (list one two)
      [(list (struct expr:ref (_ id1)) (struct expr:ref (_ id2))) (ast-diff id1 id2)]
      [(list (struct expr:int (_ val1 _)) (struct expr:int (_ val2 _))) (src-neq val1 val2)]
      [(list (struct expr:float (_ val1 _)) (struct expr:float (_ val2 _))) (src-neq val1 val2)]
      [(list (struct expr:char (_ val1 _)) (struct expr:char (_ val2 _))) (src-neq val1 val2)]
      [(list (struct expr:string (_ val1 _)) (struct expr:string (_ val2 _))) (src-neq val1 val2)]
      [(list (struct expr:compound (_ val1 _)) (struct expr:compound (_ val2 _))) '()] ; not implemented
      [(list (struct expr:array-ref (_ array1 offset1))
             (struct expr:array-ref (_ array2 offset2)))
       (append (ast-diff array1 array2) (ast-diff offset1 offset2))]
      [(list (struct expr:call (_ fun1 args1)) (struct expr:call (_ fun2 args2))) 
       (check-list (append (ast-diff fun1 fun2) (ast-diff-list args1 args2)))]
      [(list (struct expr:member (_ expr1 label1)) (struct expr:member (_ expr2 label2)))
       (append (ast-diff expr1 expr2) (ast-diff label1 label2))]
      [(list (struct expr:pointer-member (_ expr1 label1)) (struct expr:pointer-member (_ expr2 label2)))
       (append (ast-diff expr1 expr2) (ast-diff label1 label2))]
      [(list (struct expr:postfix (_ expr1 op1)) (struct expr:postfix (_ expr2 op2)))
       (append (ast-diff expr1 expr2) (ast-diff op1 op2))]
      [(list (struct expr:prefix (_ expr1 op1)) (struct expr:prefix (_ expr2 op2)))
       (append (ast-diff expr1 expr2) (ast-diff op1 op2))]
      [(list (struct expr:cast (_ type1 expr1)) (struct expr:cast (_ type2 expr2)))
       (append (ast-diff type1 type2) (ast-diff expr1 expr2))]
      [(list (struct expr:sizeof (_ term1)) (struct expr:sizeof (_ term2))) (ast-diff term1 term2)]
      [(list (struct expr:unop (_ op1 expr1)) (struct expr:unop (_ op2 expr2)))
       (append (ast-diff op1 op2) (ast-diff expr1 expr2))]
      [(list (struct expr:binop (_ left1 op1 right1)) 
             (struct expr:binop (_ left2 op2 right2))) 
       (append (ast-diff left1 left2) (ast-diff op1 op2) (ast-diff right1 right2))]
      [(list (struct expr:assign (_ left1 op1 right1)) 
             (struct expr:assign (_ left2 op2 right2))) 
       (append (ast-diff left1 left2) (ast-diff op1 op2) (ast-diff right1 right2))]
      [(list (struct expr:begin (_ left1 right1)) 
             (struct expr:begin (_ left2 right2))) 
       (append (ast-diff left1 left2) (ast-diff right1 right2))]
      [(list (struct expr:if (_ test1 cons1 alt1)) 
             (struct expr:if (_ test2 cons2 alt2))) 
       (append (ast-diff test1 test2) (ast-diff cons1 cons2) (ast-diff alt1 alt2))]
      [_ (list 4 one)]
      ))))

(define type-diff (lambda (one two)
  (let ([src-neq (lambda (x y) (if (equal? x y) '() (list (get-src two))))]
        [check-list (lambda (x) (if (member #f x) (list (get-src two)) x))])
    (match (list one two)
      [(list (struct type:primitive (_ name1)) (struct type:primitive (_ name2))) (src-neq name1 name2)]
      [(list (struct type:ref (_ id1)) (struct type:ref (_ id2))) (ast-diff id1 id2)]
      [(list (struct type:struct (_ tag1 variants1)) (struct type:struct (_ tag2 variants2)))
       (check-list (append (ast-diff tag1 tag2) (ast-diff-list variants1 variants2)))] ; needs decl
      [(list (struct type:union (_ tag1 variants1)) (struct type:union (_ tag2 variants2)))
       (check-list (append (ast-diff tag1 tag2) (ast-diff-list variants1 variants2)))] ; needs decl
      ; enum not implemented
      [(list (struct type:array (_ base1 static1 qualifiers1 length1 star1))
             (struct type:array (_ base2 static2 qualifiers2 length2 star2)))
       (check-list (append (ast-diff base1 base2) 
                             (ast-diff static1 static2) 
                             (ast-diff-list qualifiers1 qualifiers2) 
                             (ast-diff length1 length2) 
                             (ast-diff star1 star2)))]
      [(list (struct type:pointer (_ base1 qualifiers1)) (struct type:pointer (_ base2 qualifiers2)))
       (check-list (append (ast-diff base1 base2) (ast-diff-list qualifiers1 qualifiers2)))]
      [(list (struct type:function (_ ret1 formals1)) (struct type:function (_ ret2 formals2)))
       (check-list (append (ast-diff ret1 ret2) (ast-diff-list formals1 formals2)))]
      [(list (struct type:qualified (_ type1 qualifiers1)) (struct type:qualified (_ type2 qualifiers2)))
       (check-list (append (ast-diff type1 type2) (ast-diff-list qualifiers1 qualifiers2)))]
      [_ '(6)]
      ))))


(define stmt-diff (lambda (one two)
  (let ([src-neq (lambda (x y) (if (equal? x y) '() (list (get-src two))))]
        [check-list (lambda (x) (if (member #f x) (list (get-src two)) x))])
    (match (list one two)
      [(list (struct stmt:label (_ label1 stmt1)) (struct stmt:label (_ label2 stmt2)))
       (append (ast-diff label1 label2) (ast-diff stmt1 stmt2))]
      [(list (struct stmt:case (_ expr1 stmt1)) (struct stmt:case (_ expr2 stmt2)))
       (append (ast-diff expr1 expr2) (ast-diff stmt1 stmt2))]
      [(list (struct stmt:default (_ stmt1)) (struct stmt:default (_ stmt2))) (ast-diff stmt1 stmt2)]
      [(list (struct stmt:block (_ stmts1)) (struct stmt:block (_ stmts2))) 
       (check-list (ast-diff-list stmts1 stmts2))]
      [(list (struct stmt:expr (_ expr1)) (struct stmt:expr (_ expr2))) (ast-diff expr1 expr2)]
      [(list (struct stmt:if (_ test1 cons1 alt1)) 
             (struct stmt:if (_ test2 cons2 alt2))) 
       (append (ast-diff test1 test2) (ast-diff cons1 cons2) (ast-diff alt1 alt2))]
      [(list (struct stmt:switch (_ expr1 stmt1)) (struct stmt:switch (_ expr2 stmt2)))
       (append (ast-diff expr1 expr2) (ast-diff stmt1 stmt2))]
      [(list (struct stmt:while (_ expr1 stmt1)) (struct stmt:while (_ expr2 stmt2)))
       (append (ast-diff expr1 expr2) (ast-diff stmt1 stmt2))]
      [(list (struct stmt:do (_ stmt1 expr1)) (struct stmt:do (_ stmt2 expr2)))
       (append (ast-diff stmt1 stmt2) (ast-diff expr1 expr2))]
      [(list (struct stmt:for (_ init1 test1 update1 body1)) (struct stmt:for (_ init2 test2 update2 body2)))
       (append (ast-diff init1 init2) (ast-diff test1 test2) (ast-diff update1 update2) (ast-diff body1 body2))]
      [(list (struct stmt:goto (_ label1)) (struct stmt:goto (_ label2))) (ast-diff label1 label2)]
      [(list (struct stmt:continue (_)) (struct stmt:continue (_))) '()]
      [(list (struct stmt:break (_)) (struct stmt:break (_))) '()]
      [(list (struct stmt:return (_ ret1)) (struct stmt:return (_ ret2))) (ast-diff ret1 ret2)]
      [(list (struct stmt:empty (_)) (struct stmt:empty (_))) '()]
      [_ (list 8 (let-values ([(type c) (struct-info one)]) type)) ]
      ))))

(define decl-diff (lambda (one two)
  (let ([src-neq (lambda (x y) (if (equal? x y) '() (list (get-src two))))]
        [check-list (lambda (x) (if (member #f x) (list (get-src two)) x))])
    (match (list one two)
      [(list (struct decl:typedef (_ type1 declarators1)) 
             (struct decl:typedef (_ type2 declarators2))) 
       (check-list (append (ast-diff type1 type2) (ast-diff-list declarators1 declarators2)))]
      [(list (struct decl:vars (_ store1 type1 declarators1)) 
             (struct decl:vars (_ store2 type2 declarators2))) 
       (check-list (append  (ast-diff store1 store2) (ast-diff type1 type2) 
                            (ast-diff-list declarators1 declarators2)))]
      [(list (struct decl:formal (_ store1 type1 declarator1)) 
             (struct decl:formal (_ store2 type2 declarator2))) 
       (check-list (append  (ast-diff store1 store2) (ast-diff type1 type2) 
                            (ast-diff declarator1 declarator2)))]
      [(list (struct decl:function (_ store1 inline1 rtype1 declarator1 preamble1 body1)) 
             (struct decl:function (_ store2 inline2 rtype2 declarator2 preamble2 body2)))
       (check-list (append  (ast-diff store1 store2) (ast-diff inline1 inline2) (ast-diff rtype1 rtype2) 
                            (ast-diff declarator1 declarator2) (ast-diff-list preamble1 preamble2)
                            (ast-diff body1 body2)))]
      [(list (struct decl:declarator (_ id1 type1 init1)) 
             (struct decl:declarator (_ id2 type2 init2)))
       (check-list (append (ast-diff id1 id2) (ast-diff type1 type2) (ast-diff init1 init2)))]
      [_ (list 9 (let-values ([(type c) (struct-info one)]) type)) ]
      ))))

(define init-diff (lambda (one two)
  (let ([src-neq (lambda (x y) (if (equal? x y) '() (list (get-src two))))]
        [check-list (lambda (x) (if (member #f x) (list (get-src two)) x))])
    (match (list one two)
      [(list (struct init:compound (_ elements1)) (struct init:compound (_ elements2))) '()] ; not implemented
      [(list (struct init:expr (_ expr1)) (struct init:expr (_ expr2))) (ast-diff expr1 expr2)]
      [_ 10]
      ))))

(define c-expr-diff (lambda (one two)
  (ast-diff (parse-expression one) (parse-expression two))))

(define c-stmt-diff (lambda (one two)
  (ast-diff (parse-statement one) (parse-statement two))))

(define c-program-diff (lambda (one two)
  (ast-diff-list (parse-program one) (parse-program two))))


(define (try thunk catcher)
  (call/cc (lambda (k)
    (call-with-exception-handler (lambda (exn) (k (catcher exn))) thunk))))

(define (try-val thunk val)
  (try thunk (lambda (exn) val)))

(define (parse-any str)
  (let ([decl (try-val (lambda () (parse-declaration str)) #f)])
    (cond
      [(false? decl) 
         (let ([stmt (try-val (lambda () (parse-statement str)) #f)])
           (cond
             [(false? stmt) (try-val (lambda () (parse-expression str)) #f)]
             [else stmt]))]
      [else decl])))

(define (c-parse-diff one two)
  (let ([ast1 (parse-any one)][ast2 (parse-any two)])
    (cond
      [(false? ast2) (list (src 1 #f #f (+ 1 (string-length two)) #f #f #f))]
      [else (ast-diff (parse-any one) (parse-any two))])))

(define (diff-size diff)
  (cond
    [(empty? diff) 0]
    [(src? (car diff)) (+ 1 (- (src-end-offset (car diff)) (src-start-offset (car diff)))
                          (diff-size (cdr diff)))] ; the 1 here discourages lots of small highlights
    [(missing-code? (car diff)) (+ 1 (diff-size (cdr diff)))] ; 1 for missing section.
    [else (+ 1 (diff-size (cdr diff)))])) ; Throw an error or something?

(define (clean-diff diff)
  (cond
    [(empty? diff) empty]
    [(empty? (cdr diff)) diff]
    [else (if (and (src? (car diff)) (src? (second diff)) 
                   (= (src-end-offset (car diff)) (src-start-offset (second diff))))
              (clean-diff (cons (src (src-start-offset (car diff)) #f #f (src-end-offset (second diff)) #f #f #f)
                                (cdr (cdr diff))))
              (cons (car diff) (clean-diff (cdr diff))))]))

; The function c-diff takes two arguments. The first can be either a
;string or a list of strings that are possible good answers. The second
;argument is an input string that the answers are being matched
;against. The result is a list of areas where the input string should
;be highlighted to indicate an error, or places where there is possibly
;one missing input token.
;
;For each answer string we compare it with the ast and token list of
;the input string. If the string does not parse, the result of ast
;comparison is that the entire string it highlighted. Even if the
;string does not parse the lexical analysis will still give a good
;result. Ast comparison is good for ignoring superficial differences
;like extra parens. The token comparison simply traverses through each
;list of tokens. If the tokens are different, we look ahead one token
;in each list to see if we can synchronize the lists again. This will
;either highlight an unneeded token in the input string or result in a
;marker saying that a token should have gone in that spot. This is
;pretty rough and simple, but gives okay results. I took a look at
;doing something more sophisticated like Levenshtein distance but
;couldn't easily integrate it. Maybe something to add in the future.
;
;Every highlighting result has a "size" calculated for it which is
;based on the number of highlights, their size and the number of
;missing sections. The diff with the smallest value is returned which
;can be either the ast or token comparison for any one of the provided
;answer strings.

; (or/c string? (listof string?)) string? -> ???
(define (c-diff correct submitted)
  (cond
    [(empty? correct) 'null]
    [(string? correct)
     (let ([p-diff (clean-diff (c-parse-diff correct submitted))][l-diff (clean-diff (c-lex-diff correct submitted))])
       (if (< (diff-size p-diff) (diff-size l-diff)) p-diff l-diff))]
    [(list? correct) 
     (let ([fdiff (c-diff (car correct) submitted)][rdiff (c-diff (cdr correct) submitted)])
       (cond
         [(and (symbol? fdiff) (symbol? rdiff)) 'null]
         [(symbol? rdiff) fdiff]
         [(symbol? fdiff) rdiff]
         [else (if (< (diff-size fdiff) (diff-size rdiff)) fdiff rdiff)]))]
    [else empty]))

