#lang racket

;; initial code by Bill Hess, 2011-01-03

(require parser-tools/lex
         c/ast
         c/private/lexer)

(provide c-lex-diff 
         missing-code
         missing-code?
         missing-code-start
         missing-code-end)

(struct missing-code (start end))

;; turn a string into a list of tokens
(define (c-token-list str)
  (let ([lex (make-lexer str)])
    (make-token-list lex)))

;; create a lexer for a string
(define (make-lexer str)
  (let ([in (open-input-string str)])
    (lambda () (c-lexer in))))

;; pull all of the tokens out of the lexer
;; lexer -> (listof token)
(define (make-token-list lex)
  (let ([cur (lex)])
    (cond [(equal? (position-token-token cur) 'EOF) empty]
          [else (cons cur (make-token-list lex))])))


(define (src-from-token tok)
  (build-src (position-token-start-pos tok) (position-token-end-pos tok) #f))

(define (end-offset pos-tok)
  (position-offset (position-token-end-pos pos-tok)))

(define (start-offset pos-tok)
  (position-offset (position-token-start-pos pos-tok)))

; one, two - ASTs
; from, end - int positions. from is the ending offset of the last token. 
;             end is the offset of the end of string two

;; I think it would probably be a lot better to use a generalized 'diff' from 
;; a library, but it looks like Neil's is the one to use, and it doesn't (yet?)
;; provide this functionality.
(define (diff-tokens one two from end)
  (cond
    [(and (empty? one) (empty? two)) empty]
    [(empty? one) (map src-from-token two)]
    [(empty? two) (list (missing-code from end))]
    [else (let ([t1 (position-token-token (car one))]
                [t2 (position-token-token (car two))]
                [next-from (end-offset (car two))])
        (cond
          [(equal? t1 t2) (diff-tokens (cdr one) (cdr two) next-from end)]
          [(and (not (empty? (cdr two))) 
                (equal? t1 (position-token-token (second two))))
           (cons (src-from-token (car two))
                 (diff-tokens one (cdr two) next-from end))]
          [(and (not (empty? (cdr one)))
                (equal? t2 (position-token-token (second one))))
           (cons (missing-code from (start-offset (car two)))
                 (diff-tokens (cdr one) two from end))]
          [else (cons (src-from-token (car two))
                      (diff-tokens (cdr one) (cdr two) next-from end))]))]))

;(check-expect (diff-tokens (c-token-list "3 + 5") (c-token-list "3 + 5")) empty)
;(diff-tokens (c-token-list "3 + + 5") (c-token-list "3 + 5"))

(define (c-lex-diff one two)
  (diff-tokens (c-token-list one)
               (c-token-list two)
               1
               (+ 1 (string-length two))))

