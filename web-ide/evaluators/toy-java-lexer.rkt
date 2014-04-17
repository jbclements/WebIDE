#lang racket

;; A tiny lexer for a subset of Java


;; Import the parser and lexer generators.
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         rackunit)

(define-lex-abbrevs
 (lower-letter (:/ "a" "z"))

 (upper-letter (:/ #\A #\Z))

 ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
 (digit (:/ "0" "9")))


;; a toy java lexer, based on my first guesses about
;; java syntax.  I didn't look at the spec, and I'm 
;; sure it's wildly incomplete.
(define toy-parser
  (lexer
   [(eof) 'EOF]
   [(:or #\tab #\space #\newline) (toy-parser input-port)]
   [(:or "," "=" "+" "-" "*" "/" "^" "(" ")" ";") lexeme]
   [(:: (:or lower-letter upper-letter "_")
        (:* (:or lower-letter upper-letter "_" digit))) lexeme]
   [(:+ digit) lexeme]
   [(:: (:+ digit) #\. (:* digit)) lexeme]))
   

;; tokenize-string : string -> (listof string)
;; take a string, break it into substrings according to
;; the parser defined above.
(provide tokenize-string)
(define (tokenize-string str)
  (let ([ip (open-input-string str)])
    (let loop ()
      (let ([token (toy-parser ip)])
        (match token
          ['EOF '()]
          [other (cons other (loop))])))))

;; a few superficial test cases. Did I find bugs? You betcha.
(check-equal? (tokenize-string "int getboozle(x)")
              (list "int" "getboozle" "(" "x" ")"))
(check-equal? (tokenize-string "zap_34 = 9.42;")
              (list "zap_34" "=" "9.42" ";"))
