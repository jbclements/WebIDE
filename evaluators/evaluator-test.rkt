#lang racket

(require (planet dherman/json:3:0)
         net/url
         net/uri-codec)


(define sample-json
  (string->bytes/utf-8
  (string-append
   "request="
    (form-urlencoded-encode
   (jsexpr->json 
    #hasheq((id . "Facebook: Selection")
            (args . #hasheq((functionCall . "assignGroup(18)")
                            (fmessage . "I'm sorry, your if statement for group C is incorrect.")
                            (expectedOutput . "'C'")
                            (smessage . "Good Job!")
                            (function . "public char assignGroup(int age) { char group = 'x'; @groupC return group;} ")))
            (textfields . #hasheq((groupC . "abba zabba")))))))))

(define (path->path/params los)
  (map (lambda (s) (path/param s '())) los))

(define the-url
  (url "http" #f "184.73.238.21" #f #t 
       (path->path/params '("webide""evaluators""JavaEvaluator" "JavaEvaluator.php")) 
       `()
       #f))

(url->string the-url)
(define port (post-impure-port the-url sample-json
                               '("Content-Type: application/x-www-form-urlencoded")))
(regexp-match #px".*" port)
(close-input-port port)

