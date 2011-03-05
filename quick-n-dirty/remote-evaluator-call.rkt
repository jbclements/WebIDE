#lang racket

(require (planet dherman/json:3:0)
         net/url
         net/uri-codec
         rackunit)

;; given a url and a string and an assoc-list of args and an assoc-list
;; of textfields, return the response of the evaluator as a jsexpr
(define (remote-evaluator-call url id args textfields)
  (define post-bytes (jsexpr->post-bytes (make-eval-jsexpr id args textfields)))
  ;; probably can't use pure-port for bad evaluators...
  (define port (post-pure-port the-url sample-json
                               '("Content-Type: application/x-www-form-urlencoded")))
  ;; this probably isn't robust wrt bad evaluators...
  (define reply (first (regexp-match #px".*" port)))
  (close-input-port port)
  (json->jsexpr (bytes->string/utf-8 reply)))

(define (make-eval-jsexpr id args textfields)
  (jsexpr->json 
        (make-hasheq `((id . ,id)
                       (args . ,(make-hasheq args))
                       (textfields . ,(make-hasheq textfields))))))

(define (jsexpr->post-bytes jsexpr)
  (string->bytes/utf-8 (string-append "request=" (form-urlencoded-encode jsexpr))))

;; an example:

(define sample-args 
  '((functionCall . "assignGroup(18)")
    (fmessage . "I'm sorry, your if statement for group C is incorrect.")
    (expectedOutput . "'C'")
    (smessage . "Good Job!")
    (function . "public char assignGroup(int age) { char group = 'x'; @groupC return group;} ")))

(define (success-response msg)
  (hash-set #hasheq((status . "success")) 'message msg))
#;(define (fail-response msg)
  (hash-set #hasheq((status . "failure")) 'message msg))

(define amazon-evaluator
  "http://184.73.238.21/webide/evaluators/JavaEvaluator/JavaEvaluator.php")

(check-equal?
 (remote-evaluator-call (string->url amazon-evaluator)
                        "Facebook: Selection"
                        sample-args
                        '((groupC . "group = 'C';")))
 (success-response "Good Job!"))