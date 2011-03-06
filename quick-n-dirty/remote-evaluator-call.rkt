#lang racket

(require (planet dherman/json:3:0)
         net/url
         net/uri-codec
         rackunit
         "shared.rkt")

(provide remote-evaluator-call)

;; given a url and a string and an assoc-list of args and an assoc-list
;; of textfields, return the response of the evaluator as a jsexpr
(define (remote-evaluator-call url-string args textfields)
  ;; I don't believe any of the evaluators depend on the ID anyway...
  (define id "BogusID")
  (define post-bytes (jsexpr->post-bytes (make-eval-jsexpr id args textfields)))
  ;; probably can't use pure-port for bad evaluators...
  (define port (post-pure-port (string->url url-string) post-bytes
                               '("Content-Type: application/x-www-form-urlencoded")))
  ;; this probably isn't robust wrt bad evaluators...
  (define reply (first (regexp-match #px".*" port)))
  (close-input-port port)
  (jsexpr->result (json->jsexpr (bytes->string/utf-8 reply))))

;; given a name and some args and textfields, generate
;; the json string to be delivered to the evaluator
(define (make-eval-jsexpr id args textfields)
  (jsexpr->json 
        (make-hasheq `((id . ,id)
                       (args . ,(make-hasheq args))
                       (textfields . ,(make-hasheq textfields))))))

;; given a json string, format it as the bytes to be attached to the post
;; request
(define (jsexpr->post-bytes jsexpr)
  (string->bytes/utf-8 (string-append "request=" (form-urlencoded-encode jsexpr))))


;; turn the json string returned by the server into one of our structures
(define (jsexpr->result jsexpr)
  (match (hash-ref jsexpr 'status 'serverfail)
    ["success" (success)]
    ["failure" (failure (hash-ref jsexpr 'message))]
    ['serverfail (serverfail (format "unexpected result from server: ~s" jsexpr))]))

;; an example:

(define test-success-msg "success-message-htns")

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

;; why is the equal? check failing? ah well.

(define (amazon-success-equal? args textfields)
  (check-equal? (remote-evaluator-call amazon-evaluator args textfields)
                (success)))

(define (check-amazon-fail? args textfields)
  (check-equal? (failure? (remote-evaluator-call amazon-evaluator args textfields))
                #true))

(check-equal? (make-hasheq '((a . "b") (c . "d")))
              (make-hasheq '((a . "b") (c . "d"))))

(amazon-success-equal? sample-args '((groupC . "group = 'C';")))
(check-amazon-fail? sample-args '((groupC . "234;")))
