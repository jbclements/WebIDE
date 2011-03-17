#lang racket

(require (planet dherman/json:3:0)
         net/url
         net/uri-codec
         rackunit
         "../evaluators/shared.rkt")

(provide remote-evaluator-call)

(define standard-headers '("Content-Type: application/x-www-form-urlencoded"))

;; OUTER LAYER: DEALS WITH JSEXPRS

;; given a url and a string and an assoc-list of args and an assoc-list
;; of textfields, return the response of the evaluator as a jsexpr
(define (remote-evaluator-call url-string args textfields)
  ;; I don't believe any of the evaluators depend on the ID anyway...
  (define id "BogusID")
  (jsexpr->result
   (remote-evaluator-call/jsexpr url-string
                                 (make-eval-jsexpr id args textfields))))


;; turn the jsexpr returned by the server into one of our structures
(define (jsexpr->result jsexpr)
  (match (hash-ref jsexpr 'status 'serverfail)
    ["success" (success)]
    ["failure" (failure (hash-ref jsexpr 'message))]
    ['serverfail (serverfail (format "unexpected result from server: ~s" jsexpr))]))


;; given a name and some args and textfields, generate
;; the jsexpr to be delivered to the evaluator
(define (make-eval-jsexpr id args textfields)
  (make-hasheq `((id . ,id)
                 (args . ,(make-hasheq args))
                 (textfields . ,(make-hasheq textfields)))))

;; THIS LAYER MAPS JSEXPRS TO JSON-STRINGS

;; given a url-string and a jsexpr, send it to the URL and wait for a 
;; response
;; string jsexpr -> jsexpr
(define (remote-evaluator-call/jsexpr url-string jsexpr)
  (define response-bytes
    (remote-evaluator-call/bytes 
     url-string 
     (jsexpr->json jsexpr)
     standard-headers))
  (json->jsexpr (bytes->string/utf-8 response-bytes)))

;; THIS LAYER DEALS WITH BYTES

;; given a url and a bytes, send the bytes to the URL and wait for a response.
(define (remote-evaluator-call/bytes url-string json-str output-headers)
  (define post-bytes (json->post-bytes json-str))
  (define eval-response-port (post-impure-port (string->url url-string)
                                               post-bytes
                                               output-headers))
  ;; what about timeouts?
  (define headers (purify-port eval-response-port))
  (define reply-code 
    (match (regexp-match #px"^HTTP/[^ ]* ([0-9]+)" headers)
      [(list match digits) (string->number digits)]
      [other 'unparseable]))
  (cond [(= reply-code 200)
         (define reply (first (regexp-match #px".*" eval-response-port)))
         (close-input-port eval-response-port)
         reply]
        [else 
         (error 'remote-evaluator-call/bytes
                "response code: expected 200, got: ~v" 
                reply-code)]))


;; given a json string, format it as the bytes to be attached to the post
;; request
(define (json->post-bytes json-str)
  (string->bytes/utf-8 (string-append "request=" (form-urlencoded-encode json-str))))

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


(define (liveness-check url-string)
  (remote-evaluator-call/jsexpr url-string 1234))

;; TESTING:

(define amazon-evaluator
    "http://184.73.238.21/webide/evaluators/JavaEvaluator/JavaEvaluator.php")

(define (run-tests)
  
  
  (define (amazon-success-equal? args textfields)
    (check-equal? (remote-evaluator-call amazon-evaluator args textfields)
                  (success)))
  
  (define (check-amazon-fail? args textfields)
    (check-equal? (failure? (remote-evaluator-call amazon-evaluator args textfields))
                  #true))
  
  (check-equal? (make-hasheq '((a . "b") (c . "d")))
                (make-hasheq '((a . "b") (c . "d"))))
  
  (amazon-success-equal? sample-args '((groupC . "group = 'C';")))
  (check-amazon-fail? sample-args '((groupC . "234;"))))



(check-exn (lambda (exn)
             (regexp-match #px"expected 200" (exn-message exn)))
           (lambda () (liveness-check "http://www.berkeley.edu/ohhoeuntesuth")))

(check-equal? (liveness-check amazon-evaluator)
              #hasheq((message . "There is no id field in the evaluator request.")
                      (status . "failure")))

(liveness-check "http://localhost:8278")



