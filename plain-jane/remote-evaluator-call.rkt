#lang racket

(require (planet dherman/json:3:0)
         net/url
         net/uri-codec
         rackunit
         "shared.rkt")

(provide remote-evaluator-call)

(define standard-headers '("Content-Type: application/x-www-form-urlencoded"))

;; OUTER LAYER: DEALS WITH JSEXPRS

;; given a url and a string and an assoc-list of args and an assoc-list
;; of textfields, return the response of the evaluator as a jsexpr
(define (remote-evaluator-call url-string args textfields)
  ;; I don't believe any of the evaluators depend on the ID anyway...
  (jsexpr->result
   (remote-evaluator-call/jsexpr url-string
                                 (make-eval-jsexpr args textfields))))


;; turn the jsexpr returned by the server into one of our structures
(define (jsexpr->result jsexpr)
  (match (hash-ref jsexpr 'status 'serverfail)
    ["success" (success)]
    ["failure" (failure (hash-ref jsexpr 'message))]
    ['serverfail (serverfail (format "unexpected result from server: ~s" jsexpr))]))


;; given a name and some args and textfields, generate
;; the jsexpr to be delivered to the evaluator
(define (make-eval-jsexpr args textfields)
  ;; I don't believe any of the evaluators depend on the ID anyway...
  (make-hasheq `((id . "BogusID")
                 (args . ,(make-hasheq args))
                 (textfields . ,(make-hasheq textfields)))))

;; given a jsexpr, extract args and textfields; insist on this structure exactly
(define (jsexpr->args-n-fields jsexpr)
  (match (hash-map jsexpr cons)
    [(list-no-order `(id . ,dont-care)
                    `(args . ,args-hash)
                    `(textfields . ,textfields-hash))
     (list (hash-map args-hash cons)
           (hash-map textfields-hash cons))]
    [other (error 'jsexpr->args-n-fields "bad request shape: ~s" jsexpr)]))


;; composing these two yields the identity, for assoc lists mapping symbols to
;; strings.

;; this test case shouldn't depend on the ordering of the elements of the list, but 
;; it does.
(check-equal? (jsexpr->args-n-fields (make-eval-jsexpr '((a . "b") (c . "d"))
                                                       '((e . "f") (g . "h"))))
              (list '((a . "b") (c . "d"))
                    '((g . "h") (e . "f"))))

;; THIS LAYER MAPS JSEXPRS TO JSON-STRINGS

;; given a url-string and a jsexpr, send it to the URL and wait for a 
;; response
;; string jsexpr -> jsexpr
(define (remote-evaluator-call/jsexpr url-string jsexpr)
  (define response-str
    (remote-evaluator-call/bytes 
     url-string 
     (jsexpr->json jsexpr)))
  (json->jsexpr response-str))

;; THIS LAYER DEALS WITH STRINGS & BYTES

;; given a url and a string, send the string to the URL and wait for a response.
(define (remote-evaluator-call/bytes url-string str)
  (define post-bytes (str->post-bytes str))
  (define eval-response-port (post-impure-port (string->url url-string)
                                               post-bytes
                                               standard-headers))
  ;; what about timeouts?
  (define headers (purify-port eval-response-port))
  (define reply-code 
    (match (regexp-match #px"^HTTP/[^ ]* ([0-9]+)" headers)
      [(list match digits) (string->number digits)]
      [other 'unparseable]))
  (cond [(= reply-code 200)
         (define reply (first (regexp-match #px".*" eval-response-port)))
         (close-input-port eval-response-port)
         (bytes->string/utf-8 reply)]
        [else 
         (error 'remote-evaluator-call/bytes
                "response code: expected 200, got: ~v" 
                reply-code)]))


;; given a string, format it as the bytes to be attached to the post
;; request
(define (str->post-bytes str)
  (string->bytes/utf-8 (string-append "request=" (form-urlencoded-encode str))))


(define (post-bytes->str post-bytes)
  ;; if requests get long, we might not want to do this on byte-strings, but
  ;; rather on ports:
  (match (regexp-match #px#"^request=(.*)$" post-bytes)
    [(list dc match) (form-urlencoded-decode (bytes->string/utf-8 match))]
    [other (error 'post-bytes->str "badly formatted request: ~s" other)]))


(check-equal? (post-bytes->str (str->post-bytes "a\"oo\"oht& ;h.th"))
              "a\"oo\"oht& ;h.th")



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

;; check that a given URL is alive and responds with a json result to a trivial 
;; json input.
(define (url-alive? url-string)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
  (remote-evaluator-call/jsexpr url-string 1234)))

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
  
  (amazon-success-equal? sample-args '((groupC . "group = 'C';")))
  (check-amazon-fail? sample-args '((groupC . "234;")))
  
  

  (check-equal? (url-alive? "http://www.berkeley.edu/ohhoeuntesuth") #f)
  
  (check-equal? (not (not (url-alive? amazon-evaluator))) #t)
  
  (check-equal? (url-alive? "http://bogo-host-that-doesnt-exist.com/") #f))

(run-tests)





