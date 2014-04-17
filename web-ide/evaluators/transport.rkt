#lang racket

(require sxml
         json
         net/url
         net/uri-codec
         "shared.rkt")

(provide
 ;; for the client:
 remote-evaluator-call
 ;; for the server:
 post-bytes->args-n-fields
 jsexpr->response-bytes
 encode-html-for-transport
 ;; for testing:
 url-alive?)

(define standard-headers '("Content-Type: application/x-www-form-urlencoded"))

;; OUTER LAYER: DEALS WITH JSEXPRS

;; C->s

;; given a url and a string and an assoc-list of args and an assoc-list
;; of textfields, return the response of the evaluator as a jsexpr
(define (remote-evaluator-call url-string args textfields)
  ;; I don't believe any of the evaluators depend on the ID anyway...
  (jsexpr->result
   (remote-evaluator-call/jsexpr url-string
                                 (make-eval-jsexpr args textfields))))


;; s->C

;; turn the jsexpr returned by the server into one of our structures
(define (jsexpr->result jsexpr)
  (match (hash-ref jsexpr 'status 'serverfail)
    ["success" (success)]
    ["failure" (failure (decode-html-from-transport (hash-ref jsexpr 'message)))]
    ["serverfail" (serverfail (hash-ref jsexpr 'message))]
    ["callerfail" (callerfail (hash-ref jsexpr 'message))]
    [other (serverfail (format "unexpected response: ~s" jsexpr))]))


;; C->s

;; given a name and some args and textfields, generate
;; the jsexpr to be delivered to the evaluator
(define (make-eval-jsexpr args textfields)
  ;; I don't believe any of the evaluators depend on the ID anyway...
  (make-hasheq `((id . "BogusID")
                 (args . ,(make-hasheq args))
                 (textfields . ,(make-hasheq textfields)))))

;; c->S

;; given a jsexpr, extract args and textfields; insist on this structure exactly
(define (jsexpr->args-n-fields jsexpr)
  (unless (hash? jsexpr)
    (error 'jsexpr->args-n-fields "bad request shape (not a hash): ~s" jsexpr))
  (match (hash-map jsexpr cons)
    [(list-no-order `(id . ,dont-care)
                    `(args . ,(? hash? args-hash))
                    `(textfields . ,(? hash? textfields-hash)))
     (list (hash-map args-hash cons)
           (hash-map textfields-hash cons))]
    [other (error 'jsexpr->args-n-fields "bad request shape: ~s" jsexpr)]))

;; encode-html-for-transport : map sxml to representation that everyone likes
(define (encode-html-for-transport sxml)
  (srl:sxml->xml sxml))

;; decode-html-from-transport : map representation that everyone likes to sxml
;; okay, we're going to assume a single xml element comes across the wire
(define (decode-html-from-transport str)
  (call-with-input-string 
   (string-append "<div>"str"</div>") 
   (lambda (port)
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (string-append "unparsable string: " str))])
       (match (ssax:xml->sxml port '())
         [`(*TOP* (|@| . ,dc) ,content) content]
         [`(*TOP* ,content) content]
         [other (error 'decode-html-from-transport
                       "internal error 201103230517 parsed xml missing *TOP* node: ~s" 
                       other)])))))



;; composing these two yields the identity, for assoc lists mapping symbols to
;; strings.



;; THIS LAYER MAPS JSEXPRS TO JSON-STRINGS


;; C->s

;; given a url-string and a jsexpr, send it to the URL and wait for a 
;; response
;; string jsexpr -> jsexpr
(define (remote-evaluator-call/jsexpr url-string jsexpr)
  (define response-str
    (remote-evaluator-call/bytes 
     url-string 
     (jsexpr->string jsexpr)))
  (with-handlers ([exn:fail? 
                   (lambda (exn)
                     (fprintf (current-error-port)
                              "string->jsexpr failed on string: ~v" response-str)
                     (raise exn))])
    (string->jsexpr response-str)))

;; S->c

;; given a jsexpr, serialize it into a byte-string:
(define (jsexpr->response-bytes jsexpr)
  (string->bytes/utf-8 (jsexpr->string jsexpr)))

;; THIS LAYER DEALS WITH STRINGS & BYTES

;; C->s

;; given a url and a string, send the string to the URL and wait for a response.
(define (remote-evaluator-call/bytes url-string str)
  (define post-bytes (str->post-bytes str))
  (define eval-response-port (post-impure-port (string->url url-string)
                                               post-bytes
                                               standard-headers))
  ;; what about timeouts?
  (define headers (purify-port eval-response-port))
  ;; strange... why do our servers always come back with text/html as 
  ;; a mime type?
  (define reply-code 
    (match (regexp-match #px"^HTTP/[^ ]* ([0-9]+)" headers)
      [(list match digits) (string->number digits)]
      [other 'unparseable]))
  (cond [(= reply-code 200)
         (define reply (first (regexp-match #px".*" eval-response-port)))
         (close-input-port eval-response-port)
         (log-debug  (format "reply-bytes : ~v\n" reply))
         (bytes->string/utf-8 reply)]
        [else 
         (error 'remote-evaluator-call/bytes
                "response code: expected 200, got: ~v" 
                reply-code)]))

;; C->s

;; given a string, format it as the bytes to be attached to the post
;; request
(define (str->post-bytes str)
  ;; I think this form-urlencoded-encoded is totally unnecessary; I think the 
  ;; json encoding is already clean.
  (string->bytes/utf-8 (string-append "request=" (form-urlencoded-encode str))))

;; c->S

;; translate the post-bytes back into a string
(define (post-bytes->str post-bytes)
  ;; if requests get long, we might not want to do this on byte-strings, but
  ;; rather on ports:
  (match (regexp-match #px#"^request=(.*)$" post-bytes)
    [(list dc match) (form-urlencoded-decode (bytes->string/utf-8 match))]
    [other (error 'post-bytes->str "badly formatted request: ~s" other)]))


;; c->S

;; given post-bytes, map it back to a list of two association lists.
(define (post-bytes->args-n-fields post-bytes)
  (jsexpr->args-n-fields (string->jsexpr (post-bytes->str post-bytes))))



;; check that a given URL is alive and responds with a json result to a trivial 
;; json input.
(define (url-alive? url-string)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (remote-evaluator-call/jsexpr url-string 1234)
    #t))


(module+ test
  
;; TEST CASES

(require rackunit)

(check-equal? (match (post-bytes->args-n-fields
                      (str->post-bytes (jsexpr->string
                                        (make-eval-jsexpr
                                         '((a . "234")
                                           (b . "2778029"))
                                         '((c . "onteuh")
                                           (d . "ootoho"))))))
                [(list (list-no-order '(a . "234")
                                      '(b . "2778029"))
                       (list-no-order '(c . "onteuh")
                                      '(d . "ootoho")))
                 #t]
                [other #f])
              #t)

(check-equal? (match (jsexpr->args-n-fields 
                      (make-eval-jsexpr '((a . "b") (c . "d"))
                                        '((e . "f") (g . "h"))))
                [(list (list-no-order '(a . "b") '(c . "d"))
                       (list-no-order '(g . "h") '(e . "f")))
                 #t]
                [other #f])
              #t)



(check-equal? (post-bytes->str (str->post-bytes "a\"oo\"oht& ;h.th"))
              "a\"oo\"oht& ;h.th")

(check-equal? (decode-html-from-transport "abc&nbsp;de")
              "unparsable string: abc&nbsp;de")


(check-equal? (decode-html-from-transport 
               (encode-html-for-transport
                `(foo (|@| (size "19") (h "apple")) "abc " (i "def") " ghi")))
              `(div (foo (|@| (size "19") (h "apple")) "abc " (i "def") " ghi")))
)