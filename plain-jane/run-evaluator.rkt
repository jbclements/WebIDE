#lang racket

(provide run-evaluator)

(require "shared.rkt"
         web-server/http/request-structs
         rackunit
         "remote-evaluator-call.rkt"
         "c-evaluator-call.rkt")

;; run-evaluator : given a list of bindings and an evaluator,
;; run the evaluator on the given form bindings
(define ((run-evaluator bindings) eval)
  (match eval
    [(struct evaluator (url-string args segs))
     (define segs-texts (fill-in-segs segs bindings))
     (match url-string
       ;; blecch, terrible URL decision:
       ["evaluator://RegExEvaluator" 
        (regex-evaluator args segs-texts)]
       ["evaluator://any-c-int"
        (any-c-int args segs-texts)]
       ["evaluator://any-c-addition"
        (any-c-addition args segs-texts)]
       [(pregexp "^http://") 
        (remote-evaluator-call url-string args segs-texts)]
       [other 
        (error 'run-evaluator "unrecognized URL schema: ~s" url-string)])]))


;; regex-evaluator : performs a regular expression check
(define (regex-evaluator args-texts segs-texts)
  (match-let ([(list rx fail-msg)
               (fetch-args-text args-texts '(regex failed-message))]
              [(list (cons dont-care user-text))
               segs-texts])
    (cond [(regexp-match (pregexp rx) user-text) (success)]
          [else (failure fail-msg)])))

;; fetch a list of elements from a dict; signal errors if missing
(define (fetch-args-text dict keys)
  (for/list ([k (in-list keys)])
    (dict-ref dict k (lambda () 
                       (error 'fetch-args-text "evaluator missing required field: ~a" k)))))


;; fill-in-segs : args-dict (listof form:binding) -> strings-dict
;; take the argument specification from an evaluator, and 
;; plug in the strings obtained from the user. produce a mapping from 
;; strings to strings
(define (fill-in-segs segs bindings)
  (for/list ([s (in-list segs)])
    (cons (car s) (find-binding bindings (cdr s)))))

;; (listof form:binding) -> string -> string
;; find a binding with the given name. There must be exactly one, and it
;; must be a "form binding".
(define (find-binding bindings name)
  (match (filter (lambda (f) (equal? (string->bytes/utf-8 name) (binding-id f)))
                 bindings)
    [(list) (error 'find-binding "no segment found with name: ~a" name)]
    [(list (struct binding:form (dc val))) (bytes->string/utf-8 val)]
    [other 
     (error 'find-binding "more than one segment found with name: ~a" name)]))


(check-equal? (regex-evaluator `((regex . "abc")
                                 (failed-message . "z"))
                               `((unknown . "deabcf")))
              (success))
(check-equal? (regex-evaluator '((regex . "abc")
                                 (failed-message . "z"))
                               `((unknown . "deabf")))
              (failure "z"))

(check-equal? (regex-evaluator '((regex . "^\\s*abc")
                                 (failed-message . "z"))
                               `((unknown . "   abc   ")))
              (success))
(check-equal? (regex-evaluator '((regex . "^\\s*abc")
                                 (failed-message . "z"))
                               `((unknown . "   abf   ")))
              (failure "z"))

