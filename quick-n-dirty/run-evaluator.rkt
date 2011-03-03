#lang racket

(provide run-evaluator
         (struct-out success)
         (struct-out failure)
         (struct-out serverfail))

(require "shared.rkt"
         web-server/http/request-structs
         rackunit)

(struct success () #:transparent)
(struct failure (msg) #:transparent)
(struct serverfail (msg) #:transparent)


;; run-evaluator : given a list of bindings and an evaluator,
;; run the evaluator on the given form bindings
(define ((run-evaluator bindings) eval)
  (match eval
    [(struct evaluator (url args))
     (define args-text-dict (fill-in-segs args
                                          bindings))
     (match url
       ;; blecch, terrible URL decision:
       ["evaluator://RegExEvaluator" 
        (regex-evaluator args-text-dict)]
       [other 
        (error 'run-evaluator "can't handle evaluator url ~s" url)])]))


;; regex-evaluator : performs a regular expression check
(define (regex-evaluator args-text-dict)
  (match-let ([(list rx fail-msg user-text)
               (fetch-args-text args-text-dict '("regex" "failed-message" "seg-0"))])
    (cond [(regexp-match (regexp rx) user-text) (success)]
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
(define (fill-in-segs args bindings)
  (define binding-finder (find-binding bindings))
  (dict-map args (convert-1arg binding-finder)))

;; (string->string) -> (or/c literal? userfield?) -> string?
(define ((convert-1arg binding-finder) name literal-or-userfield)
  (match literal-or-userfield
    [(struct literal (str)) (cons name str)]
    [(struct userfield (segid)) (cons name (binding-finder segid))]))

;; (listof form:binding) -> string -> string
;; find a binding with the given name. There must be exactly one, and it
;; must be a "form binding".
(define ((find-binding bindings) name)
  (match (filter (lambda (f) (equal? (string->bytes/utf-8 name) (binding-id f)))
                 bindings)
    [(list) (error 'find-binding "no segment found with name: ~a" name)]
    [(list (struct binding:form (dc val))) (bytes->string/utf-8 val)]
    [other 
     (error 'find-binding "more than one segment found with name: ~a" name)]))


(check-equal? ((convert-1arg (lambda (x) "bogus")) "abc" (literal "def"))
              (cons "abc" "def"))
(check-equal? ((convert-1arg (lambda (x) "bogus")) "abc" (userfield "def"))
              (cons "abc" "bogus"))

(check-equal? (regex-evaluator '(("regex" . "abc") 
                                 ("seg-0" . "deabcf")
                                 ("failed-message" . "z")))
              (success))
(check-equal? (regex-evaluator '(("regex" . "abc")
                                 ("seg-0" . "deabf")
                                 ("failed-message" . "z")))
              (failure "z"))

