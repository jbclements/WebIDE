#lang racket

(provide run-evaluator)

(struct success (msg))
(struct failure (msg))
(struct serverfail (msg))


;; run-evaluator : take an evaluator and a bindings-promise, run it:
(define ((run-evaluator bindings) evaluator)
  (define binding-finder (find-binding bindings))
  (define args (evaluator-args evaluator))
  (define texts (map binding-finder 
                     (evaluator-segs evaluator)))
  (printf "texts: ~v\n\n" texts)
  (match (evaluator-url evaluator)
    ;; this is pretty terrible, I think:
    ["evaluator://RegExEvaluator" 
     (cond [(regex-match (regex (assoc "regex" args)) )])]
    [other (error 'run-evaluator "can't run non-regex evaluators yet")]))

;; (listof form:binding) -> string -> string
;; find a binding with the given name. There must be exactly one, and it
;; must be a "form binding".
(define ((find-binding bindings) name)
  (match (filter (lambda (f) (equal? (string->bytes/utf-8 name) (binding-id f)))
                 bindings)
    [(list) (error 'find-binding "no segment found with name: ~a" name)]
    [(list (binding:form dc val)) (bytes->string/utf-8 val)]
    [other 
     (error 'find-binding "more than one segment found with name: ~a" name)]))