#lang racket

(provide run-evaluator
         step->evaluators)

(require web-server/http/request-structs
         sxml
         rackunit
         "../evaluators/transport.rkt"
         "../evaluators/c-evaluators.rkt"
         "../evaluators/shared.rkt")

;; an evaluator is (struct string? (string->string assoc) (string->string assoc))
(struct evaluator (url args textfields) #:prefab)

;; run-evaluator : given a list of bindings and an evaluator,
;; run the evaluator on the given form bindings
(define ((run-evaluator bindings) eval)
  (match eval
    [(struct evaluator (url-string args segs))
     (define segs-texts (fill-in-segs segs bindings))
     (match url-string
       ;; blecch, terrible URL decision:
       ["evaluator://RegExEvaluator" (regex-evaluator args segs-texts)]
       ["evaluator://any-c-int"      (any-c-int args segs-texts)]
       ["evaluator://any-c-addition" (any-c-addition args segs-texts)]
       ["evaluator://c-parser-match" (c-parser-match args segs-texts)]
       [(pregexp "^http://")         (remote-evaluator-call url-string args segs-texts)]
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




;; extracting evaluators (not really sure where this code should live...):

;; sxml -> evaluator
(define (step->evaluators step)
  (map parse-evaluator 
       ((sxpath '(// w1:evaluator)) `(*TOP* ,step))))

(define (parse-evaluator eval)
  (match eval
    [`(w1:evaluator (|@| . ,attrs)
                    (w1:fixedArg (w1:name ,argname) 
                                 (w1:value ,argvalue))
                    ...
                    (w1:userfieldArg (w1:name ,fieldname)
                                     (w1:value ,fieldvalue))
                    ...)
     (evaluator (cadr (assoc 'href attrs))
                (map cons (map string->symbol argname) argvalue)
                (map cons (map string->symbol fieldname) fieldvalue))]))

(define (seg-or-arg? elt) (member (car elt) '(w1:segid w1:arg)))
(define (seg? elt) (equal? (car elt) 'w1:segid))

;; return the string for a well-formed segid
(define (seg->str elt idx)
  (match elt
    ;; this convention is gross, because the evaluator has to know the names of the segments...
    [`(w1:segid (w1:id ,(? string? segid))) (cons (string->symbol segid) segid)]
    [other (error 'seg->str "badly formed seg: ~v" elt)]))

;; return a list containing the name and value of an arg
(define (arg->strs elt)
  (match elt
    [`(w1:arg (w1:name ,(? string? name)) (w1:value ,value)) (cons (string->symbol name) value)]
    [`(w1:arg (w1:name ,(? string? name)) (w1:value)) (cons (string->symbol name) "")]
    [other (error 'arg->strs "badly formed arg: ~v" elt)]))



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


(check-equal? (step->evaluators 
               `(w1:step "a" (w1:evaluator (@ (href "ghi")))
                         (p (b (w1:evaluator (@ (href "stu"))
                                             (w1:fixedArg (w1:name "a1")
                                                     (w1:value "v1"))
                                             (w1:userfieldArg
                                              (w1:name "foo") 
                                              (w1:value "bar")))))))
              (list (evaluator "ghi" '() '())
                    (evaluator "stu" '((a1 . "v1"))
                               '((foo . "bar")))))