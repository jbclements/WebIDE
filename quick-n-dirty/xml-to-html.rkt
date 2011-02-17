#lang racket

(require (planet clements/sxml2)
         (except-in (planet clements/sxml2/ssax/ssax) run-test foldts)
         rackunit)

(provide path->xml
         xml->steps
         step->html
         )

(define (xml->steps lab-xml)
  ((sxpath '(w1:lab w1:step)) lab-xml))

(define (path->xml path)
  (call-with-input-file path
    (lambda (port)
      (ssax:xml->sxml port `((w1 . "http://www.web-ide.org/namespaces/labs/1"))))))

;; given a step, produce the corresponding html content
(define (step->html step)
  (match step
    [`(w1:step (@ ,attrs ...) ,content ...)
     (unless (assoc 'name attrs)
       (error 'step->html "step missing name attribute"))
     (define name (second (assoc 'name attrs)))
     `(div (h3 "step name: " ,name)
           ,@(map process-content content))]))

;; mush the xml into plausible html:
(define (process-content content)
  ;; for now, just strip the namespace off:
  (match content
    ;; don't display evaluators.
    [`(w1:evaluator ,stuff ...) ""]
    [`(w1:segment ,contents ...)
     (match contents
       [`((@ ,attrs ...) ,(? string? strs) ...)
        `(textarea (@ (width ,(first (dict-ref attrs 'width)))
                      (height ,(first (dict-ref attrs 'height))))
                   "" ,@strs)]
       ;; ignore the bogus ones that just hold evaluators:
       [`((@ ,attrs ...) ,(? evaluator? e) ...) ""]
       [other (error 'process-content
                     "strange segment: ~a" content)])]
    [`(,tag (@ ,attrs ...) ,content ...)
     `(,(strip-tag tag) (@ ,@attrs) ,@(map process-content content))]
    [`(,tag ,content ...)
     `(,(strip-tag tag) ,@(map process-content content))]    
    [other other]))


(check-equal? (process-content `(w1:segment (@ (width "20") (id "fresh-id-1") (height "1"))))
              `(textarea (@ (width "20") (height "1")) ""))

;; is this element an evaluator?
(define (evaluator? sxml)
  (and (list? sxml) (equal? (first sxml) 'w1:evaluator)))

;; strip the colon-separated part of a symbol off.
(define (strip-tag s)
  (match (regexp-match #px"[^:]*:(.*)" (symbol->string s))
    [(list match rhs) (string->symbol rhs)]
    [false (error 'strip-tag "tag without prefix: ~a" s)]))

(check-equal? (strip-tag 'w1:br) 'br)

(check-equal? (evaluator? `(3 w1:evaluator)) #f)
(check-equal? (evaluator? `(w1:evaluator (@ (abc "def")))) #t)

(check-equal? (process-content `(w1:b (w1:i "zoobah")))
              `(b (i "zoobah")))

(check-equal? (process-content `(w1:b (@ (awesomeness "35")) "trip"))
              `(b (@ (awesomeness "35")) "trip"))

