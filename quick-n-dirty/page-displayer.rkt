#lang racket

(require (planet lizorkin/sxml/sxml)
         (except-in (planet lizorkin/ssax/ssax) run-test foldts)
         web-server/servlet-env
         web-server/servlet/web
         "response-sxml.rkt"
         rackunit)

(define tiny-lab-path "/Users/clements/trac-webide/labs/JBCJava/tiny-lab.xml")
(define tiny-with-box-path "/Users/clements/trac-webide/labs/JBCJava/tiny-with-box.xml")
(define if-lab-path "/Users/clements/trac-webide/labs/if.xml")

(define sample-lab
  (call-with-input-file if-lab-path #;tiny-with-box-path
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

;; turn a body sxml into a top sxml
(define (top-wrap sxml)
  `(*TOP*
    (html (title "Quick And Dirty Step Rendering")
          (body ,sxml))))

;; loop through the steps, showing them all:
(define (show-steps steps)
  (cond [(empty? steps) (response/sxml
                         (top-wrap `(h1 "All Done! You rock!")))]
        [else
         (send/suspend
          (lambda (k-url)
          (response/sxml
           (top-wrap `(div ,@(map process-content (rest (rest (first steps))))
                           (p (a (@ (href ,k-url)) "continue")))))))
         (show-steps (rest steps))]))

(define (process-content content)
  ;; for now, just strip the namespace off:
  (match (let ([ans content])
           (printf "~s\n" ans)
           ans)
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

(define (evaluator? sxml)
  (and (list? sxml) (equal? (first sxml) 'w1:evaluator)))




(define (strip-tag s)
  (match (regexp-match #px"[^:]*:(.*)" (symbol->string s))
    [(list match rhs) (string->symbol rhs)]
    [false (error 'strip-tag "tag without prefix: ~a" s)]))

(check-equal? (strip-tag 'w1:br) 'br)

(define steps ((sxpath '(w1:lab w1:step)) sample-lab))

;; just do the first one, for now:

(define (start dc)
  (show-steps steps))

(serve/servlet start)

