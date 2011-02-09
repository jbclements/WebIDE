#lang racket

(require (planet lizorkin/sxml/sxml)
         (except-in (planet lizorkin/ssax/ssax) run-test foldts)
         web-server/servlet-env
         web-server/servlet/web
         "response-sxml.rkt"
         rackunit)

(define tiny-lab-path "/Users/clements/trac-webide/labs/JBCJava/tiny-lab.xml")
(define if-lab-path "/Users/clements/trac-webide/labs/if.xml")

(define sample-lab
  (call-with-input-file if-lab-path
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
           (top-wrap `(div ,(process-content (first steps))
                           (p (a (@ (href ,k-url)) "continue")))))))
         (show-steps (rest steps))]))

(define (process-content content)
  ;; for now, just strip the namespace off:
  (match content
    [`(,tag (@ ,attrs ...) ,content ...)
     `(,(strip-tag tag) (@ ,@attrs) ,@(map process-content content))]
    [`(,tag ,content ...)
     `(,(strip-tag tag) ,@(map process-content content))]
    [other other]))

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

