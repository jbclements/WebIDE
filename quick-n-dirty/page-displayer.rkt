#lang racket

(require web-server/servlet-env
         web-server/servlet/web
         web-server/http/request-structs
         "response-sxml.rkt"
         "xml-to-html.rkt"
         rackunit)

(define tiny-lab-path "/Users/clements/trac-webide/labs/JBCJava/tiny-lab.xml")
(define tiny-with-box-path "/Users/clements/trac-webide/labs/JBCJava/tiny-with-box.xml")
(define if-lab-path "/Users/clements/trac-webide/labs/if.xml")

(define sample-lab (path->xml if-lab-path))

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
         (evaluate-and-continue
          (step->evaluators (first steps))
          (send/suspend
           (lambda (k-url)
             (response/sxml
              (top-wrap `(form (@ (action ,k-url) (method "post"))
                               ,(pcon (first steps))
                               (input (@ (type "submit") (value "Submit"))))))))
          (rest steps))]))


;; evaluate-and-continue : take response and rest of steps, keep going:
(define (evaluate-and-continue evaluators request steps)
  (define bindings (filter binding:form? (request-bindings/raw request)))
  (printf "evaluators: ~a\n" evaluators)
  (map (run-evaluator bindings) evaluators)
  #;(define responses (collect-responses evaluators bindings-promise))
  (show-steps steps))

;; run-evaluator : take an evaluator and a bindings-promise, run it:
(define ((run-evaluator bindings) evaluator)
  (define texts (map (find-binding bindings) (evaluator-segs evaluator)))
  (printf "texts: ~a\n\n" texts))

(define ((find-binding bindings) name)
  (match (filter (lambda (f) (equal? (string->bytes/utf-8 name) (binding-id f)))
                 bindings)
    [(list) (error 'find-binding "no segment found with name: ~a" name)]
    [(list (binding:form dc val)) (bytes->string/utf-8 val)]
    [other 
     (error 'find-binding "more than one segment found with name: ~a" name)]))




(define steps (xml->steps sample-lab))

;; just do the first one, for now:

(define (start dc)
  (show-steps steps))

(serve/servlet start)

