#lang racket

(require web-server/servlet-env
         web-server/servlet/web
         web-server/http/request-structs
         "response-sxml.rkt"
         "xml-to-html.rkt"
         "run-evaluator.rkt"
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
  (define ev (run-evaluator bindings))
  (let loop ([evaluators evaluators])
    (cond [(empty? evaluators) (show-steps steps)]
          [else 
           (match (ev (first evaluators))
             [(struct success ()) (loop (rest evaluators))]
             [(struct failure (msg)) (fail-page msg)])])))

(define (fail-page msg)
  (response/sxml
   (top-wrap `(div (h1 "Evaluator Failed")
                   (p "failure message : " ,msg)))))






(define steps (xml->steps sample-lab))

;; just do the first one, for now:

(define (start dc)
  (show-steps steps))

(serve/servlet start)

