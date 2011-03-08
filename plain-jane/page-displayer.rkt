#lang racket

(require web-server/servlet/web
         web-server/http/request-structs
         web-server/http/xexpr
         web-server/formlets         
         net/url
         "response-sxml.rkt"
         "xml-to-html.rkt"
         "run-evaluator.rkt"
         "shared.rkt")

(define tiny-lab-path "/Users/clements/trac-webide/labs/JBCJava/tiny-lab.xml")
(define tiny-with-box-path "/Users/clements/trac-webide/labs/JBCJava/tiny-with-box.xml")
(define if-lab-path "/Users/clements/trac-webide/labs/if.xml")
(define has-evaluator-path "/Users/clements/trac-webide/labs/JBCJava/has-evaluator.xml")

(define if-url "http://brinckerhoff.org/tmp/if.xml")

(provide run-from-url
         run-lab
         request-url-from-user)

(define (sample-start)
  (run-lab (path->xml has-evaluator-path #;if-lab-path)))

;; given a lab xml url, load that lab and run it:
(define (run-from-url url-string)
  (run-lab (url->xml url-string)))

;; given xml, run a lab:
(define (run-lab lab-xml)
  (define steps (xml->steps lab-xml))
  (show-steps steps))

;; loop through the steps, showing them all:
(define (show-steps steps)
  (cond [(empty? steps) (response/sxml
                         (top-wrap `(h1 "All Done! Time for a Banana Smoothie!")))]
        [else
         (define user-response
           (send/suspend
            (lambda (k-url)
              (response/sxml
               (top-wrap `(form (@ (action ,k-url) (method "post"))
                                ,(pcon (first steps))
                                (input (@ (type "submit") (value "Submit")))))))))
         (evaluate-and-continue (step->evaluators (first steps))
                                user-response
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


;; ask the user to enter a URL
(define (request-url-from-user)
  (formlet-process
   url-query-formlet
   (send/suspend 
    (lambda (url)
      ;; oog, in order to use formlets we're stuck with xexprs:
      (response/xexpr
       `(html (head (title "Plain Jane Step Rendering"))
              (form ((action ,url))
                    ,@(formlet-display url-query-formlet)
                    (input ((type "submit"))))))))))

(define url-query-formlet
  (formlet
   (div "enter a URL:" ,{input-string . => . url})
   url))

;; turn a body sxml into a top sxml
(define (top-wrap sxml)
  `(*TOP*
    (html (head (title "Plain Jane Step Rendering"))
          (body ,sxml))))

;; show a page with a fail message on it.
(define (fail-page msg)
  (response/sxml
   (top-wrap `(div (h1 "Evaluator Failed")
                   (p "failure message : " ,msg)))))


;; read the lab xml from a path
(define (path->xml path)
  (call-with-input-file path
    (lambda (port)
      (port->xml port))))


;; read the lab xml from a URL
(define (url->xml url-string)
  (define xml-port (get-pure-port (string->url url-string)))
  (begin0 
    (port->xml xml-port)
    (close-input-port xml-port)))

#;(url->xml "http://brinckerhoff.org/tmp/tiny-lab.xml")


