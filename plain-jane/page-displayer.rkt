#lang racket

(require web-server/servlet/web
         web-server/http/request-structs
         web-server/http/xexpr
         web-server/formlets         
         net/url
         xml
         "response-sxml.rkt"
         "xml-to-html.rkt"
         "run-evaluator.rkt"
         "../evaluators/shared.rkt"
         rackunit)

(define innocuous-tags
  '(div h1 h2 h3 h4 p i))

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
           (match (let ([ans (ev (first evaluators))])
                    ;; local printf debugging...
                    (printf "result of evaluator: ~s\n" ans)
                    ans)
             [(struct success ()) (loop (rest evaluators))]
             [(struct failure (xexpr)) (fail-page xexpr)])])))


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
;; xml-element -> 
(define (fail-page result-xml-element)
  #;(define result-xml-element (parse-response-msg msg))
  (response/sxml
   (top-wrap `(div (h1 "Evaluator Failed")
                   (p "failure message : ")
                   ,result-xml-element))))

;; turn the response message into displayable xexprs:
(define (parse-response-msg msg)
  ;; on illegal xml, just return the string:
  (with-handlers ([exn:fail? (lambda (exn) msg)])
    (let ([element
           (xml->xexpr
            (call-with-input-string 
             msg
             read-xml/element))])
      (check-for-illegal-elements element)
      element)))

;; make sure that only the tags in the legal list occur. 
;; this probably isn't good enough...
(define (check-for-illegal-elements elt)
  (match elt 
    [`(,tag ((,attr ,val) ...) . ,content)
     (and (okay-tag? tag)
          ;; check attrs...?
          (andmap check-for-illegal-elements content))]
    [`(,tag . ,content)
     (and (okay-tag? tag)
          ;; check attrs...?
          (andmap check-for-illegal-elements content))]
    [other ;; should all be okay?
     #t]))

(define (okay-tag? t)
  (or 
   (memq t innocuous-tags)
   (error 'okay-tag? "illegal tag in response: ~a. Malicious evaluator?" t)))

(check-exn exn:fail?
           (lambda ()
             (check-for-illegal-elements `(div (p () (illegal!))))))
(check-equal? (check-for-illegal-elements `(div (p ((bronski "foobar")) (p "oech"))))
              #t)
(check-equal? (parse-response-msg "<div>I <i>really</i> like your code!</div>")
              `(div () "I "(i () "really")" like your code!"))
(check-equal? (parse-response-msg "<div>oh dear <i>unbalanced tag</div>")
              "<div>oh dear <i>unbalanced tag</div>")

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


