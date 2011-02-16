#lang racket

(require web-server/servlet-env
         web-server/servlet/web
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
         (send/suspend
          (lambda (k-url)
          (response/sxml
           (top-wrap `(div ,(step->html (first steps))
                           (p (a (@ (href ,k-url)) "continue")))))))
         (show-steps (rest steps))]))


(define steps (xml->steps sample-lab))

;; just do the first one, for now:

(define (start dc)
  (show-steps steps))

(serve/servlet start)

