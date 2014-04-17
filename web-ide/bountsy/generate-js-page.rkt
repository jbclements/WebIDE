#lang racket

(require sxml
         racket/runtime-path)

(define-runtime-path htdocs-path
  "./htdocs")

;; given a relative path to a script, return sxml embedding a
;; call to that script
(define (jscr rel-path)
  `(script
    (@ (type "text/javascript") (src ,rel-path) (charset "utf-8"))
    ""))

;; thinking out loud here.  A lab has a sequence of panes. 

(define parsed 
  
  `(*TOP*
  (html
   (head
    ,(jscr "jquery.min.js")
    ,(jscr "qtip.js")
    ,(jscr "script.js")
    (script
     (@ (type "text/javascript") (charset "utf-8"))
     "$(document).ready(runLab);")
    (link (@
           (type "text/css")
           (title "no title")
           (rel "stylesheet")
           (media "screen")
           (href "styles.css")
           (charset "utf-8")))
    (title "Demo Lab"))
   (body
    (div
     (@ (step "0") (class "section"))
     (form
      (@
       (method "post")
       (class "answerfield")
       (action "bountsy/alwaysSucceed"))
      (p (h3 "Integers"))
      (p "\n            Blah blah HTML here          ")
      (textarea
       (@ (rows "1") (name "comments") (id "q1box") (cols "20"))
       " ")
      (input (@ (value "evaluate") (type "submit")))))
    (div
     (@ (step "1") (class "section"))
     (form
      (@
       (method "post")
       (class "answerfield")
       (action "bountsy/any-c-int"))
      (p (h3 "Arithmetic operations"))
      (p "Yet more text here.")
      (textarea
       (@
        (name "text-field-4")
        (id "foomf")
        (evaluator "any-c-addition"))
       "")
      (input (@ (value "Evaluate") (type "submit")))
      (br)))
    (div
     (@ (step "2") (class "section"))
     (form
      (@
       (method "post")
       (class "answerfield")
       (action "bountsy/any-c-int"))
      (p (h3 "Arithmetic operations again"))
      (p "\n            More text here.\n          ")
      (textarea
       (@
        (name "text-field-4")
        (id "foomf")
        (evaluator "any-c-addition"))
       " ")
      (input (@ (value "Evaluate") (type "submit")))
      (br)))
    (div
     (@ (id "nextButtonHolder"))
     (input
      (@
       (value "Continue")
       (type "button")
       (id "nextButton"))))))))

(define target-path (build-path htdocs-path "t2.html"))
(delete-file target-path)
(srl:sxml->html parsed target-path)