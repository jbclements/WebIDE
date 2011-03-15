#lang racket

(require (planet clements/sxml2)
         rackunit
         "apat.rkt"
         "shared.rkt")

(provide port->xml
         xml->steps
         pcon
         step->evaluators)

(define webide-namespace "http://www.web-ide.org/namespaces/labs/1")

(define webide-ns `((w1 . ,webide-namespace)))

;; read the lab xml from a port
(define (port->xml port)
  (ssax:xml->sxml port webide-ns))

;; extract the steps from a lab *that uses w1 as a namespace prefix*
(define (xml->steps lab-sxml)
  ((sxpath '(w1:lab w1:step)) lab-sxml))

;; turn a step into html
(define (pcon content)
  (pre-post-order content stylesheet))

;; this stylesheet provides transformations from 
;; webide XML elements to HTML elements
(define stylesheet
  (list
   [apat (w1:step (name . others) . content)
         `(div (h3 "step name: " ,name) . ,content)]
   ;; eat the evaluators for display
   [apat (w1:evaluator any . content)
         ""]
   ;; eat the buttons for now...
   [apat (w1:button attrs . elts)
         "*THERE WOULD BE A BUTTON HERE*"]
   ;; userfields become textareas
   [apat (w1:userfield (id . others) . content)
         `(textarea  (@ (name ,id))
                     "" ,@content)]
   ;; don't process attributes or text
   `(@ *preorder* . ,(lambda args args))
   `(*text* . ,(lambda (t a) a))
   `(*default* . ,(lambda (tag . elts) 
                    (cons (strip-tag tag) elts)))))


;; version 1 stylesheet:
#;(define stylesheet
  (list
   [apat (w1:step (name . others) . content)
         `(div (h3 "step name: " ,name) . ,content)]
   ;; eat the evaluators
   [apat (w1:evaluator any . content)
         ""]
   ;; segments become textareas
   [apat (w1:segment (id width height . others) . content)
         `(textarea  (@ (name ,id)
                        (width ,width)
                        (height ,height))
                     "" ,@content)]
   ;; tables
   [apat (w1:labtable (rows cols . otherattrs) . elts)
         `(table (@ (rows ,rows) (cols ,cols))
                 ,@(regroup rows cols elts))]
   [apat (w1:add attrs . content)
         `(td . ,content)]
   ;; code tag becomes pre tag
   [apat (w1:code attrs . content)
         `(pre (@ ,@attrs) ,@content)]
   ;; don't process attributes or text
   `(@ *preorder* . ,(lambda args args))
   `(*text* . ,(lambda (t a) a))
   `(*default* . ,(lambda (tag . elts) 
                    (cons (strip-tag tag) elts)))))


;; regroup : string string (listof sxml) -> (listof td)
(define (regroup rows cols elts)
  (define width (string->number cols))
  (unless (andmap (tag-checker 'td) elts)
    (error 'process-content "expected only cells in table, got: ~a" elts))
  (unless (integer? (/ (length elts) width))
    (error 'process-content "number of table entries (~a) is not divisible by number of table columns (~a)" (length elts) width))
  (unless (integer? (/ (length elts) (string->number rows)))
    (error 'process-content "number of table entries (~a) is not divisible by number of table rows (~a)" (length elts) (string->number rows)))
  (let loop ([left elts])
    (cond [(empty? left) empty]
          [else (cons `(tr . ,(take left width))
                      (loop (drop left width)))])))


;; extracting evaluators:

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



;; extract an attribute representing a number
(define (num-attr attrs key)
  (match (assoc key attrs)
    [`(,dc ,num-str) (string->number num-str)]
    [other (error 'num-attr "no ~a attribute found among ~a" key attrs)]))

;; tag-checker : tag -> element -> boolean?
(define ((tag-checker tag) element)
  (and (list? element) (equal? (first element) tag)))

;; strip the colon-separated part of a symbol off.
(define (strip-tag s)
  (match (regexp-match #px"[^:]*:(.*)" (symbol->string s))
    [(list match rhs) (string->symbol rhs)]
    [false (error 'strip-tag "tag without prefix: ~a" s)]))

;; TEST CASES

(check-equal? (num-attr '((a "13") (b "14")) 'a) 13)
(check-equal? (num-attr '((a "13") (b "14")) 'b) 14)


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

(check-equal? (strip-tag 'w1:br) 'br)

(check-equal? ((tag-checker 'w1:evaluator) `(3 w1:evaluator)) #f)
(check-equal? ((tag-checker 'w1:evaluator) `(w1:evaluator (@ (abc "def")))) #t)

(check-equal? (pcon `(w1:b (w1:i "zoobah")))
              `(b (i "zoobah")))

(check-equal? (pcon `(w1:b (w1:evaluator (@ (name "bigbug")))))
              `(b ""))

(check-equal? (pcon `(w1:b (@ (awesomeness "35")) "trip"))
              `(b (@ (awesomeness "35")) "trip"))





(check-equal? (pcon `(w1:code "abc
def"))
              `(code "abc
def"))


(check-equal? (pcon `(w1:step (@ (name "bob")) "a step"))
              `(div (h3 "step name: " "bob")
                    "a step"))


(check-equal? (xml->steps `(*TOP* (@ (*NAMESPACES* (w1 ,webide-namespace)))
                                  (w1:lab (w1:step "abc") (w1:step "def"))))
              '((w1:step "abc") (w1:step "def")))

;; tests for version 1 spec
#;(check-equal? (pcon `(w1:add "bc"))
              `(td "bc"))

#;(check-equal? (regroup "3" "2" 
                       `((td "a1")
                         (td "a2")
                         (td "a3")
                         (td "a4")
                         (td "a5")
                         (td "a6")))
              `((tr (td "a1") (td "a2"))
                (tr (td "a3") (td "a4"))
                (tr (td "a5") (td "a6"))))

#;(check-equal? (pcon `(w1:labtable (@ (rows "2") (cols "1"))
                                  (w1:add "a")
                                  (w1:add "b")))
              `(table (@ (rows "2") (cols "1"))
                      (tr (td "a"))
                      (tr (td "b"))))

#;(check-equal? (pcon `(w1:segment (@ (id "boo")(width "20") (height "1"))))
              `(textarea (@ (name "boo")(width "20") (height "1")) ""))


