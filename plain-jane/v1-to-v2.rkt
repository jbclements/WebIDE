#lang racket

;; STILL EXPERIMENTAL

(require sxml
         "apat.rkt"
         "../../trac-webide/labs/validate-lib.rkt")

(module+ test (require rackunit))

;; NB: currently, the lab author must re-insert buttons where
;; they belong...

(define webide-1-namespace "http://www.web-ide.org/namespaces/labs/1")
(define webide-2-namespace "http://www.web-ide.org/namespaces/labs/2")

(define webide-ns `((w1 . ,webide-1-namespace)
                    (w2 . ,webide-2-namespace)))

;; read the lab xml from a port
(define (port->xml port)
  (ssax:xml->sxml port webide-ns))





;; extract the steps from a lab *that uses w1 as a namespace prefix*
(define (xml->steps lab-sxml)
  ((sxpath '(w1:lab w1:step)) lab-sxml))

;; rewrite a step:
(define (rewrite-step step)  
  (define evaluator-table (step->evaluator-table step))
  (define new-evaluators 
    (evaluator-table->new-evaluators evaluator-table))
  (match step
    [`(w1:step (@ . ,attrs) . ,content)
     (match (dict-ref attrs 'buttonName #f)
       ;; no buttonName specified
       [#f 
        `(w2:step (@ ,@attrs) 
                  ,@new-evaluators
                  (w2:content . ,content))]
       [(list name)
        (define other-attrs (dict-remove attrs 'buttonName))
        (unless (= (length evaluator-table) 1)
          (error 'rewrite-step
                 "step with buttonName must have exactly one evaluator, given: ~e"
                 step))
        (define evaluator (first evaluator-table))
        (unless (= (count-segments step) 0)
          (error 'rewrite-step
                 "step with buttonName must not have segments, given: ~e"
                 step))
        ;; check for no segments...
        (define new-button
          `(w2:button
            (@ (label ,name))
            (w2:evaluatorName ,(second evaluator))))
        `(w2:step (@ ,@other-attrs)
                  ,@new-evaluators
                  (w2:content . ,(append content
                                         (list new-button))))])]))


;; extract the evaluators from a step, and construct 
;; a table mapping them to names
(define (step->evaluator-table step)
  (define evaluators (extract-evaluators step))
  (for/list ([i (in-naturals)]
             [evaluator evaluators])
    (list evaluator (format "evaluator~s" i))))

;; given a table of evaluators, construct the top-level-style
;; evaluators that correspond to them.
(define (evaluator-table->new-evaluators evaluator-table)
  (for/list ([evaluator-and-num evaluator-table])
    (match-define (list evaluator name) evaluator-and-num)
    (match evaluator
      [`(w1:evaluator (@ . ,(list-no-order 
                             (list 'name old-name)
                             (list 'labid old-labid)
                             (list 'href href))) 
                      . ,segs-and-args)
       `(w2:evaluator (@ (name ,name) (href ,href))
                      ;; FIXME!
                      ;; these will need to be reformatted:
                      . ,(map rewrite-arg segs-and-args))]
      [other (error 'evaluator-rewrite
                    "evaluator didn't match spec: ~e" 
                    other)])))

;; rewrite the segids and args of v1. Note that this may fail
;; to validate if the fixedArgs don't precede the userfield args.
(define (rewrite-arg arg)
  (match arg
    [`(w1:arg . ,args)
     `(w2:fixedArg . ,args)]
    [`(w1:segid (w1:id ,id))
     ;; NB: the lab author can pick a better name here!
     `(w2:userfieldArg (w2:name "userfield")
                       (w2:value ,id))]))

;; version 1 stylesheet:
(define v1-stylesheet
  (list
   `(w1:step *macro*
             . ,(lambda elts
                 (rewrite-step elts)))
   ;; eat the evaluators... now done in rewrite-step
   [apat (w1:evaluator attrs . content)
         `(eat-me-later)]
   ;; segments become userfields
   [apat (w1:segment attrs . content)
         
         (match (dict-ref attrs 'buttonName #f)
           [#f `(w2:userfield
                 (@ . ,attrs)
                 ;; ...gnarr! what do evaluators in here mean?
                 ,@content
                 )])
         #;`(textarea  (@ (name ,id)
                        (width ,width)
                        (height ,height))
                     "" ,@content)]
   ;; tables
   [apat (w1:labtable (rows cols . otherattrs) . elts)
         `(w2:table (@ . ,otherattrs)
                 ,@(regroup rows cols elts))]
   [apat (w1:add attrs . content)
         `(w2:td . ,content)]
   ;; code tag becomes pre tag
   [apat (w1:code attrs . content)
         `(w2:pre (@ ,@attrs) ,@content)]
   ;; hint isn't supported, but mostly isn't used....
   [apat (w1:hint attrs . content)
         (begin
           (when (not (empty? attrs))
             (fprintf (current-error-port)
                      "DISCARDING HINT ATTRIBUTES: ~e" attrs))
           (when (not (empty? content))
             (fprintf (current-error-port)
                      "DISCARDING HINT content: ~e" attrs))
           `(eat-me-later))]
   ;; don't process attributes or text
   `(@ *preorder* . ,(lambda args args))
   `(*text* . ,(lambda (t a) a))
   `(*default* . 
               ,(lambda (tag . elts) 
                    (cons (rewrite-tag tag) elts)))))


;; return all of the evaluators contained in an element
(define (extract-evaluators elt)
  ((sxpath '(// w1:evaluator) '()) `(*TOP* ,elt)))

;; count the number of segments contained in an element
(define (count-segments elt)
  (length ((sxpath '(// w1:segment) '()) `(*TOP* ,elt))))


;; regroup : string string (listof sxml) -> (listof td)
(define (regroup rows cols elts)
  (define width (string->number cols))
  (unless (andmap (tag-checker 'w2:td) elts)
    (error 'process-content "expected only cells in table, got: ~e" elts))
  (unless (integer? (/ (length elts) width))
    (error 'process-content 
           "number of table entries (~a) is not divisible by number of table columns (~a)"
           (length elts) width))
  (unless (integer? (/ (length elts) (string->number rows)))
    (error 'process-content 
           "number of table entries (~a) is not divisible by number of table rows (~a)"
           (length elts) (string->number rows)))
  (let loop ([left elts])
    (cond [(empty? left) empty]
          [else (cons `(w2:tr . ,(take left width))
                      (loop (drop left width)))])))


;; extract an attribute representing a number
(define (num-attr attrs key)
  (match (assoc key attrs)
    [`(,dc ,num-str) (string->number num-str)]
    [other (error 'num-attr "no ~a attribute found among ~a" key attrs)]))

;; tag-checker : tag -> element -> boolean?
(define ((tag-checker tag) element)
  (and (list? element) (equal? (first element) tag)))

;; change w1: into w2:
(define (rewrite-tag s)
  (match (regexp-match #px"([^:]*):(.*)" (symbol->string s))
    [(list match "w1" rhs) (string->symbol (format "w2:~a" rhs))]
    [(list match "w2" rhs) s]
    #;[(list match lhs rhs) (string->symbol rhs)]
    [false (error 'strip-tag "tag without prefix: ~a" s)]))

;; TEST CASES
(module+ test 
(check-equal? (num-attr '((a "13") (b "14")) 'a) 13)
(check-equal? (num-attr '((a "13") (b "14")) 'b) 14)



(check-equal? ((tag-checker 'w1:evaluator) `(3 w1:evaluator)) #f)
(check-equal? ((tag-checker 'w1:evaluator) `(w1:evaluator (@ (abc "def")))) #t)


(check-equal? (xml->steps `(*TOP* (@ (*NAMESPACES* (w1 ,webide-1-namespace)))
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



)


;; extract the top-level "lab" element.
(define (extract-lab lab)
  (match ((sxpath '(w1:lab)) lab)
    [(list l) l]
    [other (error 'aontehu)]))

;; wrap a *TOP* element back around the tree.
(define (wrap-with-top element)
  `(*TOP* (@
           (*NAMESPACES* 
            #;(w1 "http://www.web-ide.org/namespaces/labs/2")
            (w2 "http://www.web-ide.org/namespaces/labs/2")))
          ,element))

#;(when (file-exists? "/tmp/a.xml")
  (delete-file "/tmp/a.xml"))


;; rewrite the lab from sxml to sxml
(define (rewrite lab)
  ((sxml:modify '("//eat-me-later" delete))
   (wrap-with-top
    (pre-post-order
     (extract-lab lab)
     v1-stylesheet))))

;; COPIED FROM VALIDATE.RKT:

;; return #true for xml paths
(define (xml-path? f)
  (regexp-match #px".*\\.xml$" (path->string f)))

;; return #true for paths beginning with "broken"
;; NB: assumes that the path contains just a file name!
(define (broken-test? f)
  (regexp-match #px"^broken" (path->string f)))

;; check all files in a directory
(define (translate-dir dir)
  (for ([f (in-list (find-files xml-path? dir))])
    (define-values (base name must-be-dir?) (split-path f))
    (when (and (not must-be-dir?)
               (not (symbol? name))
               (xml-path? name)
               (not (broken-test? name)))
      (printf "translating file: ~s\n" f)
      (define sxml
        (call-with-input-file f
          (lambda (p)
            (port->xml p))))
      ;; blecch!
      (with-handlers
          ((exn:fail?
            (lambda (exn)
              (display (exn-message exn) (current-error-port))
              #f)))
      (define cleaned
        ((sxml:modify '("//eat-me-later" delete))
         ((sxml:modify '("//eat-me-later" delete))
          ((sxml:modify '("//eat-me-later" delete))
           (rewrite sxml)))))
      (display-to-file (srl:sxml->xml cleaned)
                       (build-path base 
                                   "translated-labs"
                                   name))))))

(translate-dir "/Users/clements/trac-webide/labs/")

#;(define lab1
  (call-with-input-file "/Users/clements/trac-webide/labs/arrays.xml"
  (lambda (p)
    (port->xml p))))



#;processed2
#;(validate-sxml processed2)

(module+ test
  
  (check-equal? 
   (count-segments 
    `(w1:lab
       (@ (name "Build an android apk"))
       (w1:description "\nBuilding an android apk.\n")
       (w1:step
        (@ (name "Test test") (buttonName "Compile apk"))
        "\nBuild the android apk.\n "
        (w1:segment "foo")
        (w1:evaluator
         (@
          (name "Test")
          (labid "test")
          (href
           "http://anoteuh")))
        (w1:segment "bar"))))
   2)
  
  (define t1 
    '(*TOP*
      (@
       (*NAMESPACES*
        (w1 "http://www.web-ide.org/namespaces/labs/1")
        (w2 "http://www.web-ide.org/namespaces/labs/2")))
      (*PI* xml "version=\"1.0\"")
      (w1:lab
       (@ (name "Build an android apk"))
       (w1:description "\nBuilding an android apk.\n")
       (w1:step
        (@ (name "Test test") (buttonName "Compile apk"))
        "\nBuild the android apk.\n "
        (w1:evaluator
         (@
          (name "Test")
          (labid "test")
          (href
           "http://anoteuh")))))))
  (check-true (validate-sxml (rewrite t1)))
  (check-equal? 
   (rewrite t1)
   '(*TOP*
     (@ (*NAMESPACES* (w2 "http://www.web-ide.org/namespaces/labs/2")))
     (w2:lab
      (@ (name "Build an android apk"))
      (w2:description "\nBuilding an android apk.\n")
      (w2:step
       (@ (name "Test test"))
       (w2:evaluator
        (@
         (name "evaluator0")
         (href
          "http://anoteuh")))
       (w2:content
        "\nBuild the android apk.\n "
        (w2:button (@ (label "Compile apk")) (w2:evaluatorName "evaluator0")))))))
  
  
  
  
  (define zombie-node '(zombie))
(define t3 
  `(*TOP*
    (body
     (a ,zombie-node
        13
        ,zombie-node)
     (b ,zombie-node
        ,zombie-node))))
(check-equal? ((sxml:modify '("//zombie" delete)) t3)
              `(*TOP* (body (a) (b))))
  )

