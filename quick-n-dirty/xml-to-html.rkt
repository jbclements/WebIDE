#lang racket

(require (planet clements/sxml2)
         (except-in rackunit foldts)
         "apat.rkt")

(provide path->xml
         xml->steps
         step->html
         pcon)

(define (xml->steps lab-xml)
  ((sxpath '(w1:lab w1:step)) lab-xml))

(define (path->xml path)
  (call-with-input-file path
    (lambda (port)
      (ssax:xml->sxml port `((w1 . "http://www.web-ide.org/namespaces/labs/1"))))))


(define stylesheet
  (list
   [apat
    (w1:step (name . others) . content)
    `(div (h3 "step name: " ,name) . content)]
   [apat
    (w1:evaluator any . content)
    ""]
   [apat
    (w1:segment (width height . others) . content)
    `(textarea  (@ (width ,width)
                   (height ,height))
                "" ,@content)]
   `(*default*
     .
     (lambda args args))))

(define (pcon content)
  (pre-post-order content stylesheet))



;; mush the xml into plausible html:
(define (process-content content)
  ;; for now, just strip the namespace off:
  (match content
    ;; don't display evaluators.
    [`(w1:evaluator ,stuff ...) ""]
    [`(w1:segment ,contents ...)
     (match contents
       [`((@ ,attrs ...) ,(? string? strs) ...)
        `(textarea (@ (width ,(first (dict-ref attrs 'width)))
                      (height ,(first (dict-ref attrs 'height))))
                   "" ,@strs)]
       ;; ignore the bogus ones that just hold evaluators:
       [`((@ ,attrs ...) ,(? (tag-checker 'w1:evaluator) e) ...) ""]
       [other (error 'process-content
                     "strange segment: ~a" content)])]
    ;; rewrite grody GWT tables into HTML ones:
    [`(w1:labtable (@ ,attrs ...) ,adds ...)
     (process-table attrs adds)]    
    [`(,tag (@ ,attrs ...) ,content ...)
     `(,(strip-tag tag) (@ ,@attrs) ,@(map process-content content))]
    [`(,tag ,content ...)
     `(,(strip-tag tag) ,@(map process-content content))]    
    [other other]))

;; process-table : attr-list element-list -> sxml
(define (process-table attrs adds)
  (define height (num-attr attrs 'rows))
  (define width (num-attr attrs 'cols))
  (unless (andmap (tag-checker 'w1:add) adds)
    (error 'process-content "expected only adds in table, got: ~a" adds))
  (unless (integer? (/ (length adds) width))
    (error 'process-content "number of table entries (~a) is not divisible by number of table columns (~a)" (length adds) width))
  (unless (integer? (/ (length adds) height))
    (error 'process-content "number of table entries (~a) is not divisible by number of table rows (~a)" (length adds) height))
  (define grouped (let loop ([left adds])
                    (cond [(empty? left) empty]
                          [else (cons (process-table-row (take left width))
                                      (loop (drop left width)))])))
  `(table (@ (rows ,(number->string height))
             (cols ,(number->string width)))
          ,@grouped))


;; process-table-row : elements -> sxml
(define (process-table-row elements)
  (define cells (map process-table-cell elements))
  `(tr ,@cells))

(define (process-table-cell element)
  (match element
    [`(w1:add (@ ,attrs ...) ,elts ...)
     `(td (@ ,attrs ...) ,@(map process-content elts))]
    [`(w1:add ,elts ...)
     `(td ,@(map process-content elts))]))

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


(check-equal? (num-attr '((a "13") (b "14")) 'a) 13)
(check-equal? (num-attr '((a "13") (b "14")) 'b) 14)



(check-equal? (strip-tag 'w1:br) 'br)

(check-equal? ((tag-checker 'w1:evaluator) `(3 w1:evaluator)) #f)
(check-equal? ((tag-checker 'w1:evaluator) `(w1:evaluator (@ (abc "def")))) #t)

(check-equal? (process-content `(w1:b (w1:i "zoobah")))
              `(b (i "zoobah")))

(check-equal? (process-content `(w1:b (@ (awesomeness "35")) "trip"))
              `(b (@ (awesomeness "35")) "trip"))

(check-equal? (process-table-cell `(w1:add "bc"))
              `(td "bc"))

(check-equal? (process-table-row `((w1:add "bc") (w1:add "de")))
              `(tr (td "bc") (td "de")))

(check-equal? (process-table `((rows "3") (cols "2"))
                             `((w1:add "a1")
                               (w1:add "a2")
                               (w1:add "a3")
                               (w1:add "a4")
                               (w1:add "a5")
                               (w1:add "a6")))
              `(table (@ (rows "3") (cols "2"))
                      (tr (td "a1") (td "a2"))
                      (tr (td "a3") (td "a4"))
                      (tr (td "a5") (td "a6"))))




(check-equal? (process-content `(w1:segment (@ (width "20") (id "fresh-id-1") (height "1"))))
              `(textarea (@ (width "20") (height "1")) ""))



(check-equal? (pcon `(w1:segment (@ (width "20") (id "fresh-id-1") (height "1"))))
              `(textarea (@ (width "20") (height "1")) ""))

