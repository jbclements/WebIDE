#lang racket

;; Copyright 2010-2014 John Clements (clements@racket-lang.org)

;; NB: THIS FILE DUPLICATED FROM WEB-IDE TRAC SUBVERSION TREE

(require racket/runtime-path
         sxml)

(provide validate-path
         validate-sxml)

(define-runtime-path here ".")

(define verbose-mode? (make-parameter #f))

(define jing-path (path->string (build-path here "jing.jar")))

(unless (file-exists? jing-path)
  (error 'validate "abort! can't find jing.jar in same directory as script"))

(define rnc-path (path->string (build-path here "local-webide.rnc")))

(unless (file-exists? rnc-path)
  (error 'validate "abort! can't find local-webide.rnc in same directory as script"))

;; run jing on the given path's xml file.
(define (validate-path path #:verbose? [verbose? #f])
  (when verbose?
    (printf "checking file: ~a\n" path))
  (system (format "java -jar \"~a\" -c \"~a\" \"~a\""
                  jing-path
                  rnc-path
                  (path->string path))))

(define (validate-sxml sxml)
  (define temp-file (make-temporary-file))
  (call-with-output-file 
      temp-file
      #:exists 'truncate
      (lambda (port)
        (srl:sxml->xml sxml port)))
  (validate-path temp-file))

(module+ test
  (require rackunit)
  
(check-equal? (validate-sxml 
               '(*TOP* 
                 (@ (*NAMESPACES* (ns "http://www.web-ide.org/namespaces/labs/1")))
                 (ns:lab (@ (name "labz")))))
              #t))
