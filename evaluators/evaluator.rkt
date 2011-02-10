#lang racket

(require racket/sandbox
         "common.ss"
         rackunit)

(provide (all-defined-out))

;;
;; EVALUATION FRAMEWORK
;;

;; wrap a bunch of expressions as a module for use in an evaluator
(define (evaluator-module exps)
  `(module user lang/htdp-beginner
     (require rackunit/private/check)
     ,@exps))

;; the evaluator to be used for 
(define default-evaluator
  (parameterize ([sandbox-output 'string])
    (make-module-evaluator 
     (evaluator-module '()))))


;; when there are defns to be tested, we want a new 
;; module-evaluator that knows about them.
(define (fresh-evaluator exps)
  (parameterize ([sandbox-output 'string])
    (make-module-evaluator (evaluator-module exps))))

(define (read-all text)
  (with-input-from-string text
    (lambda ()
      (parameterize ([read-decimal-as-inexact #f])
        (with-handlers ([exn:fail:read? (lambda (exn)
                                          (read-error (exn-message exn)))])
          (read-stxs
           (let loop ()
             (let ([r (read-syntax)])
               (cond [(eof-object? r) null]
                     [else (cons r (loop))])))))))))

;; evaluate the stxs in the default evaluator (or the given one, if supplied)
(define (my-eval stxs pre-defs)
  (let ([evaluator (cond [(empty? pre-defs) default-evaluator]
                         ;; this could be hashed:
                         [else (fresh-evaluator pre-defs)])])
  (with-handlers ([exn:fail? (lambda (exn) (error-result (exn-message exn) (get-output evaluator)))]
                  [(lambda (exn) #t) (lambda (exn) (error-result (format "~v" exn)))])
    (let ([results (map evaluator stxs)])
      (values-result results (get-output evaluator))))))

