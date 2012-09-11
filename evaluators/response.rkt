#lang racket

(require xml
         json)

(provide failure success internal-error)

;; create a string indicating failure:
(define (failure xexpr) (result "failure" (xexpr->string xexpr)))

;; create a string indicating success:
(define (success xexpr) (result "success" (xexpr->string xexpr)))

;; create a string indicating an internal error:
(define (internal-error xexpr) (result "internal-error" (xexpr->string xexpr)))

(define (result tag text)
  (jsexpr->string (make-immutable-hasheq `((tag . ,tag) (text . ,text)))))