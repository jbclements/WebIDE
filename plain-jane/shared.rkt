#lang racket

(provide (all-defined-out))

;; an evaluator is (struct string? (string->string assoc) (string->string assoc))
(struct evaluator (url args textfields) #:transparent)

;; a response from an evaluator:
(struct success () #:transparent)
(struct failure (msg) #:transparent)
(struct serverfail (msg) #:transparent)

