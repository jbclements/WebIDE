#lang racket

(provide (all-defined-out))

;; an evaluator is (struct string? (dict-of string? literal-or-userfield?))
(struct evaluator (url args) #:transparent)

;; a literal-or-userfield is either 
;; - (struct literal string?), or
;; - (struct userfield string?)
(struct literal (val) #:transparent)
(struct userfield (name) #:transparent)

