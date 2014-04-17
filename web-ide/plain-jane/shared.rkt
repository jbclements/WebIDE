#lang racket

(provide (all-defined-out))

;; a response from an evaluator:
(struct success () #:prefab)
(struct failure (msg) #:prefab)
(struct serverfail (msg) #:prefab)

