#lang setup/infotab

(define collection 'multi)

(define deps (list
              "base"
              "math-lib"
              "parser-tools-lib"
              "rackunit-lib"
              "srfi-lite-lib"
              "web-server-lib"
              "java" "c-utils" "sxml" "mongodb"))

(define build-deps
  (list "at-exp-lib"))