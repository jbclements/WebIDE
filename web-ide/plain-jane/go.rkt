#lang racket

(require web-server/servlet-env
         "page-displayer.rkt")

;; the initial startup:
(define (start dc)
  (run-from-url (request-url-from-user)))

(serve/servlet start
               #:port 8025
               #:listen-ip #f
               #:command-line? #t)