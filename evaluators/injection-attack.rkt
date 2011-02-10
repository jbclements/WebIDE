#lang racket

(require web-server/servlet-env
         web-server/http/request-structs
         xml
         (planet dherman/json:3:0))


(define (start request)
  "Injection attack!!! </body><html>")


(serve/servlet start
               #:port 8071
               #:listen-ip #f
               #:launch-browser? #f
               #:servlet-path "/eval.rkt"
               #:log-file "/tmp/bogus-webide-backend-log")

