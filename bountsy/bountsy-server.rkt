#lang racket

(require web-server/servlet-env
         web-server/http
         web-server/templates
         net/url)

(define the-url "http://localhost:29387/bountsy/alwaysSucceed")

;; this function responds to any request
(define (start request)  
  (match (url-path (request-uri request))
    ;; is the path "bountsy" followed by another local path?
    [(list (path/param "bountsy" '()) (path/param evaluator-localpath '()))
     ;; bounce the given POST text through to the evaluator
     (define evaluator-response
       (first
        (regexp-match
         #px".*"
         (post-pure-port (string->url (string-append
                                       "http://brinckerhoff.org:8025/"
                                       evaluator-localpath))                         
                         (request-post-data/raw request)))))
     ;; ... and return it:
     (response/plain evaluator-response)]
    ;; default: serve initial page
    [other (response/plain 
            (string->bytes/utf-8 (include-template "index.html")))]))

(define (response/plain text)
  (response/full
      200 #"Okay"
      (current-seconds)
      TEXT/HTML-MIME-TYPE
      empty
      (list text)))

(serve/servlet start
               #:port 29387
               #:listen-ip #f
               ;;#:launch-browser? #f
               #:servlet-regexp #px""  ;; trivially succeeds
               #:log-file "webide-backend-webserver-log")