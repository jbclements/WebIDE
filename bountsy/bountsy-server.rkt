#lang racket

(require web-server/servlet-env
         web-server/http
         web-server/templates
         net/url
         racket/runtime-path)

(define-runtime-path here ".")

;; this function responds to any request
(define (start request)  
  (match (url-path (request-uri request))
    ;; is the path "bountsy" followed by another local path?
    [(list (path/param "bountsy" '()) (path/param evaluator-localpath '()))
     (define post-text (request-post-data/raw request))
     (log-info (format "post-text: ~s\n" post-text))
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
    [other (error 'start "unexpected(ly long) path: ~s" other)]))

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
               #:launch-browser? #f
               #:servlet-path "/startup.html" ;; actually *not* the servlet
               #:servlet-regexp #px"^/bountsy/.*" ;; capture bountsy paths
               #:server-root-path here
               #:log-file "webide-backend-webserver-log")