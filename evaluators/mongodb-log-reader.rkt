#lang racket

(require (planet jaymccarthy/mongodb)
         (planet dherman/json)
         web-server/http/request-structs
         net/url)

(define m (create-mongo))
(define d (mongo-db m "webideEvalLog"))
(define requests (mongo-collection d "requests"))
(define results (mongo-collection d "results"))


(define (extract-all-submissions)
  (for/list ([r (mongo-collection-find requests '())])
    (define request (hash-ref r 'request))
    (define bindings (hash-ref request 'bindings))
    (match bindings
      [(vector (hash-table ('id #"request")
                           ('value bytes)))
       (list (seconds->date (hash-ref r 'timestamp))
             (hash-ref request 'uri)
             (json->jsexpr (bytes->string/utf-8 bytes)))]
      [other (error 'extract-all-requests
                    "unexpected format for bindings: ~s" 
                    other)])))

(extract-all-submissions)