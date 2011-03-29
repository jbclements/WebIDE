#lang racket

(require (planet jaymccarthy/mongodb)
         (planet dherman/json)
         srfi/19
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
       (define jsexpr (json->jsexpr (bytes->string/utf-8 bytes)))
       (match jsexpr
         [(hash-table ('id id)
                      ('args args)
                      ('textfields textfields))
          (list (hash-ref r 'timestamp)
                (list (hash-ref request 'uri)
                      args
                      id)
                textfields)]
         [other
          (list (hash-ref r 'timestamp)
                (list 'bogus-problem
                      other)
                (hash))])]
      [other (error 'extract-all-requests
                    "unexpected format for bindings: ~s" 
                    other)])))

(define (group-by-problem submissions)
  (define ht
    (for/fold ([ht (hash)])
      ([p (in-list submissions)])
      
      (hash-set ht (second p) (cons (list (date->string
                                           (time-utc->date (first p))
                                           "~4")
                                          (hash-map (third p) list))
                                    (hash-ref ht (second p) empty)))))
  (hash-map ht list))

(group-by-problem (extract-all-submissions))