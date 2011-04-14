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

(printf "number of results: ~s\n" (mongo-collection-count results))

#;(define (extract-failing-submissions)
  (for/list ([r (mongo-collection-find results '())])
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
      [other (list (hash-ref r 'timestamp)
                   (list 'empty-bindings)
                   (hash))])))

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

(define probs (group-by-problem (extract-all-submissions)))




(define (process-problem p)
  (match p
    [(list (list uri args "First C Lab") submissions)
     (list (list uri args "First C Lab")
           (collect-with-stats submissions))]
    [other
     (fprintf (current-error-port)
              "ignoring problem: ~s\n" (first p))]))

(define (collect-with-stats submissions)
  (define ht
    (for/fold ([ht (hash)])
      ([s (in-list submissions)])
      (match s 
        [(list timestamp (list (list dc text)))
         (hash-set ht text (+ 1 (hash-ref ht text 0)))]
        [other 
         (error 'collect-with-stats "unexpected texts: ~s" other)])))
  (sort (hash-map ht list) > #:key second))


