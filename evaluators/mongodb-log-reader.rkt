#lang racket

(require (planet jaymccarthy/mongodb)
         srfi/19
         json
         (planet williams/science/statistics))

(define m (create-mongo))
(define d (mongo-db m "webideEvalLog"))
(define requests (mongo-collection d "requests"))
(define results (mongo-collection d "results"))

(printf "number of requests: ~s\n" (mongo-collection-count requests))
(printf "number of results: ~s\n" (mongo-collection-count results))

;; -> (listof (list/c timestamp duration result problem textfields))
(define (extract-result-request-pairs)
  (for/list ([r (mongo-collection-find results '())])
    (define result-timestamp (hash-ref r 'timestamp))
    (match (for/list ([r (mongo-collection-find requests
                                                `((_id . ,(hash-ref r 'request-id))))])
             r)
      [(list request)
       (define time-diff 
         (time-difference (hash-ref request 'timestamp) result-timestamp))
       (define time-diff/seconds (+ (time-second time-diff)
                                    (/ (time-nanosecond time-diff)
                                       1000000000)))
       `(,result-timestamp
         ,time-diff/seconds
         ,(hash-ref r 'result)
         ,@(parse-request (hash-ref request 'request)))])))


;; the ids of the requests that had logged results
(define successful-request-ids
  (for/list ([r (mongo-collection-find results '())])
    (hash-ref r 'request-id)))

(define unsuccessful-requests
  (for/list ([r (mongo-collection-find requests '())]
             #:when (not (member (hash-ref r '_id) successful-request-ids)))
    r))

;; request-hash -> (list problem textfields)
(define (parse-request r-hash)
  (when (not (hash-has-key? r-hash 'bindings))
    (error 'uhoh "r-hash: ~s" r-hash))
  (define bindings (hash-ref r-hash 'bindings))
  (match bindings
    [(vector (hash-table ('id #"request")
                         ('value bytes)))
     (define jsexpr (with-handlers
                        ([exn:fail? (lambda (exn) #f)])
                        (string->jsexpr (bytes->string/utf-8 bytes))))
     (match jsexpr
       [(hash-table ('id id)
                    ('args args)
                    ('textfields textfields))
        (list (list (hash-ref r-hash 'uri)
                    args
                    id)
              textfields)]
       [other
        (list (list 'bogus-uri
                    'bogus-args
                    'bogus-id)
              (hash))])]
    [other (list (list 'totally-bogus-uri
                       'totally-bogus-args
                       'totally-bogus-id)
                 (hash))]))


(define submissions (extract-result-request-pairs))

(define durations (map second submissions))
(printf "mean and variance of latency:")
(mean-and-variance durations)



;; (submission -> any) (listof submissions) (listof (list/c any (list submissions)))
(define (group-by key-finder submissions)
  (define ht
    (for/fold ([ht (hash)])
      ([p (in-list submissions)])
      (define key (key-finder p))
      (hash-set ht key (cons p
                             (hash-ref ht key empty)))))
  (hash-map ht list))

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

(define labs (group-by (lambda (x) (third (fourth x))) submissions))
(printf "number of labs: ~s\n" (length labs))
(printf "lab ids:\n")
(map first labs)
(printf "sizes:\n")
(map length (map second labs))

(define (successful? p) 
  (match (third p)
    [(hash-table ('status "success")
                 ('message m)) #t]
    [other #f]))

(define (lab-info lab-pair)
  (define name (first lab-pair))
  (define submissions (second lab-pair))
  (define times (map first submissions))
  (define sorted (sort times time<=?))
  (define earliest (first sorted))
  (define latest (last sorted))
  (define by-problem (group-by fourth submissions))
  (define problem-counts (map (lambda (pg) (list (first pg) (length (second pg))
                                                 (length (filter successful? (second pg)))))
                              by-problem))
  (list name
        (length submissions)
        (length by-problem)
        problem-counts
        (date->string (time-utc->date earliest) "~4")
        (date->string (time-utc->date latest) "~4")))

(define labs-of-interest
  (map (lambda (name) (assoc name labs))
       (list "Day" #;"First C Lab" "selection" "Facebook: Selection")))

(map lab-info labs-of-interest)

(define by-problem (group-by fourth submissions))

(define sample-problem-1 
  (assoc '("/c-parser-match" #hasheq((pattern . "(number % 7) == 0")) "Day")
         by-problem))

(define sample-fails (filter (lambda (x) (not (successful? x))) (second sample-problem-1)))

(length sample-fails)
(define grouped-fails (group-by (lambda (x) x) (map fifth sample-fails)))
(map (lambda (x)
       (list (second x)
             (first x)))
     (sort (map (lambda (x) (list (match-let ([(hash-table ('divisible str)) (first x)])
                                    str) 
                                  (length (second x)))) grouped-fails)
           >
           #:key second))





#|
(define probs (group-by-problem submissions))

(printf "number of problems: ~s\n" (length probs))


(define lab-ids (remove-duplicates
                 (map third (map first probs))))
(printf "ids:\n")
lab-ids

()


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


|#
