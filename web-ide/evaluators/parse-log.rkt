#lang racket


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

(define a (file->value "/tmp/report-1.txt"))

(map process-problem a)
