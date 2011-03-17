#lang racket

(require web-server/servlet-env
         web-server/http/request-structs
         web-server/http/response-structs
         net/url
         (planet dherman/json:3:0)
         "java-header-example.rkt"
         "c-evaluator-call.rkt"
         rackunit
         "shared.rkt"
         "transport.rkt")

(define (result tag text)
  (response/full
   200
   #"Okay"
   (current-seconds)
   TEXT/HTML-MIME-TYPE ;; should change? maybe not.
   empty
   (list
    (string->bytes/utf-8 
     (jsexpr->json 
      (make-immutable-hasheq `((status . ,tag) (message . ,text))))))))

(define (start request)
  (let/ec abort
    ;; no exceptions allowed to pass:
    (with-handlers ([(lambda (x) #t)
                     (lambda (exn)
                       (log-and-return
                        abort
                        "serverfail"
                        (format "internal error: ~s" 
                                (exn-message exn))))])
      (log-debug "received request")
      (define uri (request-uri request))
      (define post-data (request-post-data/raw request))
      (define path-string (url-path->path abort (url-path uri)))
      (match-let* ([(list args-assoc textfields-assoc) 
                    (with-handlers 
                        ([exn:fail? 
                          (lambda (exn)
                            (log-and-return 
                             abort
                             "callerfail"
                             (format
                              "exception during decoding input data with message: ~a"
                              (exn-message exn))))])
                      (post-bytes->args-n-fields post-data))]
                   [(list (list required-args required-fields evaluator)) 
                    (dict-ref evaluator-table path-string
                              (lambda ()
                                (log-and-return
                                 abort
                                 "callerfail"
                                 (format 
                                  "unknown evaluator path: ~a"
                                  path-string))))])
        (check-fields abort uri "args" required-args args-assoc)
        (check-fields abort uri "textfields" required-fields textfields-assoc)
        (
         
         ;;add it here!
         (evaluator args-assoc textfields-assoc))))))

;; url-path->path : (listof path/param) -> string
(define (url-path->path aborter url-path)
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (log-and-return
                      aborter
                      "callerfail"
                      (format "exception during decoding path with message: ~a"
                              (exn-message exn))))])
    (apply string-append (add-between (map path/param->string url-path) "/"))))

;; path/param->string : turn a path/param into a string. Disallow
;; params and up/same components
(define (path/param->string pp)
  (match pp
    [(path/param (? string? str) '()) str]
    [(path/param dc1 dc2) 
     (error 'path/param->string 
            "path component ~s contains a non-string or has a parameter"
            pp)]
    [other (error
            'path/param->string
            "not a path/param: ~s" pp)]))


;; are any of the required fields or args missing?
;; check-fields : (any/c -> *doesn't return*) string? (listof string?) dict? -> void
(define (check-fields aborter uri kind-str required assoc)
  (define present (dict-map assoc (lambda (k v) k)))
  (define missing (remove* present required))
  (unless (empty? missing)
    (log-and-return aborter 
                    "callerfail"
                    (format "request for evaluator ~s is missing these required ~s: ~s"
                            (url->string uri)
                            kind-str
                            missing))))


;; log an error, return it to the user
(define (log-and-return escaper status message)
  (log-error message)
  (escaper (result status message)))

(define (approx-age-header-checker args textfields)
  (match (dict-map textfields (lambda (k v) v))
    [(list firstline)
     (match (birth-year-example firstline)
       ['success (result "success" "success")]
       [(? string? msg) (result "failure" msg)])]
    [other (result "callerfail" "request must have exactly one text field")]))


(define evaluator-table
  ;; each entry requires a path-string and a list containing the list of required arguments,
  ;; the list of required text fields, and the function
  ;; that handles the evaluation.
  `(("getApproxAgeHeader" (() () ,approx-age-header-checker))
    ("alwaysSucceed" (() () ,(lambda (dc1 dc2) (result "success" "success"))))
    ("any-c-int" (() () ,any-c-int))
    ("any-c-addition" (() () ,any-c-addition))
    ("c-parser-match" ((pattern) () ,c-parser-match))))


(define (run-tests)
  (check-equal? (url-path->path
                 (lambda (x) x)
                 (list (path/param "foo" '())
                       (path/param "goo" '())))
                "foo/goo")
  (check-equal? (url-path->path
                 (lambda (x) 'fail)
                 (list (path/param "foo" '("zipzap"))
                       (path/param "goo" '())))
                'fail)
  (check-equal? (url-path->path
                 (lambda (x) 'fail)
                 (list (path/param 'up '())
                       (path/param "goo" '())))
                'fail))


(run-tests)

(serve/servlet start
               #:port 8025
               ;; #:listen-ip #f ;; running locally, for now.
               #:launch-browser? #f
               #:servlet-regexp #px""  ;; trivially succeeds
               #:log-file "/tmp/webide-backend-log")