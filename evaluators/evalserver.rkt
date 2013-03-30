#lang racket/base


(require (for-syntax racket/base)
         racket/match
         racket/list
         racket/dict
         web-server/servlet-env
         web-server/http/request-structs
         web-server/http/response-structs
         net/url
         "java-header-example.rkt"
         "c-evaluators.rkt"
         rackunit
         "shared.rkt"
         "transport.rkt"
         "mongodb-logger.rkt")


(define (start request)
  (let/ec abort
    ;; no exceptions allowed to pass:
    (with-handlers ([(lambda (x) #t)
                     (lambda (exn)
                       ((log-and-return
                         abort
                         #f)
                        "serverfail"
                        (format "internal error: ~s" 
                                (exn-message exn))))])
      (log-debug "received request")
      (define logged-request-tag (log-incoming-eval-request request))
      (define aborter (log-and-return abort logged-request-tag))
      
      (define uri (request-uri request))
      (define post-data (request-post-data/raw request))
      (define path-string (url-path->path aborter (url-path uri)))
      (match-let* ([(list args-assoc textfields-assoc) 
                    (with-handlers 
                        ([exn:fail? 
                          (lambda (exn)
                            (aborter
                             "callerfail"
                             (format
                              "exception during decoding input data with message: ~a"
                              (exn-message exn))))])
                      (post-bytes->args-n-fields post-data))])
        (define evaluator 
          (first
           (dict-ref evaluator-table path-string
                     (lambda ()
                       (aborter
                        "callerfail"
                        (format 
                         "unknown evaluator path: ~a"
                         path-string))))))
        (log-debug "calling evaluator")
        (define result-dict (result->dict (evaluator path-string args-assoc textfields-assoc)))
        (log-outgoing-eval-result logged-request-tag result-dict)
        (make-webide-response result-dict)))))

;; url-path->path : (listof path/param) -> string
(define (url-path->path aborter url-path)
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (aborter
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
    (aborter 
     "callerfail"
     (format "request for evaluator ~s is missing these required ~s: ~s"
             (url->string uri)
             kind-str
             missing))))


;; log an error, return it to the user
(define ((log-and-return escaper logged-request-id) status message)
  (log-error message)
  (define result-dict `((status . ,status) (message . ,message)))
  (log-outgoing-eval-result logged-request-id result-dict)
  (escaper (make-webide-response result-dict)))

(define (approx-age-header-checker firstline)
  (match (birth-year-example firstline)
    ['success (success)]
    [(? string? msg) (failure msg)]))


#;(define evaluator-table
  ;; each entry requires a path-string and a list containing the list of required arguments,
  ;; the list of required text fields, and the function
  ;; that handles the evaluation.
  `(("getApproxAgeHeader" (() () ,approx-age-header-checker))
    ("alwaysSucceed" (() () ,(lambda (dc1 dc2) (success))))
    ("any-c-int" (() () ,any-c-int))
    ("any-c-addition" (() () ,any-c-addition))
    ("c-parser-match" ((pattern) () ,c-parser-match))))

(define evaluator-table
  `(("getApproxAgeHeader"
     ,(evalpat `(() ((,dc . ,usertext))) (approx-age-header-checker usertext)))
    ("alwaysSucceed"
     ,(lambda (path args texts) (success)))
    ("any-c-int" 
     ,(evalpat `(() ((,dc . ,usertext))) (any-c-int usertext)))
    ("any-c-addition"
     ,(evalpat `(() ((,dc . ,usertext))) (any-c-addition usertext)))
    ("c-parser-match"
     ,(evalpat `(((pattern . ,pattern)) ((,dc . ,usertext))) (c-parser-match pattern usertext)))
    ("c-stmt-parser-match"
     ,(evalpat `(((pattern . ,pattern)) ((,dc . ,usertext))) (c-stmt-parser-match pattern usertext)))))

;; this pattern abstracts over the common pattern,
;; allowing you to specify just the desired pattern and the action.
(define-syntax (evalpat stx)
  (syntax-case stx ()
    [(_ pat call)
     #`(lambda (path args texts)
         (match (list args texts)
           [pat call]
           [other (callerfail (format "bad arguments or user texts for path ~s in: ~s and ~s" 
                                      path args texts))]))]))

(define-syntax (evalpat/1 stx)
  (syntax-case stx ()
    [(_ fun)
     #`(evalpat `(() (,dontcare . ,usertext)) (fun usertext))]))



;; turn an evaluator response into a webide response
(define (result->dict result)
  (match result
    ;; adding success error message again, to make WebIDE happy:
    [(success)        `((status . "success") (message . "Good Job!"))]
    [(failure msg)    `((status . "failure") (message . ,(encode-html-for-transport msg)))]
    [(serverfail msg) `((status . "serverfail") (message . ,msg))]
    [(callerfail msg) `((status . "callerfail") (message . ,msg))]))


;; format a response
(define (make-webide-response assoc)
  (response/full
   200
   #"Okay"
   (current-seconds)
   TEXT/HTML-MIME-TYPE ;; should change? maybe not.
   empty
   (list
    (jsexpr->response-bytes 
     (make-immutable-hasheq assoc)))))



(check-equal? (url-path->path
               (lambda args 5)
               (list (path/param "foo" '())
                     (path/param "goo" '())))
              "foo/goo")
(check-equal? (url-path->path
               (lambda args 'fail)
               (list (path/param "foo" '("zipzap"))
                     (path/param "goo" '())))
              'fail)
(check-equal? (url-path->path
               (lambda args 'fail)
               (list (path/param 'up '())
                     (path/param "goo" '())))
              'fail)


(serve/servlet start
               #:port 8025
               #:listen-ip #f
               #:launch-browser? #f
               #:servlet-regexp #px""  ;; trivially succeeds
               #:log-file "webide-backend-webserver-log")
