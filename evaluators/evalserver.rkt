#lang racket

(require web-server/servlet-env
         web-server/http/request-structs
         xml
         (planet dherman/json:3:0)
         racket/dict
         "java-header-example.rkt"
         rackunit)

;; these will probably change, as we refine the protocol:
(define id-name 'id)
(define text-fields-name 'textfields)

;; create a string indicating failure:
(define (failure xexpr) (result "failure" (xexpr->string xexpr)))

;; create a string indicating success:
(define (success xexpr) (result "success" (xexpr->string xexpr)))

;; create a string indicating an internal error:
;;(define (internal-error xexpr) (result "internal-error" (xexpr->string xexpr)))
;; internal-error not yet supported, falling back to failure:
(define internal-error failure)

(define (result tag text)
  (jsexpr->json (make-immutable-hasheq `((status . ,tag) (message . ,text)))))

(define (start request)
  (printf "received request\n")
  (let* ([b (request-bindings/raw request)]
         [_ (printf "raw bindings: ~a\n" b)]
         [request-binding (bindings-assq #"request" b)]
         [_ (printf "the request: ~a\n" request-binding)])
    (cond [(not request-binding)                 (internal-error "couldn't find expected \"request\" binding")]
          [(not (binding:form? request-binding)) (internal-error "binding for request didn't have an associated value")]
          [else
           (with-handlers (#;[exn:fail? (lambda (exn) (internal-error "couldn't decode request fields, possibly not in JSON format"))])
             (let* ([request-json (bytes->string/utf-8
                                   (binding:form-value request-binding))]
                    [request-hash (json->jsexpr request-json)])
               (printf "raw json-encoded request: ~v\n" request-json)
               (process-request-hash request-hash)))])))

(define (process-request-hash request-hash)
  (cond 
    [(and (dict-has-key? request-hash id-name) 
          (dict-has-key? request-hash text-fields-name))
     (let* ([id-name (hash-ref request-hash id-name)]
            [text-fields (hash-ref request-hash text-fields-name)])
       (cond [(not (hash? text-fields))
              (internal-error "text fields entry wasn't the JSON encoding of an object")]
             [(not (dict-has-key? evaluator-table id-name))
              (internal-error (format "unknown step id: ~s" id-name))]
             [else 
              (match-let ([(list (list required-fields evaluator)) (dict-ref evaluator-table id-name)])
                (cond 
                  [(not (andmap (lambda (field) (dict-has-key? text-fields field)) required-fields))
                   (internal-error (format
                                    "request for id ~s is missing some of the required fields ~s"
                                    required-fields))]
                  [else
                   (with-handlers ([exn:fail? (lambda (exn) (internal-error (exn-message exn)))])
                     (apply evaluator (map (lambda (field) (dict-ref text-fields field)) required-fields)))]))]))]
    [else
     (internal-error (format "evaluation request must have ~s and ~s fields, got: ~s" 
                             id-name text-fields-name
                             request-hash))]))

(define (approx-age-header-checker firstline)
  (match (first-line-checker firstline)
    ['success (success "Good Job!")]
    [(? string? msg) (failure msg)]))


(define evaluator-table
  ;; each entry requires a name and a list containing the list of text fields and the function
  ;; that handles the evaluation.
  `(("getApproxAgeHeader" ((firstLine) ,approx-age-header-checker))))


(serve/servlet start
               #:port 8067
               #:listen-ip #f
               #:launch-browser? #f
               #:servlet-path "/eval.rkt"
               #:log-file "/tmp/webide-backend-log")



#;(check-equal? (approx-age-header-checker "public lapdog")
              (failure "After the word public, you need a type name."))