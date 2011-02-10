#lang racket

(require web-server/servlet-env
         web-server/http/request-structs
         (planet dherman/json:3:0)
         "response.rkt")

;; these will probably change, as we refine the protocol:
(define id-name #"id")
(define text-fields-name #"textfields")


(define (start request)
  (printf "received request\n")
  (let* ([b (request-bindings/raw request)]
         [id-binding (bindings-assq id-name b)]
         [text-fields-binding (bindings-assq text-fields-name b)])
    (cond 
      [(not id-binding)
       (internal-error "couldn't find binding for evaluation id")]
      [(not (binding:form? id-binding))
       (internal-error "binding for evaluation id didn't have an associated value")]
      [(not text-fields-binding)
       (internal-error "couldn't find binding for text fields")]
      [(not (binding:form? text-fields-binding))
       (internal-error "binding for text fields didn't have an associated value")]
      [else
       (with-handlers ([exn:fail? 
                        (lambda (exn) 
                          (internal-error "couldn't decode text fields, possibly not in JSON format"))])
         (let* ([evaluation-id (binding:form-value id-binding)]
                [text-fields (json->jsexpr (bytes->string/utf-8 (binding:form-value text-fields-binding)))])
           (printf "id: ~v\ntext fields: ~v\n" evaluation-id text-fields)
           (if (not (list? text-fields))
               (internal-error "text fields entry wasn't the JSON encoding of an array")
               (if (not (andmap hash? text-fields))
                   (internal-error "at least one of the text fields wasn't the JSON encoding of an object")
                   (if (findf (lambda (hash) (equal? (hash-ref hash 'fieldName (lambda () #f)) "secretfail")) (filter text-fields))
                       (failure `(p "You found the secret failure back-door!"))
                       (eval-submission evaluation-id text-fields))))))])))

;; a simple string comparison evaluator:
(define (eval-submission id text-fields)
  ;; always succeed:
  (success "good job!"))


(serve/servlet start
               #:port 8067
               ;; #:listen-ip #f
               #:launch-browser? #f
               #:servlet-path "/eval.rkt"
               #:log-file "/tmp/webide-backend-log")

