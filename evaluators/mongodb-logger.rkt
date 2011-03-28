#lang racket

(require (planet jaymccarthy/mongodb)
         web-server/http/request-structs
         net/url)

(provide log-incoming-eval-request
         #;log-outgoing-eval-result)

(define m (create-mongo))
(define d (mongo-db m "webideEvalLog"))
(define requests (mongo-collection d "requests"))
(define results (mongo-collection d "results"))

;; log-incoming-eval-request : request? -> bson-objectid?
(define (log-incoming-eval-request request)
  (define oid (new-bson-objectid))
  (define timestamp (current-seconds))
  (define request-bson
    `((method . ,(request-method request))
      (uri . ,(url->string (request-uri request)))
      (headers . ,(list->vector (map header->bson (request-headers/raw request))))
      (bindings . ,(list->vector (map binding->bson (request-bindings/raw request))))
      (post-data . ,(request-post-data/raw request))
      (host-ip . ,(request-host-ip request))
      (host-port . ,(request-host-port request))
      (client-ip . ,(request-client-ip request))))
  (define log-doc
    `((_id . ,oid)
      (timestamp . ,timestamp)
      (request . ,request-bson)))
  (mongo-collection-insert-one! requests log-doc)
  oid)


(define (header->bson header)
  ;; field names are required to be case-insensitive readable ASCII values, per RFC 822
  `((field . ,(bytes->string/utf-8 (header-field header)))
    (value . ,(header-value header))))

(define (binding->bson binding)
  (match binding
    [(binding:form id value)
     `((id . ,id)
       (value . ,value))]
    [other "unknown binding..."]))