#lang racket/base
(require racket/list
         web-server/http/cookie
         web-server/http/response-structs
         (planet clements/sxml2))

(define (response/sxml
         sxml
         #:code [code 200] 
         #:message [message #"Okay"]
         #:seconds [seconds (current-seconds)]
         #:mime-type [mime-type TEXT/HTML-MIME-TYPE]
         #:cookies [cooks empty]
         #:headers [hdrs empty]
         #:preamble [preamble #""])
  (response
   code message seconds mime-type 
   ; rfc2109 also recommends some cache-control stuff here for cookies
   (append hdrs (map cookie->header cooks))
   (λ (out)
     (write-bytes preamble out)
     ;; let's just see how this looks
     (srl:sxml->html sxml out))))

(provide response/sxml)
