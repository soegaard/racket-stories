#lang racket/base
;;;
;;; Send mail using postmark
;;;

(provide send-reset-password-email)

(require postmark     ; from the postmark-client package
         json
         "secret.rkt") ; contains the server api key

(define key (postmark postmark-api-token))

(require json)

(define (send-reset-password-email email name-of-user action-url)
  ; The model values will be injected into the template named "password-reset".
  ; The template can be edited on Postmark's homepage.  
  (define model (hasheq 'name         name-of-user
                        'action_url   action-url))
  
  (postmark-send-email-with-template
   key	 
   #:to             email
   #:from           "passwordreset@racket-stories.com"
   #:template-alias "password-reset"	 
   #:template-model model
   #:tag            "reset-password"))

