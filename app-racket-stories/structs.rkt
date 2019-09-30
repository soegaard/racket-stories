#lang racket/base
;;;
;;; Structs
;;;

(provide (struct-out authentication-error)
         (struct-out validation)
         (struct-out deployment)
         (struct-out development)
         (struct-out testing)
         (struct-out staging)
         (struct-out production))
         
;;;
;;; Form Validation
;;;

(struct authentication-error (message) #:transparent)

(struct validation (value validated? invalid? feedback) #:transparent)


;;;
;;; Deployment
;;;

; Our software development proccess our software consists of multiple stages:
;   - development
;   - testing
;   - deployment

; The development environment is usually a local machine
; and the deployment environment is a web server.

; It's ideal to have similar environments, but at times
; there are some differences. The configuration files
; thus needs to know whether we are in one or the other stage.

(struct deployment ()             #:transparent)
(struct development deployment () #:transparent) ; local developer machine
(struct testing     deployment () #:transparent) ; test env
(struct staging     deployment () #:transparent) ; mirror of production env
(struct production  deployment () #:transparent) ; serves end-users
