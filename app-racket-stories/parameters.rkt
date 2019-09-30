#lang racket/base
(provide (all-defined-out))
(require "structs.rkt")

;;;
;;; Parameters
;;; 

; The server stores the current request in current-request.
(define current-request (make-parameter #f))


(define current-user        (make-parameter #f))

; The control checks login-status and sets the parameter.
; The view needs this to display the login status.
(define current-login-status (make-parameter #f))


(define current-output-cookies (make-parameter '()))

(define (add-output-cookie c)
  (current-output-cookies (cons c current-output-cookies)))

(define current-banner-message (make-parameter #f))

