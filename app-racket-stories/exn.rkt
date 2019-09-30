#lang racket/base
;;;
;;; Exceptions
;;;

; The exceptions uses to communicate errors.

(provide (all-defined-out))

(struct exn:fail:user:bad exn:fail ()) ; user creation failed due to bad username
