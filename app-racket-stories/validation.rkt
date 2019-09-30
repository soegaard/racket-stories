#lang racket/base
(provide (all-defined-out))

;;;
;;; Validation
;;;

; This file contains functions that can be used to validate data submitted
; via forms, as well as functions that use the validation results.

; The validators all return `validation` structs:

;   (struct validation (value validated? invalid? feedback) #:transparent)

; Here  validated? is true, if the struct is the result of a validator,
;       invalid?   is true, if value wasn't valid,
;       feedback   is the text shown to the user for an invalid value.


;;; Imports

(require racket/match
         "def.rkt" "structs.rkt")

;;; Validators for the input fields in the "Submit new entry page":

(define (validate-url url)
  (define feedback "Enter an url that starts with http:// or https://")
  (match (regexp-match #rx"http[s]?://.+" url) ; too permissive?
    [#f (validation url #t #t feedback)]
    [_  (validation url #t #f "")]))

(define (validate-title title)
  (cond
    [(< (string-length title) 4) (validation title #t #t "Enter a longer title")]
    [else                        (validation title #t #f "")]))

;;; Combinator

; The form data as a whole is only valid of all field were valid.

(define (all-valid? . validations)
  (for/and ([v validations])
    (not (validation-invalid? v))))

;;; Styling

; Bootstrap needs the `input` form to have a class indication
; whether it has been validated and if it has, whether it was valid or invalid.

(define (input-class-validity v) ; validation -> css-class (a string)
  (and v (let ()
           (defm (validation _  validated? invalid? _) v)
           (if validated? (if invalid? "is-invalid" "is-valid") ""))))
