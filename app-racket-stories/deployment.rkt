#lang racket/base
;;;
;;; DEPLOYMENT
;;;

; Determine whether the deployment is:
;  development, testing, staging or production

(provide the-deployment)

(require racket/match racket/os
         "structs.rkt")

(define the-deployment
  (case (system-type 'os)
    [(macosx windows) (development)]
    [(unix)           (cond
                        [(regexp-match #rx"web-rs" (gethostname))
                         (if (file-exists? "PRODUCTION")
                             (production)
                             (staging))]
                        [else
                         (development)])]
    [else             (development)]))
