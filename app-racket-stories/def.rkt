#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/match)

;;; shorter names used for internal definitions
(define-syntax defv (make-rename-transformer #'define-values))
(define-syntax defm (make-rename-transformer #'match-define))
(define-syntax def  (make-rename-transformer #'define))

(provide defv defm def)


(define-syntax (or/integer stx)
  (syntax-parse stx
    [(_)                    #'#f]
    [(_ e:expr)             (syntax/loc stx (let ([t e]) (and (integer? t) t)))]
    [(_ e0:expr e:expr ...) (syntax/loc stx
                              (let ([t e0])
                                (or (and (integer? t) t)
                                    (or/integer e ...))))]))

(provide or/integer)
