#lang racket/base
;;;
;;; CONFIGURATION
;;;

; This file contains settings that may differ between development, testing and
; actual deployment.


;;; DATABASES

; Our code can run on two different databases: sqlite and postgreql.

; The advantage of sqlite is that it requires no setup; it simply
; stores everything in a local file. The disadvantage is that
; it has a very small set of features. For someone who just wants to
; try the racket-stories app on their own computer, sqlite
; will work out of the box.

; The real racket-stories.com uses a postgresql database.

; Postgreql on the other hand needs a bit of setup:
; a server process (that handles multiple databases, users,
; authentication etc) must be started.

; During development a local postgresql server will be practical
; to use.

(provide (all-defined-out))

;; Requirements

(require racket/format racket/match racket/runtime-path
         "deployment.rkt" "parameters.rkt" "secret.rkt" "structs.rkt")


;;; Sqlite

(define-runtime-path sqlite-db "../dbs/racket-stories-sqlite.db")
; no user / password


;;; Postgresql

(define database-name
  (match the-deployment
    [(development) "racket-stories-development"]
    [(testing)     "racket-stories-testing"]
    [(staging)     "racket-stories-staging"]
    [(production)  "racket-stories"]
    [else "racket-stories-development"]))

(define (database-user)
  (match the-deployment
    [(or (staging) (production))  (decrypt "3c05c524ae")]
    [(or (development) (testing)) "rs"]))

(define (database-password)
  (match the-deployment
    [(or (staging) (production))  (decrypt "3b1cc134b544d84e19f85b9975f9cf87")]
    [(or (development) (testing)) "rs"]))

(define (database-server)
  (match the-deployment
    [(or (staging) (production))
     (decrypt (~a "2a149f32b84c874b02ad5cd07cfa95c43fdad09b67c55bd07e"
                  "6a3a9c6b971895ecda4cdc7def8871ee78ef46686170ee949d"))]
    [(or (development) (testing)) "localhost"]))


(define (database-port)
  (match the-deployment
    [(or (staging) (production))  (string->number (bytes->string/utf-8 (decrypt "7c438277fc")))]
    [(or (development) (testing)) 5432]))
