#lang racket/base
;;;
;;; DATABASE
;;;

(provide connect-to-database)

; For users who wants to try running racket-stories on their own machines,
; we use a simple sqlite database to avoid any database setup.
; The real racket-stories is deployed using a postgresql database.

; See "config.rkt" for configuration options.

(require racket/match racket/os db 
         "config.rkt" "deployment.rkt"
         "parameters.rkt" "structs.rkt")

(unless (directory-exists? sqlite-db-dir)
  (make-directory sqlite-db-dir))


(define (connect-to-database)
  (match (or the-deployment (development))
    [(or (development) (testing)) (if (member (gethostname) '() #;'("mbp"))
                                      (connect-to-postgresql)
                                      (connect-to-sqlite))]
    [(or (staging) (production))  (connect-to-postgresql)]))


; We keep a pool of connections to the database. Reusing connections
; to database is faster than setting up a new connection each time
; (and a single connection is fragile).

(define pool #f)

(define (connect-to-sqlite)
  (set! pool (or pool
                 (connection-pool
                  (λ () (sqlite3-connect    #:database sqlite-db
                                            #:mode     'create)))))
  (connection-pool-lease pool))


(define (connect-to-postgresql)
  (set! pool (or pool                 
                 (connection-pool
                  (λ () (postgresql-connect #:database (database-name)
                                            #:password (database-password)
                                            #:user     (database-user)
                                            #:server   (database-server)
                                            #:port     (database-port)
                                            #:ssl      'yes)))))
  (connection-pool-lease pool))
