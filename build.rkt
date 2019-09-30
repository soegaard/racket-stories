#lang racket
;;;
;;; Build
;;;

; This file creates a new folder "build", then copies all files
; needed to deploy the app on the server, to the folder.
; If the build folder already exists, it is deleted.


(require racket/runtime-path)

(define current-prefix          (make-parameter ""))
(define current-build-directory (make-parameter "build"))

(define-runtime-path files-root "files-root")
(define-runtime-path app-rs     "app-racket-stories")

(define (build app-dir)
  (define base      (current-prefix))
  (define (to-path dir)
    (match base
      ["" (build-path dir)]
      [_  (build-path base dir)]))
  (define build-dir (to-path (current-build-directory)))

  ; DELETE build-dir
  (when (directory-exists? build-dir)
    (delete-directory/files build-dir #:must-exist? #t))

  
  ; make the build directory
  (unless (directory-exists? build-dir)
    (make-directory build-dir))

  ; copy the app(s)
  (copy-directory/files (to-path app-dir)
                        (build-path build-dir app-dir))

  ; copy the static files
  (copy-directory/files files-root
                        (build-path build-dir "files-root"))

  ; database path
  (define dbs-dir (build-path build-dir "dbs"))
  (unless (directory-exists? (to-path dbs-dir))
    (make-directory (to-path dbs-dir)))
  
  (copy-directory/files "server.rkt"
                        (build-path build-dir "server.rkt")))

(build "app-racket-stories")

