#lang racket/base
;;;
;;; Authentication
;;;

(provide derive-key
         verify-password
         random-salt)

; Principles:
;   1. Don't store passwords in plain text
;      (It's a problem if an attacker gets access to the database)
;      Store a key derived from the password.

;   2. Don't store a simple hash of the password.
;      (An attacker might have rainbow tables)
;      Conclusion: Use a salt (random string, non-secret)
;      string together with the passwords to derive a key.

;   3. Use a standard KDF (Key Derivation function)
;      Conclusion: Use the `crypto` package.

;   4. Use the Argon2 algorithm, but fallback to
;      PBKDF2 when not available.

; Note:
;     Salts aren't secret. Their only purpose is to make
;     a rainbow table attack impossible.

; Note:
;     Argon2 is not included in openssl and libargon2
;     is not distributed with Racket - so not everyone
;     has it installed.

; Cloning and compiling the Argon project
;   ( https://github.com/P-H-C/phc-winner-argon2 )
; is simple:

; > git clone https://github.com/P-H-C/phc-winner-argon2.git
; > cd cd phc-winner-argon2/
; > make 
; > sudo make install

; But .. on new macOS the last step requires you to change a line in the Makefile:
;   From: PREFIX ?= /usr
;   To:   PREFIX ?= /usr/local


(require crypto crypto/all crypto/argon2
         "def.rkt")

;;;
;;; KEY DERIVATION
;;;

(define kdf-spec-argon2 'argon2id) 
(define kdf-spec-pbkdf2 (list 'pbkdf2 'hmac 'sha1))

; Get an implementation of a key derivation function from the spec.
(use-all-factories!)
(def kdf-impl (or (get-kdf kdf-spec-argon2)
                  (get-kdf kdf-spec-pbkdf2)))

(unless kdf-impl
  (error 'authentication.rkt
         "could not get an implementation of the key derivation function"))

; To derive a key, use:
;     (kdf k pass salt [params])
; where k is the kdf-imp.

; The parameters can be tweaked - see the docs for crypto.

(define pbkdf2-params (list (list 'iterations 65536) 
                            #;(list 'key-size   64)))

(define argon2id-params  '((p 2)       ; number of cores
                           (t 256)     ; time cost
                           (m 2048)    ; memory cost in kb per core
                           ; (keysize )  ; use automatic
                           ))

(def params (if (get-kdf kdf-spec-argon2)
                argon2id-params
                pbkdf2-params))

;; Okay - now we have a key derivation implemented.
;; Let's derive some keys.
                
(define (derive-simple-key password salt)
  (kdf kdf-impl password salt params))

(define (verify-simple-key password stored-salt stored-key)
  (equal? (derive-simple-key password stored-salt) stored-key))

; (time (derive-simple-key #"foo" #"123"))


; If at some point we need to switch to another key derivation algorithm,
; we will have a situation where some users have keys for old algorithm
; and others for the new one. To keep track of this, it is convenient
; to embed information on the key derivation algorithm into the key
; stored in the database. The `crypto` library provides `pwhash` for
; this purpose.

; An additional benefit of `pwhash` is that it generates a salt
; automatically and embeds it in the key. Therefore we don't need
; an extra field in the database to store the password salt.

(define (derive-key password)
  (pwhash kdf-impl password params))

(define (verify-password password key)
  (pwhash-verify kdf-impl password key))

;;;
;;; SALT
;;;

(define (random-salt [n 16])
  (crypto-random-bytes n))
