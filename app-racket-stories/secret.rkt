#lang racket/base
;;;
;;; SECRET
;;;

; This file contains secrets.

; Since the source is publicly available the secrets need to be
; encrypted in the source code. At runtime the keys are decrypted
; using a key stored in the environment.

; Alternatives:
;   Store the keys in the environment (environment variables or files).
;   The "envy" package can be used to handle environment variables:
;      https://lexi-lambda.github.io/envy/envy.html

(provide github-client-id     ; see prefs at github.com
         github-client-secret 
         (rename-out [aes-decrypt decrypt])
         (rename-out [aes-encrypt encrypt])) 

;;;
;;; Encryption and decryption of secrets
;;;

(require crypto crypto/libcrypto (only-in file/sha1 hex-string->bytes))
(crypto-factories (list libcrypto-factory))


; We will use AES and we need a random iv (initialization vector).
(define aes-cipher-specifier '(aes ctr))
(define iv #"\316\320\344\354\3p\260\20\353\5N<\347q\331\371")
; Use (generate-cipher-iv '(aes ctr)) to generate another.

(define (new-iv)  (generate-cipher-iv  aes-cipher-specifier))
(define (new-key)
  (bytes->hex-string
   (generate-cipher-key aes-cipher-specifier #:size 32)))


; aes-encrypt : string -> string
(define (aes-encrypt plain-text) 
  (bytes->hex-string
   (encrypt aes-cipher-specifier key iv plain-text)))

; aes-decrypt : string -> string
(define (aes-decrypt crypto-text)
  (decrypt aes-cipher-specifier key iv
           (hex-string->bytes crypto-text)))


;;;
;;; The key
;;;

; Use (new-key) to generate a new key.

; The key used to decrypt the secrets in this file can be stored
; either in the environment variable "rskey" or in a file "rskey"
; in the user home (i.e. outside this repo).

(define key ; 16, 24 or 32 bytes
  (cond [(getenv "RSKEY") => hex-string->bytes]
        [(getenv "HOME")
         => (λ (home)
              (define p (build-path home ".racket-stories/rskey"))
              (cond
                [(file-exists? p) (with-input-from-file p
                                     (λ () (hex-string->bytes (read-line))))]
                [else
                 (displayln "No key in either environment or home. Using default."
                            (current-error-port))]))]
        [else
         (displayln "No key in either environment or home. Using default."
                    (current-error-port))
         #"A secret key!!!!"])) ; this one is 16 bytes

;;;
;;; Github
;;;

; To authenticate users with Github our app has been given
; a client-id and client-secret, so we can identify ourselves
; to Github. (In this scenario our app is the client).
; In other words this secret is shared between us and Github.

; See 

(define github-client-id
  (aes-decrypt "2b158374fc518e1d41e40cca7ba5de867589c7d5"))

(define github-client-secret
  (aes-decrypt
   "7e44d624f9048f4812b254c82bf3818075dc95d568c15cd32e38769426914e82b58d49d02db3cf21"))



