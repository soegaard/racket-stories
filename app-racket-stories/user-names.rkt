#lang racket/base
;;;
;;; User names
;;;

; 1. Check maximum length
; 2. Accepted characters
; 3. Trim all whitespace
; 4. Check whether username is in black list.

; The blacklist contains words like
;     admin, null, root, etc.

; Note:
;  resources/username-blacklist.txt
;  is from
;    https://github.com/marteinn/The-Big-Username-Blacklist
;  (sep 14, 2019)


(provide normalize-username
         good-username?
         bad-username-reason)


(require racket/contract racket/string racket/runtime-path
         "def.rkt")

(module+ test (require rackunit))


;; Accepted characters and trimming

; A bit restrictive, but easier to relax rules later than tighten them.
(def forbidden-chars #px"[^a-zA-Z0-9_-]")

(define/contract (trim-username username) (-> string? string?)
  (string-trim username))

(module+ test
  (check-equal? (trim-username "  foo \t\n ") "foo"))


;; Normalization

; Only the normalized version of a username is stored in the database.
; Normalization avoids the situation where Foo and foo are different users.

(define (normalize-username username)
  (string-downcase (trim-username username)))


;; Blacklisting

(define-runtime-path username-blacklist.txt "resources/username-blacklist.txt")

(def blacklisted-usernames
  (with-input-from-file username-blacklist.txt
    (λ () (for/hash ([line (in-lines)])
            (values (normalize-username line) #t)))))

(define (blacklisted-username? username)
  (def u (normalize-username username))
  (hash-ref blacklisted-usernames u #f))

(module+ test
  (check-equal? (blacklisted-username? "admin") #t "admin blacklisted")
  (check-equal? (blacklisted-username? "foo")   #f "foo is ok"))

;; Good usernames 

; find reason a username is bad, returns #f if the username is good
(define/contract (bad-username-reason username) (-> string? (or/c string? boolean?))
  (def u (string-trim username))
  (cond
    [(> (string-length u) 64)         "username too long"]
    [(< (string-length u)  3)         "username too short"]
    [(regexp-match forbidden-chars u) "username contains forbidden character (Only letters, digits, dash and underscore are allowed)"]
    [(blacklisted-username? u)        "username is in use"] ; a white lie
    [else                             #f]))

(define/contract (good-username? username) (-> string? boolean?)
  (not (bad-username-reason username)))

(module+ test
 (check-equal? (good-username? "foo")     #t)
 (check-equal? (good-username? "foo/")    #f "no slash")
 (check-equal? (good-username? "foo bar") #f "no whitespace")
 (check-equal? (good-username? "foo-")    #t "dash allowed")
 (check-equal? (good-username? "foo-")    #t "underscore allowed")
 (check-equal? (good-username? "foo2")    #t "digits allowed")
 (check-equal? (good-username? "admin")   #f "admin blacklisted"))


