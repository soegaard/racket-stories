#lang racket/base

;;;
;;; CONTROL
;;;

; The interface between the web-server in "server.rkt" and the control
; is the function `dispatch`. Each time the web-server receives an request
; it is passes on to `dispatch`.

(provide dispatch)

;; Imports

(require (for-syntax racket/base)         
         racket/format
         racket/match
         racket/port
         racket/promise
         racket/runtime-path
         web-server/dispatch/extend
         web-server/servlet-env
         web-server/servlet
         web-server/http/redirect
         web-server/http/cookie
         web-server/http/id-cookie ; authenticated cookies
         web-server/dispatchers/dispatch-files
         web-server/dispatchers/dispatch
         web-server/http/request-structs
         net/http-client
         net/uri-codec
         json
         "def.rkt" "exn.rkt" "parameters.rkt" "structs.rkt"
         "validation.rkt" 
         "model.rkt" "view.rkt" "secret.rkt")

;;;
;;; Utils
;;;

(define (bytes->number b)
  (string->number (bytes->string/utf-8 b)))

;;;
;;; Bindings
;;;

; get-binding
;   Extract a binding from the request if present and then
;   apply convert to the extracted value.
;   Typical use:  (get-binding #"username" bytes->string-utf/8)
(define (get-binding binding-name [convert values] #:request [req (current-request)])
  (match (bindings-assq binding-name (request-bindings/raw req))
    [(? binding:form? b) (convert (binding:form-value b))]
    [_ #f]))

;;;
;;; Cookies
;;;

; We are going to store session information (the session token)
; on the client in cookies. To make these tamper proof, we need a way to verify,
; that they haven't been changed by an attacker (possibly the user).

; In order to store `authored-seconds&data` we send
; `digest&authored-seconds&data` where digest is a
; cryptographics hash of authored-seconds&data and, very important,
; a secret salt only known to the server(s).
; If using multiple servers, they need to share the secret salt.

; In short: we are not encrypting the data, we are merely
; associating it with a digest, so it can't be altered.

(define-runtime-path cookie-salt.bin "cookie-salt.bin")
(def cookie-salt (make-secret-salt/file cookie-salt.bin))

(define (ensure-bytes v)
  (cond
    [(bytes? v)  v]
    [(string? v) (string->bytes/utf-8 v)]
    [else        (error 'ensure-bytes (~a "got: " v))]))




(define create-id-cookie
  (case (system-type)
    ; Assume that we are developing/testing when the
    ; system type is mac or windows - in which case
    ; we are not using ssl.    
    [(macosx windows)
     ; otherwise we are on the server were ssl is running
     (λ (name value)
       (make-id-cookie name value
                       #:key cookie-salt
                       #:http-only? #t
                       #:extension "SameSite=Strict"))]
    [else
     (λ (name value)
       (make-id-cookie name value
                       #:key cookie-salt
                       #:domain "racket-stories.com"
                       ; only for http/https (not client side javascript)
                       #:http-only? #t
                       ; secure? means the client only sends cookie via https
                       #:secure? #t
                       #:extension "SameSite=Strict"
                       ; #:expires ...
                       ; #:max-age ...                            
                       ))]))

(define (make-session-cookie token)
  (create-id-cookie "session-token" token))

(define (make-logged-out-cookie)
  (create-id-cookie "session-token" ""))


(define (make-username-cookie username)
  (create-id-cookie "username" username))

(define (get-id-cookie-value req name)
  (request-id-cookie req #:name name #:key cookie-salt
                     ; #:timeout ...
                     ; #:shelf-life ...
                     ))

;;;
;;; DISPATCH
;;;

; The main entry point to the control is `dispatch`.
; Dispatch is called by the web-server and receives the current request.

; Before anything else, we check whether the request came from
; a user that has an session cookie with a session token.
; If so, we fetch the session from the db (the model checks that
; the sessions hasn't expired). Such a session implies that the
; user is logged-in, so we can set the current-user.

; After setting current-login-status and current-user, we
; continue to `dispatch-on-url` which based on the url
; decides what happens next.

(define (dispatch req)
  (current-request req)
  (def token    (get-id-cookie-value req "session-token"))
  (def session  (get-session token))
  (def user     (and session (get-user (session-user-id session))))
  
  (parameterize ([current-login-status   (and session user #t)] ; todo remove
                 [current-user           (and session user)])
    (dispatch-on-url req)))

;;; URL Dispatching

; In order to make some nice routing rules, we need a few utilities
; in order to define route arguments.

; from web-server/dispatch/url-patterns
(define-syntax define-bidi-match-expander/coercions
  (syntax-rules ()
    [(_ id in-test? in out-test? out)
     (begin (define-coercion-match-expander in/m in-test? in)
            (define-coercion-match-expander out/m out-test? out)
            (define-bidi-match-expander id in/m out/m))]))

;; (define string->integer? (make-coerce-safe? string->integer))
;; (define-bidi-match-expander/coercions integer-arg
;;   string->integer? string->integer
;;   integer? number->string)

(define (vote-direction? x) (or (equal? x "up") (equal? x "down")))
(define-bidi-match-expander/coercions vote-direction-arg
  vote-direction? values vote-direction?  values)

(define (popular-period? x) (member x (list "day" "week" "month" "year" "all")))
(define-bidi-match-expander/coercions popular-period-arg
  popular-period? values popular-period?  values)


; (def entry-id-arg    integer-arg)
; (def page-number-arg integer-arg)

(defv (dispatch-on-url generate-url)
  ; pages:   show a given html page (generated by the view)
  ; actions: performs action, then redirects to a page
  (dispatch-rules
   ; pages
   [("")                                          (λ (req) (do-home req 0))]                 
   [("home")                                      (λ (req) (do-home req 0))]
   [("home" "page" (integer-arg))                 do-home]
   [("new")                                       (λ (req) (do-new req 0 ))]
   [("new" "page" (integer-arg))                  do-new]
   [("popular")                                   (λ (req) (do-popular req "week" 0))]
   [("popular" (popular-period-arg)
               "page" (integer-arg))              do-popular]
   [("user"    (string-arg))                      do-user]    ; show other user
   [("profile")                                   do-profile] ; show own user profile
   [("from" (integer-arg))                        do-from]  ; entries from same site as entry-id
   [("about")                                     do-about]                
   [("login")                                     do-login/create-account] 
   [("submit")                                    do-submit]               ; new entry page

   ; actions
   ;   only recognize up-votes - use the next line if you need both
   [("vote" "up" (integer-arg) (integer-arg)) #:method "post"  (λ (req e p) (do-vote req "up" e p))]
   ; [("vote" "up" (entry-id-arg) (page-number-arg)) #:method "post"  (λ (reg e p) (do-vote "up" e p))]
   ; [("vote" (vote-direction-arg) (integer-arg) (integer-arg)) #:method "post"  do-vote]

   ; these redirect to other pages (and show a banner at the top)
   [("login-to-vote")                             do-login-to-vote]
   [("login-to-submit")                           do-login-to-submit]
   [("login-to-profile")                          do-login-to-profile]
   [("resubmission")                              do-resubmission]
   [("associate-github")                          do-associate-github]

   ; form submissions
   [("logout-submitted")         #:method "post"  do-logout-submitted] ; logout, then show front page
   [("entry-submitted")          #:method "post"  do-entry-submitted]
   [("login-submitted")          #:method "post"  do-login-submitted]
   [("create-account-submitted") #:method "post"  do-create-account-submitted]
   [("profile-submitted")        #:method "post"  do-profile-submitted]

   [("github-login")                              do-github-login]    ; initiate login (by user)
   [("github-callback")          #:method "post"  do-github-callback] ; callback       (by github)
   [("github-callback")                           do-github-callback]))

   
   ; No else clause means the next dispatch ought to serve other files
   ; -- that is the web-server will then serve files from extra-files-root.
   ; This means we can't add a catch-all here to see which routes fell through...

;;;
;;; PAGES
;;;


(define (do-about req)
  (def result (html-about-page))
  (response/output (λ (out) (display result out))))


; The home, new and popular pages are very similar.
; They display a list of entries in some order.
; We have one function in the view  html-list-page,
; that handle all three cases.

(define (do-home req page-number)
  (def first-rank  (+ 1 (* page-number (PAGE-LIMIT))))
  (def entries     (newest page-number)) ; this will like change at some point
  (def votes      (user-votes-on-newest (current-user) page-number))
  (def result      (html-list-page "home" page-number first-rank entries #:votes votes))
  (response/output (λ (out) (display result out))))

(define (do-new req page-number)
  (def first-rank (+ 1 (* page-number (PAGE-LIMIT))))
  (def entries    (newest page-number))
  (def votes      (user-votes-on-newest (current-user) page-number))
  (def result     (html-list-page "new" page-number first-rank entries #:votes votes))
  (response/output (λ (out) (display result out))))

(define (do-popular req period page-number)
  (def first-rank  (+ 1 (* page-number (PAGE-LIMIT))))
  (def entries     (popular (string->symbol period) page-number))
  (def votes       (user-votes-on-popular (current-user) period page-number))
  (def result      (html-popular-page page-number first-rank entries period votes))
  (response/output (λ (out) (display result out))))



(define (do-user req username)
  (def u (get-user username))
  (def result (html-user-page u #:github-user (and u (get-github-user/user u))))
  (response/output (λ (out) (display result out))))

(define (do-profile req)
  (match (current-user)
    [u (def gu (get-github-user/user u))
       (def result (html-profile-page u #:github-user gu))
       (response/output (λ (out) (display result out)))]
    [_
     (redirect-to "/login-to-profile" temporarily)]))


(define (do-from req entry-id)
  (def e (get-entry entry-id)) ; #f if not found
  (def s (and e (entry-site e)))
  (def entries (or (and e (from-site s)) '()))
  (def result (html-from-page entries))
  (response/output (λ (out) (display result out))))


(define (do-login-to-vote req)
  (parameterize ([current-banner-message "Login to vote."])
    (do-login/create-account req)))

(define (do-login-to-submit req)
  (parameterize ([current-banner-message "Login to submit."])
    (do-login/create-account req)))

(define (do-login-to-profile req)
  (parameterize ([current-banner-message "Login to change your profile."])
    (do-login/create-account req)))

(define (do-resubmission req)
  (parameterize ([current-banner-message "Your submission was submitted recently by another user."])
    (do-home req 0)))

(define (do-associate-github req)
  (parameterize ([current-banner-message "To login with Github, you need to link your accounts."])
    (do-login/create-account req)))


(define (do-login/create-account req)
  (def result (html-login-page))
  (response/output (λ (out) (display result out))))

(define (do-logout-submitted req)
  (def un (get-binding #"username" bytes->string/utf-8))
  
  (redirect-to "/" temporarily
               #:headers (map cookie->header
                              (list (make-logged-out-cookie)))))

(define (do-login-submitted req)
  (def un (get-binding #"username" bytes->string/utf-8))
  (def p  (get-binding #"password"))
  (cond
    [(and un p)
     (match (authenticate-user un p) ; returns user on success
       ; On a successful login we generate a new session token
       ; and store it in a cookie.
       ; The redirection prevents the form data being submitted
       ; twice due to reloads in the browser.

       [(? user? u)
        (def s (register-session u)) ; stores session i db
        (redirect-to
         "/" temporarily
         #:headers (map cookie->header
                        (list (make-session-cookie (session-token s)))))]
       ; If the login failed, the user must try again.
       [(authentication-error msg)                          
        (redirect-to "/login" temporarily)])]
    [else
     (redirect-to "/login" temporarily)]))

; 0. User goes to:
;      https://racket-stories/gtihub-login
; 1. Which redirects to:
;      https://github.com/login/oauth/authorize?login=soegaard&client_id=ec150ed77da7c0f796ec
;    where the client_id indentifies racket-stories.com for github.
; 2. After login, the user is redirected by Github to:
;       https://racket-stories.com/github-callback?code=xxxxxxxxx
; 3. We post the code to Github, and gets an access token.
;    Use the access token to get user information.
;    And then we answer the request from 2.

; References:
;   https://developer.github.com/apps/building-oauth-apps/authorizing-oauth-apps/
;   Nice diagram:
;   https://www.vertabelo.com/blog/how-to-store-authentication-data-in-a-database-part-4-implementing-the-login-with-facebook-button-in-python/

(define (do-github-login req)  
  (def our-state (get-new-github-state))
  (def github-action-url
    (~a "https://github.com/login/oauth/authorize?client_id=ec150ed77da7c0f796ec"
        "&state=" our-state))
  (redirect-to github-action-url))


(define (do-github-callback req)
  ; We get here after a succesful login on github.
  ; We get a `code`, which we will use to get an access token.
  
  (def client-id      github-client-id)
  (def client-secret  github-client-secret)
  (def code           (get-binding #"code"))                        ; used to get access token
  (def redirect-uri   "https://racket-stories.com/github-callback") ; needs to be the registered callback
  (def state          (get-binding #"state"))                       ; needs to be the same we sent out
  
  (def (str x) (bytes->string/utf-8 x))
  (def out-headers  (list "Content-Type: application/x-www-form-urlencoded"))
  
  ; we need to send POST the following information to github
  ; in order to get an access token back
  (def data
    (alist->form-urlencoded
     (list (cons 'client_id     (str client-id))
           (cons 'client_secret (str github-client-secret))
           (cons 'code          (str code))
           (cons 'redirect_uri  redirect-uri)
           (cons 'state         (str state)))))

  (cond
    [code (defv (status headers in)
            (http-sendrecv "github.com"
                           "/login/oauth/access_token"
                           #:ssl? #t
                           ; #:port port	 
                           #:version "1.1"
                           #:method "POST" 
                           #:headers out-headers	 
                           #:data data	 
                           #:content-decode '()))
          (define result (port->string in))
          
          (def state (get-binding #"state"))
          (cond
            ; check that we get our state back again
            [(valid-github-state? (str state))
             ; we expect the result to have the form:
             ;   "access_token=xxxx&scope=&token_type=bearer"
             (match (regexp-match #rx"access_token=(.*)&scope=(.*)&token_type=(.*)" result)
               [(list all token scope type)
                ; Now we have the token, we can get the identity (and more)
                (defv (status headers in)
                  (http-sendrecv "api.github.com" "/user"
                                 #:ssl? #t
                                 #:version "1.1"
                                 #:method "GET" 
                                 #:headers (list (~a "Authorization: token " token))
                                 ; #:data data	 
                                 #:content-decode '()))
                (define result (port->string in))
                (def ht (string->jsexpr result))
                (def u (current-user)) ; might be #f
                (def s (login-github-user u ht))
                (cond
                  ; login succeeded
                  [s     (redirect-to
                          "/" temporarily
                          #:headers (map cookie->header
                                         (list (make-session-cookie (session-token s)))))]
                  [else (redirect-to "/associate-github")])])]
            [else
             ; the state and our-state were different (man in the middle attack?)
             (redirect-to "https://racket-stories.com/" temporarily)])]
    [else
     (redirect-to "https://racket-stories.com/" temporarily)]))



(define (do-profile-submitted req)
  (def a  (get-binding #"about" bytes->string/utf-8))
  (def u  (current-user))
  (cond
    [(and u a) (change-user-about u a)
               (redirect-to "/profile" temporarily)]
    [u         (redirect-to "/profile" temporarily)] ; show error?
    [else      (redirect-to "/login-to-profile" temporarily)]))

(define (do-create-account-submitted req)
  (def un (bytes->string/utf-8 (get-binding #"username")))
  (def p  (get-binding #"password"))
  (def e  (bytes->string/utf-8 (get-binding #"email")))
  (with-handlers ([exn:fail:user:bad?
                   (λ (e)
                     (def msg (exn-message e))
                     ; (displayln msg) ; todo: show user
                     (redirect-to "/login" temporarily))])
    (def u (create-user un p e))
    (def s (register-session u)) ; stores session i db
    (def t (session-token s))
    (redirect-to "/" temporarily
                 #:headers (map cookie->header
                                (list (make-session-cookie t))))))


(define (do-vote req direction entry-id page-number) ; an arrow was clicked on the given page
  (match (current-user)
    [#f (redirect-to "/login-to-vote" temporarily)]    
    [u  (define (register dir)
          (register-vote #:user     u
                         #:entry-id entry-id
                         #:ip       (request-client-ip req)
                         #:dir      dir))
        (match direction
          ["up"   (register 'up)]
          ["down" (register 'down)]    
          [else    'do-nothing])
        ; to make sure a reload doesn't resubmit, we redirect to the front page
        (redirect-to (~a "/home/page/" page-number) temporarily)]))


(define (do-submit req)
  (match (current-user)
    [#f (redirect-to "/login-to-submit" temporarily)]
    [u  (def result (html-submit-page))
        (response/output (λ (out) (display result out)))]))

;;;
;;; do-submit
;;;

(define (do-entry-submitted req)
  (def logged-in? (and (current-user) #t))
  (def u          (current-user))
  ; We get here when the form on the "Submit new entry" page is submitted.
  (def url   (get-binding #"url"   bytes->string/utf-8))
  (def title (get-binding #"title" bytes->string/utf-8))
  (def ip    (request-client-ip req))
  
  ; If the submitted url and title are valid, we will insert an
  ; entry in the database and redirect to the database.
  ; Unless the submissions already is in the database, in which
  ; case we allow it if the previous submission is older than a month.
  
  ; If the data is invalid, we need to show the submit page again,
  ; this time with validation results.
  
  (define vu (validate-url   url))   ; see "validation.rkt" for definition
  (define vt (validate-title title))

  (cond
    [logged-in?
     (cond
       [(all-valid? vu vt)
        (def e (get-entry/url url))
        (cond
          [(and e (recent? e))
           ; entry already in database and recently
           ; if we had comments, we would redirect to the comments page
           (redirect-to "/resubmission")]
          [else
           (register-entry #:title title #:url url #:user u #:ip ip)
           ; to make sure a reload doesn't resubmit, we redirect to the front page
           (redirect-to "/" temporarily)])]
       [else
        (def result (html-submit-page #:validation (list vu vt)))
        (response/output
         #:headers (list (header #"Location" #"foo"))
         (λ (out) (display result out)))])]
    [else
     (redirect-to "/" temporarily)]))
