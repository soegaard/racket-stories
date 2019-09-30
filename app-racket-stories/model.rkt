#lang racket/base
;;;
;;; MODEL
;;;

;; This file contains the underlying model - i.e. the database.
;; 

(provide
 ;; Entry
 (except-out (schema-out entry) make-entry)
 register-entry
 ; create-entry
 get-entry
 get-entry/url     ; get youngest entry with a given url
 ~entry            ; format entry as a string
 ; insert-entry      ; insert entry into database 
 increase-score    ; increase score of entry
 decrease-score    ; decrease score of entry
 top               ; top entries (highest scores)
 page              ; page of entries
 newest            ; page of entries, sort order after age
 popular           ; page of entries, sort order after score (i.e. vote count)
 from-site         ; entries from given site
 url-in-db?        ; is url already in database?
 recent?           ; is the entry less than a month old?
 PAGE-LIMIT

 ;; User
 (except-out (schema-out user) make-user)
 create-user
 get-user
 authenticate-user
 change-user-about

 ;; Github users and state
 (except-out (schema-out github-user) make-github-user)
 login-github-user
 get-github-user/user
 
 get-new-github-state ; return string with a new random string
 valid-github-state?  ; was the input a valid string?

 ;; Votes
 register-vote
 ; insert-vote
 ; create-vote
 user-votes-on-popular ; list of entry-ids already voted on
 user-votes-on-newest

 has-user-voted-on-entry?

 ;; Sessions
 get-session
 session-token
 register-session
 session-expired?
 ; session-logged-in? 
 session-terminate
 session-user-id
 )


;;;
;;; Dependencies
;;;

(require racket/format (except-in racket/list group-by) racket/match
         racket/sequence racket/string racket/runtime-path
         net/url openssl/sha1 db deta gregor gregor/period threading
         json
         (except-in crypto bytes->hex-string)
         "def.rkt" "exn.rkt" "structs.rkt"
         "authentication.rkt" "database.rkt" "deployment.rkt"
         "parameters.rkt" "user-names.rkt")

;;;
;;; Utilities
;;;

(define (safe-first xs)
  (and (pair? xs) (first xs)))


;;;
;;; CONFIGURATION
;;;

(define PAGE-LIMIT (make-parameter 30))   ; number of entries on each page


;;;
;;; Database Connection
;;;

(define db (connect-to-database))  ; see "database.rkt"


;;;
;;; Utilities
;;;

; lookups : query -> list
;   run query and return the results as a list
(define (lookups query)
  (for/list ([v (in-entities db query)])
    v))

;;;
;;; MODEL
;;;


;;;
;;; Entry
;;;

;; Each entry in our database consists of a title to display,
;; an url to the article and a score representing the votes.

(define-schema entry
  ([id             id/f       #:primary-key #:auto-increment]
   [title          string/f   #:contract non-empty-string?]
   [url            string/f   #:contract non-empty-string?]
   [site           string/f]
   [score          integer/f]
   [created-at     datetime/f]
   [submitter      id/f]       ; user id
   [submitter-name string/f]   ; redundant, but saves database access
   ))

(define (create-entry #:title          title
                      #:url            url
                      #:score          score
                      #:created-at     [created-at (now)]
                      #:submitter      submitter
                      #:submitter-name submitter-name)
  (def u (string->url url))
  (def site (if u (url-host u) ""))

  (make-entry #:title          title
              #:url            url
              #:site           site
              #:score          score
              #:created-at     created-at
              #:submitter      submitter
              #:submitter-name submitter-name))



;; Since we expect many entries in our database, we will think of them 
;; as divided into pages. The number of entries in each page is given by 
;; the parameter PAGE-LIMIT.


;; DATABASE INSERTION AND UPDATES

; get-entry : entry-or-entry-id -> entry
(define (get-entry entry)
  (match entry
    [(? entry?   entry) entry]
    [(? integer? id)    (lookup db (~> (from entry #:as e) (where (= id ,id))))]
    [_ (error 'get-entry (~a "entry or entry id expected, got: " entry))]))

(define (get-entry/url url)
  (def es (entries-with-url url))
  (match es
    ['() #f]
    [_   (first (sort es datetime>? #:key entry-created-at))]))

(define (insert-entry entry)
  (insert-one! db entry))

(define (increase-score entry)
  (def e (get-entry entry))  
  (update! db (update-entry-score e (λ (score) (+ score 1)))))

(define (decrease-score entry)
  (def e (get-entry entry))
  (update! db (update-entry-score e (λ (score) (- score 1)))))


(define (register-entry #:title title #:url url #:user u #:ip ip)
  (def e (insert-entry
          (create-entry #:title title
                        #:url url
                        #:score 10
                        #:submitter (user-id u)
                        #:submitter-name (user-username u))))
  (insert-vote (create-vote (user-id u) (entry-id e) ip)))


;;; FORMATTING

(define (~entry e)
  (set! e (get-entry e))
  (defm (entry _ id title url site score created-at submitter submitter-name) e)
  (~a id ":'" title "':'" url "':(" site ") " score ":" submitter-name))


;;; DATABASE RETRIEVAL

(define (top n)
  (lookups (~> (from entry #:as e)
               (order-by ([e.score #:desc]))
               (limit ,n))))

(define (page n) ; n = page-number
  (lookups (~> (from entry #:as e)
               (order-by ([e.score #:desc]))
               (limit  ,(PAGE-LIMIT))
               (offset ,(* (PAGE-LIMIT) n)))))


(define (query:newest n) ; n = page-number
  (~> (from entry #:as e)
      (order-by ([e.created-at #:desc]))
      (limit  ,(PAGE-LIMIT))
      (offset ,(* (PAGE-LIMIT) n))))

(define (newest n) ; n = page-number
  (lookups (query:newest n)))

(def 1year     (sql-interval   1 0 0 0 0 0 0))
(def 1month    (sql-interval   0 1 0 0 0 0 0))
(def 1week     (sql-interval   0 0 7 0 0 0 0))
(def 1day      (sql-interval   0 0 1 0 0 0 0))
(def 1eternity (sql-interval 100 0 0 0 0 0 0))


(define (popular period n)
  (def query (query:popular period n))
  (lookups (query:popular period n)))

(define (query:popular period n) ; n = page-number counting from 0
  ; period is one of the strings: day, week, month, all
  (def query
    (match (dbsystem-name (connection-dbsystem db))
      ['sqlite3    (def p (match period
                            ["day"   "-1 day"]
                            ["week"  "-7 days"]
                            ["month" "-1 month"]                            
                            ["year"  "-1 year"]
                            [_       "-100 years"]))
                   (~> (from entry #:as e)
                       (where (>= e.created-at (DateTime "Now" "LocalTime" ,p)))
                       (order-by ([e.score #:desc]))                       
                       (limit  ,(PAGE-LIMIT))
                       (offset ,(* (PAGE-LIMIT) n))
                       ; now we have a page of entries, now add information
                       ; on which entries the user already voted on
                       #;(join vote #:as v #:on (and (= v.user-id  ,uid)
                                                     (= v.entry-id e.id)))
                       )]
      ['postgresql   (def p (match period
                              ["day" 1day] ["week" 1week] ["month" 1month]
                              ["year" 1year] [_ 1eternity]))
                     (~> (from entry #:as e)
                       (where (> e.created-at (- (now) (cast ,p interval))))
                       (order-by ([e.score #:desc]))                       
                       (limit  ,(PAGE-LIMIT))
                       (offset ,(* (PAGE-LIMIT) n)))]
      [n (error (~a n " is not supported"))]))
  query)


(define (entries-with-url url-str)
  (lookups (~> (from entry #:as e)
               (where (= url ,url-str)))))

(define (count-entries-with-url url-str)
  (lookup db (~> (from entry #:as e)
                 (where (= url ,url-str))
                 (select (count *)))))

(define (url-in-db? url-str)
  (positive? (count-entries-with-url url-str)))

(define (count-entries)
  (lookup db (~> (from entry #:as e)
                 (select (count *)))))

(define (from-site s)
  (for/list ([e (in-entities db
                  (~> (from entry #:as e)
                      (where (= site ,s))
                      (order-by ([e.score #:desc]))))])
    e))

(define (recent? e)
  ; "recent" means within a month
  (def created-at (entry-created-at e))
  (datetime>? created-at (-period (now) (period [months 1]))))


;;;
;;; User
;;;

(define (digest-value? x) (and (integer? x) (<= 0 x 3)))

(define-schema user
  ([id              id/f        #:primary-key #:auto-increment]
   [username        string/f    #:unique #:contract non-empty-string?]
   [key             string/f] ; key derived from password and salt
   [email           string/f]
   [email-validated boolean/f]
   [created-at      datetime/f]
   [about           string/f]   ; text written by the user
   [send-digest     integer/f   #:contract digest-value?]  
   [last-digest-at  datetime/f]))

(define <digest:no>     0)
(define <digest:hourly> 1)
(define <digest:daily>  2)
(define <digest:weekly> 3)

(define (insert-user user)
  (insert-one! db user))

(define (count-users)
  (lookup db (~> (from user #:as u)
                 (select (count *)))))

(define (get-user [user #f] #:username [username #f])
  (match user
    [(? user? u)     u]
    [(? integer? id) (get-user/id id)]
    [_ (match username
         [(? string? un) (get-user/username un)]
         [#f (if (string? user)
                 (get-user/username user)
                 (error 'get-user (~a "neither user(id) or username provided, got: " user)))])]))

(define (~user u)
  (set! u (get-user u))
  (defm (struct* user ([username un] [email e] [email-validated ev])) u)
  (~a un "(" e ")" (if ev " " "!")))

(define (list-users)
  (for/list ([e (in-entities db (from user #:as u))])
    e))

(define (get-user/id id)
  (lookup db (~> (from user #:as u) (where (= id ,id)))))

(define (get-user/username username)
  (lookup db (~> (from user #:as u)
                 (where (= username ,username)))))

(define (username-in-db? username)
  (positive? (lookup db (~> (from user #:as u)
                            (select (count *))))))


(define (create-user username password email)
  (unless (good-username? username)
    (error (exn:fail:user:bad (bad-username-reason username))))

  ; TODO create-user : validate email
  
  (def u (make-user #:username        (normalize-username username)
                    #:key             (derive-key password) ; also embeds salt
                    #:email           email
                    #:email-validated #f
                    #:created-at      (now)
                    #:about           ""
                    #:send-digest     <digest:no>
                    #:last-digest-at  (now)))
  (with-handlers ([exn:fail:sql?
                   (λ (e)
                     (raise (exn:fail:user:bad
                             "username in use" (current-continuation-marks))))])
    ; Since the field "username" has the #:unique property,
    ; inserting an existing "username" will provoke an sql error.
    ; Note: We are not using `username-in-db?` first.
    ;       In theory two different users could register at almost the same time
    (insert-user u)))

(define (change-user-about u a)
  (update! db (update-user-about u (λ (_) a))))


; authenticate user, return the user on success
(define (authenticate-user username password)
  (def u (get-user/username username))
  (match u
    [#f (authentication-error "username not in db")]
    [u  (if (verify-password password (user-key u))
            u
            (authentication-error "wrong password"))]))

;;;
;;; Github
;;; 

(define-schema github-user
  ([id         id/f        #:primary-key #:auto-increment]
   [user-id    integer/f]            ; racket-stories user-id   
   [github-id  integer/f   #:unique] ; github user id
   [login      string/f    #:unique] ; github login (username)
   [real-name  string/f]             
   [email      string/f]
   [github-url string/f]
   [avatar-url string/f]
   [blog-url   string/f]))

; To prevent man-in-the-middle attacks, we attach a state
; to each github login. When we get a code back from Github,
; we need to check that the received state is one we sent out.
(define-schema github-state
  ([state          string/f]))

;; State Related

(define (get-new-github-state)
  (define new-state (bytes->hex-string (crypto-random-bytes 16)))
  (insert-one! db (make-github-state #:state new-state))
  new-state)

(define (valid-github-state? state) ; string -> boolean
  ; the state is valid, if it is in the database
  ; if so it is removed, so it can't be reused
  ; if not, it wasn't valid

  ; we delete the state (and count the number of deleted rows)
  (def result (query db (delete (~> (from github-state #:as s)
                                    (where (= s.state ,state))))))
  
  (match (assq 'affected-rows (simple-result-info result))
    [(cons _ n) (not (zero? n))]
    [_ #f]))


;; User Related
         
(define (insert-github-user github-user)
  (insert-one! db github-user))

(define (count-github-users)
  (lookup db (~> (from github-user #:as gu)
                 (select (count *)))))

(define (create-github-user user-id github-ht)
  (define (get n [convert values] [default ""])
    (convert (hash-ref github-ht n default)))

  (def gu (make-github-user #:user-id    user-id
                            #:github-id  (get 'id values 0)                            
                            #:login      (get 'login)
                            #:real-name  (get 'name)
                            #:email      (get 'email)
                            #:github-url (get 'html_url)
                            #:avatar-url (get 'avatar_url)
                            #:blog-url   (get 'blog_url)))
  
  (with-handlers ([exn:fail:sql?
                   (λ (e)
                     (raise (exn:fail:user:bad
                             "likely: github id or github login in use"
                             (current-continuation-marks))))])
    (insert-github-user gu)))


(define (get-github-user/user u)
  (get-github-user/user-id (user-id u)))

(define (get-github-user/user-id uid)
  (lookup db (~> (from github-user #:as gu) (where (= gu.user-id ,uid)))))

(define (get-github-user/github-id gid)
  (lookup db (~> (from github-user #:as gu)
                 (where (= gu.github-id ,gid)))))

(define (get-github-user/github-login login)
  (lookup db (~> (from user #:as u)
                 (where (= login ,login)))))

(define (get-user/github-id gid)
  (def gu (get-github-user/github-id gid))
  (and gu (get-user/id (github-user-user-id gu))))


(define (list-github-users)
  (lookups (from github-user #:as u)))

(define (link-user-and-github-user u gu)
  (update! db (update-github-user-user-id gu (λ (_) (user-id u)))))
  

(define (login-github-user u ht)
  ; 1. If the user is logged (using a password) we know his user-id, so
  ; 1a. if the github-user is present
  ;         register session  (the control must return a cookie with the session token)
  ; 1b  if the github-user is not present
  ;         link the github-user and the user.
  ;         register session  (and send cookie back)

  ; 2. If the user is not logged in,
  ; 2a.  if the the github-user is linked to a user,
  ;         register sesstion  (and send cookie back)
  ; 2b.  if the github is not linked, then
  ;         tell user he needs to make a normal user first

  ; Get the github user (if present)
  (def github-id (hash-ref ht 'id #f))
  (def gu        (and github-id
                      (get-github-user/github-id github-id)))
  
  (cond
    [(and u gu)  (cond
                   [(= (user-id u) (github-user-user-id gu))
                    (register-session u)]
                   [else
                    ; this github account was previously used with
                    ; another racket-stories account
                    (link-user-and-github-user u gu)
                    (register-session u)])]
    [u           (create-github-user (user-id u) ht)                 
                 (register-session u)]
    [gu          (def u (get-user/id (github-user-user-id gu)))
                 (cond
                   [u    (register-session u)]
                   [else (error 'internal "a github user exists without an user")])]
    [else        #f]))
  

;;;
;;; VOTES
;;;

; In order to prevent multiple votes for the same entry from the same user,
; we need store past votes. When we get to Hacker News size we will
; split entries into "active" and "archived" (say older than a month) 
; entries and only keep votes for active entries.
; Now we just store all votes.

(define-schema vote
  ([id              id/f        #:primary-key #:auto-increment]
   [user-id         id/f]
   [entry-id        id/f]
   [ip              string/f]))

; Note: We don't store whether it is an up or down vote.
;       Don't change your mind. Also ... I intend to remove down votes.

(define (has-user-voted-on-entry? user entry) ; structs or ids
  (def u (if (user?  user)  (user-id  user)  user))
  (def e (if (entry? entry) (entry-id entry) entry))
  (not (zero? (lookup db (~> (from vote #:as v)
                             (where (and (= v.user-id  ,u)
                                         (= v.entry-id ,e)))
                             (select (count *)))))))


(define (register-vote #:user u #:entry-id eid #:ip ip #:dir d)
  ; A logged-in user can vote on an entry, unless:
  ;   - he already voted on that entry
  ;   - he submitted it him-self
  ; When an entry is submitted, it is automatically given a vote,
  ; so we don't need to check self-submission.
  (cond
    [(not (has-user-voted-on-entry? u eid))
     (insert-vote (make-vote #:user-id  (user-id u)
                             #:entry-id eid
                             #:ip       ip))
     (match d
       ['up   (increase-score eid)]
       ['down (decrease-score eid)])
     #t]
    [else #f]))


(define (insert-vote vote)
  (insert! db vote))

(define (create-vote user-id entry-id ip)
  (make-vote #:user-id user-id #:entry-id entry-id #:ip ip))

(define (list-votes)
  (for/list ([v (in-entities db (from vote #:as v))])
    v))

(define (count-votes)
  (lookup db (~> (from vote #:as v)
                 (select (count *)))))


(define (user-votes-on-entries user/id entries-query)
  (def uid (or/integer user/id (user-id user/id)))
  (lookups
   (~> (from (subquery entries-query) #:as e)
       (join vote #:as v #:on (= v.entry-id e.id))
       (where (= v.user-id ,uid))                                             
       (select e.id))))

(define (user-votes-on-popular user/id period page )
  (def uid (or/integer user/id (and (user? user/id) (user-id user/id))))
  (or (and uid
           (user-votes-on-entries uid (query:popular period page)))
      '())) ; anonymous has not voted

(define (user-votes-on-newest user/id page)
  (def uid (or/integer user/id (and (user? user/id) (user-id user/id))))
  (or (and uid
           (user-votes-on-entries uid (query:newest page)))
      '())) ; anonymous has not voted


;;;
;;; SESSIONS
;;;

; When a user successfully logins this fact is recorded as a session.
; 

(define-schema session
  ([id             id/f       #:primary-key #:auto-increment]
   [user-id        integer/f]
   [token          string/f]
   [created-at     datetime/f]
   [expires-at     datetime/f]))

(define (list-sessions)
  (lookups (from session #:as s)))


(define (register-session user/id)
  (displayln (list 'register-session user/id))
  (def uid   (or/integer user/id (and (user? user/id) (user-id user/id))))
  (def token (bytes->hex-string (crypto-random-bytes 16)))
  (cond
    [(integer? uid) (def s (make-session #:user-id    uid
                                         #:token      token
                                         #:created-at (now)
                                         #:expires-at (+period (now) (period [years 10]))))
                    (insert-one! db s)]
    [else #f]))

(define (get-session token)
  (match token    
    [""             #f]
    [(? string? t)  (def s (get-session/token token))
                    (and (not (session-expired? s))
                         s)]
    [_ #f]))

(define (get-session/token token)
  (lookup db (~> (from session #:as s)
                 (where (= s.token ,token)))))

(define (session-expired? session)
  (and session
       (datetime>? (now) (session-expires-at session))))

(define (session-belongs-to-user? s u)
  (= (session-user-id s) (user-id u)))

(define (session-logged-in? s u)
  (def s0 (get-session/token (session-token s)))
  (and s0
       (not (session-expired? s0))
       (session-belongs-to-user? s0 u)))

(define (session-terminate s u)
  (when (session-belongs-to-user? s u)
    (delete! db s)))


;;;
;;; DATABASE CREATION
;;; 

(define (populate-database)
  (when (= (count-users) 0)
    (create-user "foo" #"foo" "foo@foo.com"))
  (when (= (count-entries) 0)
    (define (create title url score)
      (create-entry #:title title #:url url #:score score #:submitter 1 #:submitter-name "foo"))
    (insert-entry (create "Racket News" "https://racket-news.com" 42))
    (insert-entry (create "Racket News - Issue 1" "https://racket-news.com/2019/02/racket-news-issue-1.html" 11))
    (insert-entry (create "Racket News - Issue 2" "https://racket-news.com/2019/02/racket-news-issue-2.html" 12))
    (insert-entry (create "Racket Blog" "https://blog.racket-lang.org" 32))
    (insert-entry (create "Blog - Alexis King" "https://lexi-lambda.github.io/index.html" 23))
    (insert-entry (create "Blog - Greg Hendershott" "https://www.greghendershott.com/" 14))
    (insert-entry (create "Blogs that use Frog" "http://stevenrosenberg.net/racket/2018/03/blogs-that-use-frog.html" 12))))

(define (populate-github-user)
  (def ht (string->jsexpr
#<<HERE
 {
  "login": "foo",
  "id": 461763,
  "avatar_url": "https://avatars2.githubusercontent.com/u/461735?v=4",
  "html_url": "https://github.com/foo",
  "name": "Foo Bar Baz",
  "blog": "http://www.foo.com",
  "email": "foo@foo.com"
}
HERE
))
  (when (= (count-github-users) 0)
    (create-github-user 1 ht)))



(define schemas '(entry user vote session github-user github-state))

(define (create-tables)
  ; Note: This creates the tables if they don't exist.
  ;       Nothing is done if they do exist.
  (for ([s schemas])
    (create-table! db s)))

(define (drop-tables)
  (for ([s schemas])    
    (with-handlers ([exn? void])
      (drop-table! db s))))


; (drop-tables)



(create-tables)

(match the-deployment
  [(or (development) (testing))
   (populate-database)    ; for testing
   (populate-github-user) ; for testing
   ]
  [_ (void)])


