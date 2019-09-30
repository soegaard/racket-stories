#lang at-exp racket/base
;;;
;;; VIEW
;;;

; The view presents data from the model to the user.
; In the case of an web-app the view generates html which
; is sent to the user's browser.

; There is a choice to be made on how to represent html in the program.

; A simple approach is to use plain strings, but manipulating html
; in string form has the potential of being very ineffecient (reparseing,
; repeated creation of new strings vhen strings are concatenated etc.)

; A popular choice of representation in the Racket/Scheme world is S-expressions.
; Here another choice has been made: html will be represented using the
; (html) element struct from `scribble/html`.  

; The choice has a few advantages:
;   - Scribble's at-expressions can be used to construct html.
;   - calling functions that constructs pieces of html
;     looks the same as a call that constructs actual html elements
;   - typos in tag names will be caught on compile time
;   - it is easy to mix html tags and text

; For each html tag  p, h1, a, img, div, span, etc. there is a function
; of the same name that constructs an element representing a piece of html with that tag.

; The general syntax is:
;     @name[attribute1: "value1" attribute2: "value2"]{Some text}
; When no attributes are needed:
;     @name{Some text}
; When no text is needed:
;     @name[attribute1: "value1" attribute2: "value2"]

; Some examples:
;   @p{This is some text}            (p "This is some text")            <p>This is some text</p>
;   @div[class: "score"]{42 points}  (div 'class: "score" "42 points")  <div class="score">42 points</div>

; Nesting is easy:

;   @p{This @it{word} is in italics.}

;   @div[class: "centered"]{
;     @div[class: "title"]{Racket News}
;     @div[class: "score"]{42 points}}

; Note that one can abbreviate the above as:

;   @div[class: "centered"
;     @div[class: "title"]{Racket News}
;     @div[class: "score"]{42 points}]

; Use ~x to turn an html element into a string.


;;;
;;; Exports
;;;

;; The control needs the following functions:

(provide html-about-page
         ; html-home-page -> replaced by html-list-page
         html-list-page  ; handles  home, new
         html-popular-page
         html-submit-page
         html-login-page
         html-user-page
         html-profile-page
         html-from-page
         html-associate-github-page)


;; Dependencies

(require (for-syntax racket/base)
         racket/format racket/file racket/match racket/string net/sendurl
         urlang/html (only-in scribble/html label)
         (prefix-in html: urlang/html)
         web-server/http/request-structs
         gregor
         "def.rkt" "parameters.rkt" "structs.rkt"
         "validation.rkt"
         "model.rkt")

;;; Test Module

(module+ test (require rackunit))

;;;
;;; Parameters
;;;

(define current-page (make-parameter #f)) ; used by navigation-bar

;;;
;;; External Resources
;;;

(define fontawesome-js "https://kit.fontawesome.com/d1076de1c9.js")

;;;
;;; Internal Resources
;;;

(define racket-logo               "/static/color-racket-logo.png")
(define white-racket-logo         "/static/white-racket-logo.png")
(define white-logo-racket-stories "/static/white-logo-racket-stories.svg")
(define bootstrap-social          "/static/bootstrap-social-gh-pages/bootstrap-social.css")

;;;
;;; STYLING
;;;

;; This web-app uses Bootstrap to style the html.

;; Bootstrap is a so-called CSS framework.
;; By giving an html element a class, the browser will style
;; the element according to a style sheet. The Bootstrap
;; documentation outlines how to use the class names.
;; One advantage of using a CSS framework is that there
;; are many themes to choose from - without needing any
;; changes in the code.

; main-column
;   The main column has a navigation bar at top and a colored middle section below.
;   The styles "container" and "container-fluid" are from Bootstrap.
;   The style main_colum is defined in the section below styling.
(define (main-column . xs)
  @div[class: "container"
        @navigation-bar{}
        @(when (current-banner-message)
           (list @p{}
                 @div[class: "alert alert-warning" role: "alert"
                       (current-banner-message)]))
        @div[class: "container-fluid main_column"
              @xs]])

(define (main-column/no-color . xs)
  @div[class: "container"
        @navigation-bar{}
        @(when (current-banner-message)
           (list @p{}
                 @div[class: "alert alert-warning" role: "alert"
                       (current-banner-message)]))
        @div[class: "container-fluid"
              @xs]])




; navigation-bar
;   The navigation bar appears on top of all pages and shows the user
;   the current (active) page.
(define (navigation-bar)
  ; The nav-item corresponding to the current page
  ; needs the class "active" (makes item stand out)  
  (define (active item)
    (cond [(equal? (~a item) (~a (current-page))) " active"]
          [else                                   ""]))
  (def page (~a (current-page)))
  (def Page (string-titlecase page))
  @navbar[class: "navbar-nav-scroll"
           @a[href: "/"
               @img[class: "racket-logo mr-4" src: white-logo-racket-stories
                     alt:  "racket logo" height: "40px"]]           
           @nbsp @nbsp
           @ul[class: "navbar-nav bd-navbar-nav flex-row mr-auto"
                ; Until home has a sort order that combines age and score,
                ; home sorts the same way as new. No need to display it.
                ;; @li[class: (~a "nav-item" (active 'home))
                ;;      @a[class: "nav-link" href: "/"]{Home}]
                ;; @span[class: "navbar-text"]{ | }
                @li[class: (~a "nav-item" (active 'new))
                     @a[class: "nav-link" href: "/new"]{New}]
                @span[class: "navbar-text"]{ | }
                @li[class: (~a "nav-item" (active 'popular))
                     @a[class: "nav-link" href: "/popular"]{Popular}]
                @span[class: "navbar-text"]{ | }
                @li[class: (~a "nav-item" (active 'submit))
                     @a[class: "nav-link" href: "/submit"]{Submit}]
                @span[class: "navbar-text"]{ | }
                @li[class: (~a "nav-item" (active 'about))
                     @a[class: "nav-link" href: "/about"]{About}]
                ; If the page isn't one of the always-feaured pages, we need to show it
                @(unless (member page '("home" "new" "popular" "submit" "about"))
                   (list @span[class: "navbar-text"]{ | }
                         @li[class: "nav-item active"
                              @a[class: "nav-link" href: (~a "/" page)]{@Page}]))]
           @(login-status)])

(define the-footer
  @footer[@center{@span[class: "text-muted"]{Jens Axel Søgaard @br jensaxel@"@"soegaard.net}}])


(define (login-status)
  (def u (current-user))
  (def name (and (user? u) (user-username u)))
  (match u
    [#f @ul[class: "navbar-nav bd-navbar-nav flex-row"
             @li[class: "nav-item ml-auto"
                  @a[class: "nav-link" href: "/login"]{login}]]]
    [_ @ul[class: "navbar-nav bd-navbar-nav flex-row"
            @a[class: "nav-link" href: "/profile"]{@name}
            @span[class: "navbar-text"]{ | }
            ; @a[class: "nav-link" href: "/logout"]{logout}
            @logout-link-form{}
            ]]))

(define (logout-link-form)
  @form[class: "" name: "logout_form" action: "/logout-submitted" method: "post"
         @(html-a-submit "logout_form" "/logout-submitted" "logout" #:class "nav-link")])

(define (submit-button . xs)
  (apply button (list* type: "submit" class: "btn btn-primary" @xs)))

(define (form-group . xs)
  (apply div (list* class: "form-group"  @xs)))
    
(define (form-input #:class [class ""] . xs)
  ; Bootstrap uses the class "form-control" for inputs.
  ; If we need additional classes, we need to use #:class,
  ; unfortunately it is not legal html to emit two separate
  ; class attributes (sigh).
  (apply input (list* class: (~a "form-control " class) xs)))

(define (navbar . xs)
  ; Bootstrap uses the class "navbar" for the navigation bar.
  ; A dark navigation bar will get white text.
  (apply html:nav (list* class: "navbar navbar-expand-lg navbar-dark uppernav" @xs)))

(define (navbar-brand-a . xs)
  ; Bootstrap uses the class "navbar-brand" for the brand (logo).
  (apply a (list* class: "navbar-brand" xs)))

(define (list-group . xs)
  (apply div (list* class: "list-group" @xs)))

(define (list-item . xs)
  (apply div class: "list-group-item" @xs))

(define (list-item-action . xs)
  (apply a (list* class: "list-group-item" xs)))


;;;
;;; Stylesheet
;;;

; border: 1px solid red

(define stylesheet "
    .mw600px        { max-width: 600px; } 
    .mb-very-small  { margin-bottom: 0.1rem; }
    body            { font-size: 1rem; margin: 2rem; }
    a               { display: inline; }
    a.from          { color: #212529; }
    .vote-icon      { font-size: 1.5rem; color: var(--purple); margin-bottom: 0.2rem;}
    .uppernav       { background-color: var(--purple);}
    .main_column    { background-color: #f6f6ef; }
    .col-main       { background-color: #f6f6ef; }

    .entry-row          { vertical-align: middle; }
      .rank-col         { vertical-align: middle; text-align: right;   font-size: 300%; }
      .arrow-col        { vertical-align: middle; text-align: center; }
        form.arrows     { vertical-align: middle; margin: auto;       }
          .updowngrid   { vertical-align: middle; margin: auto; padding: 0px;  }
          .updowngrid a { display: grid; }
      .titlescore-col   { vertical-align: middle; text-align: left; display: inline-grid; padding-right: 0px;}
        .titlescore     {                         text-align: left; display: inline-grid; margin: auto auto auto 0px;}
      .submitter-name a { color: black; }

    .updowngrid.hidden {visibility: hidden; }

    .col-github-blue    { background-color: #4078c0; }
")


;;;
;;; HTML Page 
;;;


;; Functions to embed scripts and css into a page
(define (script   url) @~a{<script src=@url ></script>})    
(define (link-css url) @~a{<link href=@url rel="stylesheet">})

(define (html-page #:title title #:body body)
  ;; Given a body (a string) wrap it in a basic html template  from 
  ;; the Bootstrap project:
  ;;     http://getbootstrap.com/getting-started/#template
  ;; Finally load icons from FontAwesome.
  (def the-body (match body
                  [(? string? body) body]
                  [(? list? body)   (string-append* (map ~x body))]
                  [else             (~x body)]))
  @~a{
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="google-signin-client_id" content="racket-stories-1568926163791.apps.googleusercontent.com">
    <!-- The above 3 meta tags *must* come first in the head;
         any other head content must come *after* these tags -->

    <!-- Bootstrap (CSS Styling Framework) -->
    <link rel="stylesheet" 
          href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" 
          integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" 
          crossorigin="anonymous">
    <link rel="stylesheet" href=@|bootstrap-social| type="text/css">
     
    <!-- Favicons (site logo in tab bar) -->
    <link rel="apple-touch-icon" sizes="180x180" href="/favicons/apple-touch-icon.png">
    <link rel="icon" type="image/png" sizes="32x32" href="/favicons/favicon-32x32.png">
    <link rel="icon" type="image/png" sizes="16x16" href="/favicons/favicon-16x16.png">
    <link rel="mask-icon" href="/favicons/safari-pinned-tab.svg" color="#6f42c1">
    <meta name="msapplication-TileColor" content="#9f00a7">
    <meta name="theme-color" content="#6f42c1">

    
    <!-- FontAweseome (for icons) -->
    @script[fontawesome-js]

    <!-- Our Stylesheet -->
    <!-- TODO: Put this in an external file -->
    <style> @|stylesheet| </style>

    <!-- Google Sign-In (only needed on login page) -->
    <!-- Need an authorized domain to test Google Sign-In --> 
    <!-- <script src="https://apis.google.com/js/platform.js" async defer></script> -->

    <!-- Title -->
    <title>@title </title>
  </head>
  <body>
      @the-body
      @(~x the-footer)
      <!-- Include all compiled plugins (below), or include individual files as needed -->
      <!-- jQuery first, then Popper.js, then Bootstrap JS -->
      <!-- jQuery (necessary for Bootstrap's JavaScript plugins) -->
    <script src="https://code.jquery.com/jquery-3.3.1.slim.min.js"
            integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" 
            crossorigin="anonymous"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js" 
            integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1"
            crossorigin="anonymous"></script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js"
            integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM"
            crossorigin="anonymous"></script>
    <script> function onSignIn(googleUser) {
               var profile = googleUser.getBasicProfile();
               console.log('ID: ' + profile.getId()); // Do not send to your backend! Use an ID token instead.
               console.log('Name: ' + profile.getName());
               console.log('Image URL: ' + profile.getImageUrl());
               console.log('Email: ' + profile.getEmail()); // This is null if the 'email' scope is not present.
               }
    </script>
  </body>
  </html>})

;;;
;;; The User Page
;;;

(define (list->table xss)
  (define (list->row xs) @tr[ (map td xs) ])
  @table[class: "table"
    @(apply tbody (map list->row xss))])

(define (html-github-info #:github-user gu)
  (define (link   url) @a[href: url]{@url})
  (define (avatar url) @img[src: url width: "130px" height: "130px"])
  (def github-user-info
    (and gu (list (list "name:"    (github-user-real-name  gu))
                  (list "user:"    (github-user-login      gu))
                  (list "profile:" (link (github-user-github-url gu)))
                  (list "blog:"    (link (github-user-blog-url   gu)))
                  (list "avatar:"  (avatar (github-user-avatar-url gu))))))
  (list->table (or github-user-info '())))
  


(define (html-user-page user #:github-user [gu #f])
  (current-page "user")
  
  (def u user)
  (def name    (user-username u))
  (def created (~t (user-created-at u) "E, MMMM d, y"))
  (def about   (user-about u))

  (html-page
   #:title "User - Racket Stories"
   #:body @main-column{ @h2{User Profile}
                        @p{@(list->table 
                             (list (list "user:"    name)
                                   (list "created:" created)
                                   (list "about:"   about)))}
                        @(when gu
                           (list
                            @h2{Github}
                            @(html-github-info #:github-user gu)))}))

(define (html-profile-page user #:github-user [gu #f])
  (current-page "profile")
  
  (def u user)
  (def name    (user-username u))
  (def a       (user-about u))
  (def about   (if (equal? a "")  "Enter a text to show other users" a))
  (def created (~t (user-created-at u) "E, MMMM d, y"))

  (def user-info
    (list (list "user:"    name)
          (list "created:" created)))
                      
  (html-page
   #:title "Profile - Racket Stories"
   #:body
   (list @main-column{@h2{User Profile}
                      @p{@(list->table user-info)}                       
                      @form[name: "profile_form" action: "/profile-submitted" method: "post" novalidate: ""]{
                        @input[name: "action" value: "submit" type: "hidden"]
                        @form-group[@label[for: "about"]{About}
                          @(render-form-input "about" about #f)]
                        @submit-button{Update}}                      
                      @p{}}

         @p{}
         
         @div[class: "container"
           @div[class: "container main_column"
             @div[class: "row"
               @div[class: "col-sm-6 col-main"          
                     @h2{Github}
                     @(cond
                        [(not gu)  @span[@p{}
                                         @(github-button)
                                         @p{}  
                                         @p{ Click the Github-button to link your Github 
                                                   account to your Racket Stories account.}
                                         @p{Afterwards you can login with Github.}]]
                        [else            @p{@(html-github-info #:github-user gu)}])]]]])))


;;;
;;; The About Page
;;;

(define (html-about-page)
  (define (github-a name)
    (def url (~a "https://github.com/soegaard/racket-stories/tree/master/racket-stories/" name))
    (@a[href: url]{@tt{@name}}))
  ; links to the source files at Github
  (def model.rkt   (github-a "model.rkt"))
  (def view.rkt    (github-a "view.rkt"))
  (def control.rkt (github-a "control.rkt"))
  (def server.rkt  (github-a "server.rkt"))
  
  (current-page "about") ; used by navigation-bar to hightlight the current page
  (html-page
   #:title "About - Racket Stories"
   #:body
   @main-column{
     @h1{About}
     @p{Welcome to Racket Stories.}
     @p{Racket Stories is a place where you can find and share links to
        anything Racket related: blog posts, tutorials, new packages, papers etc.}
     @p{Racket Stories is at the same time a show case for a "production" web-site 
        written in Racket with the source being available at Github.
        There are many ways to write web-apps, so the source is just an example
        of one way to use Racket.}
     @p{If you find bugs, large or small, submit an issue at Github}

     @p{Have fun with it.
             @br @em{Jens Axel Søgaard}
             @br @tt{jensaxel@"@"soegaard.net}}

     @h2{Installation}

     @p{The main reason for publishing the source is that I want people to
            have something to try and tinker with, without inventing anything
            from scratch. All you need to try the web-site on your own machine
            is to clone the Github repo, install a few packages, and run the code.
            No configuration is needed.}

     @p{The source code for this application is available at Github.:
              @div[class: "mw600px mx-auto text-left"
                    @a[href: "https://github.com/soegaard/racket-stories"]{https://github.com/soegaard/racket-stories}]
              @br
              If you prefer a smaller example, look at @tt{listit1}, @tt{listit2} and @tt{listit3} here:
              
              @div[class: "mw600px mx-auto text-left"
                    @a[href: "https://github.com/soegaard/web-tutorial"]{https://github.com/soegaard/web-tutorial}]
              @br
        Clone the repository. Install a few packages (see next paragraph). 
        Open "server.rkt" in your editor and run it.
        This will start a web-server and instruct your web-browser to show you
        the front page.}

     @p{Here are the packages you need besides the packages that are included in
        the standard Racket distribution.
              @div[class: "mw600px mx-auto text-left"
                @code{raco pkg install deta}      @br
                @code{raco pkg install urlang}    @br
                    @code{raco pkg install threading} @br]}

     @h2{Overview}

     @p{The app uses the Model-View-Controller architecture (MVC).}
            
     @p{To make this super clear the app consists of the files @model.rkt, @view.rkt and @|control.rkt|. 
        In a larger application it would make sense to make separate folders for the model,
        view and control @mdash and split the files into smaller pieces.
        For example each html page in @view.rkt could get its own file.}

     @p{The @em{model} (see @model.rkt) consists of a database in which the
        the urls and scores are stored. You can open @model.rkt in your editor,
        run it and then experiment in the repl with @tt{(top 3)}, @tt{(page 1)} etc.}

     @p{The @em{view} (see @view.rkt) presents data from the model to the user.
        In the case of an web-app the view generates html which
        is sent to the user's browser.}

     @p{The @em{control} (see @control.rkt) accepts input from the user,
        reads or writes to the model, and then sends output back using
        the view to present the output}

     @p{The server (see @server.rkt) takes care of receiving requests
        from the users and sending back responses. For this application
        the server will send all requests to the control to be handled.}
     
     @h2{Packages}
     
     @p{Some of the packages the app relies on are listed below.

            @div[class: "mw600px mx-auto"
              @list-group[
                @list-item-action[href: "https://docs.racket-lang.org/deta/index.html"]{
                  @h5{deta}
                  @p{The Deta library is used to map database tables to Racket structs.
                     After updating the values of a struct, Deta can send the relevant changes to the database.
                     In fact Deta can be used to perform Create-Read-Update-Delete (CRUD) operations on
                     your model @mdash as well as making arbitrary queries.}
                  @p{You can think of Deta as an Object-Relational Mapper using structs instead of objects.}}
                @p{}
                @list-item-action[href: "https://docs.racket-lang.org/db/index.html"]{
                  @h5{db}
                  @p{The @tt{db} library is the foundation for Deta.
                     We are also using it to create the initial database connection.
                     Using @tt{db} has the effect that we can easily switch between
                     PostgreSQL, SQLite and MySQL backends.}}
                @p{}
                @list-item-action[href: "https://docs.racket-lang.org/scribble-pp/html.html"]{
                  @h5{scribble/html}
                  @p{A popular approach of representing html is S-expression.
                     In this app we have however chosen to represent html as structures.
                     This choice has a few advantages: First of all we can use Scribble's
                     at-expressions to construct html. Second, calling functions that
                     constructs pieces of html looks the same as a call that constructs
                     an actual html construct.}}
                @p{}
                @list-item-action[href: "https://github.com/soegaard/urlang/tree/master/urlang/html"]{
                  @h5{urlang/html}
                  @p{A wrapper of @tt{scribble/html} that provides a few utilities that
                       makes it easier to work with the element structure of @tt{scribble/html}.
                       One of the provided functions is @tt{~x} which converts an element
                       into string.}}
                @p{}
                @list-item-action[href: "https://docs.racket-lang.org/threading/index.html"]{
                  @h5{threading}
                  @p{The @tt{threading} library provides a few macros that flatten nested function calls.
                         Here we/Deta use @tt{~>} to construct database queries with a nice syntax.}}
                @p{}
                @list-item-action[href: "https://docs.racket-lang.org/web-server/index.html"]{
                  @h5{web-server}
                  @p{The @tt{web-server} library provides tools for working with requests
                         and responses. It also makes it easy to get a server up and running
                         without a lengthy installation and configuration process.}}
                @p{ }
                ]]}

     }))

;;;
;;; Associate Github 
;;;

(define (html-associate-github-page)
  (current-page "associate-github")
  (html-page
   #:title "Associate Github - Racket Stories"
   #:body
   @main-column{
                @h1{Associate a Github Account}
                @p{Before you can login directly with a github account,
                   you need to create a standard account first.
                   While logged in you can associate a github account,
                   by signing in with github.}}))


;;;
;;; The Submit Page (submit new entry)
;;;

;; The page has two input fields: one for the url and one for the title.
;; The control provides validation results we can use to fill out
;; feedback if needed.

(define (render-form-input name initial-value validation-result)
  (def vr validation-result)
  (define (maybe-wrap v initial) (or v (validation initial #f #f "")))
  (defm (validation the-value _ _ feedback) (maybe-wrap vr initial-value))
  
  ; The css only displays the feedback when the input has the class "is-invalid",
  ; so we can include the feedback div even when it is not needed.
  @form-input[#:class (input-class-validity vr)  name: name type: "text" value: the-value
    @div[class: "invalid-feedback"]{@feedback}])


(define (html-submit-page #:validation [the-validation #f])
  (current-page "submit")
  (defm (or (list vu vt) (and vu vt #f)) the-validation)
  (html-page
   #:title  "Submit a new entry - Racket Stories"
   #:body
   @main-column{
     @h2{Submit a new entry}
     @form[name: "submitnewform" action: "/entry-submitted" method: "post" novalidate: ""]{
       @input[name: "action" value: "submit" type: "hidden"]
       @form-group[
         @label[for: "url"]{URL}
         @(render-form-input "url" "https://" vu)]
       @form-group[
         @label[for: "title"]{Title}
         @(render-form-input "title" "A Title" vt)]
       @submit-button{Submit}}
     @p{}
     @p{Everything Racket related has interest.}}))

;;;
;;; The Home Page and the New Page
;;;

; The only difference between the home page and the new page
; is the sorting order. The keyword argument #:new? determines,
; whether we are on the home page or the new page.

(define (html-list-page name page-number first-rank entries
                        #:message [message #f]
                        #:period  [period ""]
                        #:votes   [votes #f])
  (current-page name)
  ; name is one of "home", "new" or "popular"
  (def Name      (string-titlecase name)) ; "Home", "New" or "Popular"
  (def next      (+ page-number 1))
  (def more-url  (match name
                   ["new"     (~a "/new/page/"  next)]
                   ["home"    (~a "/home/page/" next)]
                   ["popular" (~a "/popular/"   period "/page/" next)]
                   [_ (error 'html-list-page "expected home, new or popular.")]))
  (html-page
   #:title (~a Name "- Racket Stories")
   #:body
   @main-column{
     @(when message (list @br @message @br))
     @(html-list-of-entries page-number first-rank entries
                            #:voting?  #t
                            #:ranking? #t
                            #:votes    votes)
     @nav[aria-label: "more"
       @ul[class: "pagination"
         @li[class: "page-item"
              @a[ ; class: "page-link"
                  href: more-url]{More}]]]
     @p{ }}))

(define (html-popular-page page-number first-rank entries period votes)
  (define (url period)    (~a "/popular/" period "/page/" 0))
  (define (make-a p txt)
    (def same? (equal? p period))
    @a[class: @~a{dropdown-item @(if same? "active" "")}
        href: (if same? "#" (url p))]{@txt})

  (def msg @span{The most popular entries for the last:
                 @div[class: "btn-group mb-very-small"
                   @button[class: "btn btn-outline-primary btn-sm dropdown-toggle"
                            type=: "button"
                            data-toggle: "dropdown"
                            aria-haspopup: "true"
                            aria-expanded: "false"]{@period}
                   @div[class: "dropdown-menu"
                         (make-a "day"   "day")
                         (make-a "week"  "week" )
                         (make-a "month" "month")
                         (make-a "year"  "year")
                         (make-a "all"   "century")]]})
  
  (html-list-page "popular" page-number first-rank entries
                  #:message msg
                  #:period period
                  #:votes votes))

;;;
;;; From
;;;

(define (html-from-page entries)
  (current-page "from")
  (html-page
   #:title "From - Racket Stories"
   #:body
   @main-column{
     @(html-list-of-entries 0 1 entries #:voting? #f #:ranking? #t)
     @p{ }}))


;;; List of entries

(define (html-list-of-entries page-number first-rank entries
                              #:voting?  [voting?  #f]
                              #:ranking? [ranking? #f]
                              #:votes    [votes    #f])
  
  (def pn page-number)
  (define logged-in? (and (current-user) #t))

  ; if votes is present it contains a list of entry ids of the
  ; entries the user already voted on - don't hide them
  ; (don't omit them - that breaks alignment)
  (define (show-thumbs-up? eid)
    (or (not votes)
        (and votes (not (memv eid votes)))))
  
  (define (entries->rows entries)
    (for/list ([e entries] [rank (in-naturals first-rank)])
      (entry->table-row e rank)))
  
  (define (entry->table-row e rank)
    (defm (struct* entry ([title the-title] [url the-url] [site site] [score the-score] [id id]
                                            [submitter-name submitter-name])) e)
    (def  form-name (~a "arrowform" id))
    @div[class: "entry-row row"
          ; hide rank with `d-none` if needed (element is kept to keep size)
          @span[class: @~a{rank-col  col-auto @(if ranking? "" "d-none")}]{ @(or rank "0") }
          ; to teach new users, we display the voting arrows, even if they are logged-out
          @(when voting?
             @span[class: "arrow-col col-auto row" 
               @form[class: "arrows" name: form-name action: @~a{vote/@id} method: "post"
                 @input[name: "arrow" type: "hidden"] 
                   @span[class: (~a "updowngrid" (if (show-thumbs-up? id)  "" " hidden"))
                     @(html-a-submit form-name (~a "/vote/up/"   id "/" pn) (html-icon 'thumbs-up))
                     #;(html-a-submit form-name (~a "/vote/down/" id "/" pn) (html-icon 'chevron-down))]]])
          @span[class: "titlescore-col col"
            @span[class: "titlescore"
              @span[@a[href: the-url]{ @the-title } " (" @a[class: "from" href: (~a "/from/" id)]{@site} ") "]
              @span[class: "score"]{@the-score points by
                     @span[class: "submitter-name"
                            @a[href: (~a "/user/" submitter-name) ]{ @submitter-name }]}]]])
                                                       
  @span[class: "entries container-fluid"]{
    @(entries->rows entries)})


(define (html-a-submit form-name action text #:class [class ""])  
  @a[class: class href: @~a{javascript:
                            document.@|form-name|.action='@|action|';
                            document.@|form-name|.submit(); 
                            }]{@text})


;;;
;;; Login Page
;;; 


(define (html-login-page #:validation [the-validation #f])
  (current-page "login")
  (html-page
   #:title "Login or create new account - Racket Stories"
   #:body
   @main-column/no-color[
    @div[class: "container"
      @div[class: "row"
        @div[class: "col-sm-6 col-main"
          @nbsp
          @h1{Login}
          @form[name: "loginform" action: "/login-submitted" method: "post"]{
            @form-group{
              @label[for: "username"]{Username}
              @form-input[name: "username"   type: "text" value: ""]}
            @form-group{
              @label[for: "password"]{Password}
              @form-input[name: "password" type: "password" value: ""]}
            @submit-button{Login}}

     ;     @form[name: "loginform" action: github-action-url method: "post"]{
     ;       @submit-button{Login with Github}}

     
     ; @h2{Sign-In with Google}
     ; @div[class:"g-signin2" data-onsuccess: "onSignIn"]

          @nbsp @br @br
     
          @h1{Create Account}
          @form[name: "createaccountform" action: "/create-account-submitted" method: "post"]{
            @form-group{
              @label[for: "username"]{Username}
              @form-input[name: "username"   type: "text" value: ""]}
            @form-group{
              @label[for: "password"]{Password}
              @form-input[name: "password" type: "password" value: ""]}
            @form-group{
              @label[for: "email"]{Email (optional)}
              @form-input[name: "email" type: "email" value: ""]}
            @submit-button{Create Account}}
              @p{@nbsp}]

          @div[class: "col-sm-1"]

          @div[class: "col-sm-5"
            @nbsp
            @h1[class: "text-center"]{Login with Github}
            
            @p[(github-button)]
              @p{You can login with Github, if you have linked your Racket Stories
                 account to your Github account.}
              @p{To link the accounts: 
                    @ul[@li{login to Racket Stories (with name/password)}
                        @li{go to your profile page (click your username at the top, right)}
                        @li{click the "Sign in with Github" button}]}]    
            ]]]))

(define (github-button)
  ; (def github-action-url "https://github.com/login/oauth/authorize?client_id=ec150ed77da7c0f796ec")  
  @a[class: "btn btn-block" href: "/github-login" style: "border: 0;"
      @button[type: "button" class: "btn col-github-blue"
        @span[style: "color: #ffffff;"]{
          @i[class: "fab fa-github fa-2x" 
             style: "vertical-align:middle;"]{} 
          @nbsp Sign in with Github}]])

          

;;;
;;; Icons
;;;

; Icons are from FontAwesome. Used for the up/down arrows.

(define (html-icon name)
  @i[class: (~a "vote-icon fas fa-" name)])
