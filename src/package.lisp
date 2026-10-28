;;;; package.lisp

(defpackage #:gopher-browser
  (:use #:common-lisp)
  (:local-nicknames (#:g #:gopher))
  (:export
   ;; settings
   #:data-directory
   #:home-page
   #:set-home-page
   ;; bookmarks
   #:bookmark
   #:bookmark-title
   #:bookmark-url
   #:load-bookmarks
   #:bookmarks
   #:add-bookmark
   #:remove-bookmark
   ;; pages
   #:page
   #:page-url
   #:page-title
   #:page-runs
   #:page-links
   #:page-image
   #:page-mime-type
   #:run
   #:run-text
   #:run-style
   #:run-link
   #:link
   #:link-url
   #:link-label
   #:link-kind
   #:link-item-kind
   #:download-link-p
   #:page-text
   #:page-add-lines
   #:document-page
   #:message-page
   ;; navigation
   #:session
   #:make-session
   #:session-history
   #:session-fetch
   #:session-record
   #:session-back
   #:session-forward
   #:session-back-url
   #:session-forward-url
   #:session-download
   #:session-create-identity
   #:session-identity
   #:outcome
   #:outcome-kind
   #:outcome-url
   #:outcome-page
   #:outcome-prompt
   #:outcome-sensitive-p
   #:outcome-target
   #:outcome-message
   #:outcome-host
   #:outcome-response
   #:outcome-suggested-name
   ;; the application
   #:main))
