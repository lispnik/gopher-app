;;;; settings-tests.lisp -- the home page, bookmarks, and the about: pages.

(in-package #:gopher-browser/tests)

(def-suite settings :in all-tests :description "What the browser remembers.")
(in-suite settings)

(test the-data-directory-is-the-one-the-environment-names
  (with-data-directory (directory)
    (is (string= (namestring (uiop:ensure-directory-pathname directory))
                 (namestring (gb:data-directory))))))

(test the-home-page-is-the-start-page-until-set
  (with-data-directory ()
    (is (string= "about:start" (gb:home-page)))
    (gb:set-home-page "gopher://example.org/")
    (is (string= "gopher://example.org/" (gb:home-page)))))

(test bookmarks-are-kept-in-order-and-survive
  (with-data-directory ()
    (is (null (gb:bookmarks)))
    (gb:add-bookmark "One" "gopher://one/")
    (gb:add-bookmark "Two" "gemini://two/")
    (is (equal '("One" "Two") (mapcar #'gb:bookmark-title (gb:load-bookmarks))))
    ;; The same URL again takes the new title and keeps its place.
    (gb:add-bookmark "Uno" "gopher://one/")
    (is (equal '(("Uno" . "gopher://one/") ("Two" . "gemini://two/"))
               (mapcar (lambda (b) (cons (gb:bookmark-title b) (gb:bookmark-url b)))
                       (gb:load-bookmarks))))
    (gb:remove-bookmark "gopher://one/")
    (is (equal '("gemini://two/") (mapcar #'gb:bookmark-url (gb:load-bookmarks))))))

(test a-bookmark-with-no-title-is-titled-by-its-address
  (with-data-directory ()
    (gb:add-bookmark "  " "gopher://one/")
    (is (string= "gopher://one/" (gb:bookmark-title (first (gb:bookmarks)))))))

(test a-damaged-bookmarks-file-is-no-bookmarks
  (with-data-directory ()
    (with-open-file (out (gb::data-file "bookmarks.sexp") :direction :output)
      (write-string "((\"unterminated" out))
    (is (null (gb:bookmarks)))
    ;; And it is never evaluated.
    (with-open-file (out (gb::data-file "bookmarks.sexp") :direction :output
                                                          :if-exists :supersede)
      (write-string "#.(error \"evaluated\")" out))
    (is (null (gb:bookmarks)))))

(test the-start-page-lists-places-and-bookmarks
  (with-data-directory ()
    (gb:add-bookmark "My hole" "gopher://my.hole/")
    (let* ((page (gb::about-page "about:start"))
           (urls (map 'list #'gb:link-url (gb:page-links page))))
      (is (member "gopher://gopher.floodgap.com/" urls :test #'string=))
      (is (member "gemini://geminiprotocol.net/" urls :test #'string=))
      (is (member "gopher://my.hole/" urls :test #'string=))
      (is (string= "Gopher" (gb:page-title page))))))

(test the-bookmarks-page
  (with-data-directory ()
    (is (search "Nothing is bookmarked" (gb:page-text (gb::about-page "about:bookmarks"))))
    (gb:add-bookmark "Floodgap" "gopher://gopher.floodgap.com/")
    (let ((page (gb::about-page "about:bookmarks")))
      (is (= 1 (length (gb:page-links page))))
      (is (search "Floodgap" (gb:page-text page))))))

(test unknown-about-pages
  (is (null (gb::about-page "about:nothing-here")))
  (is (gb::about-url-p "About:Start"))
  (is (not (gb::about-url-p "gopher://about/"))))

(test a-new-browser-starts-with-suggested-bookmarks
  (with-data-directory ()
    (setf gb::*suggest-bookmarks* t)
    (let ((urls (mapcar #'gb:bookmark-url (gb:bookmarks))))
      (is (= (length gb::+suggested-bookmarks+) (length urls)))
      (is (member "gopher://gopher.floodgap.com/" urls :test #'string=))
      (is (member "gemini://geminiprotocol.net/" urls :test #'string=)))
    ;; Delete them all: they do not come back by themselves.
    (dotimes (i (length gb::+suggested-bookmarks+)) (gb::delete-bookmark 0))
    (is (null (gb:bookmarks)))
    ;; Asked for, they do, after what is there, and only the missing ones.
    (gb:add-bookmark "Mine" "gopher://mine/")
    (gb:add-bookmark "Floodgap, as I call it" "gopher://gopher.floodgap.com/")
    (is (= (1- (length gb::+suggested-bookmarks+)) (gb::add-suggested-bookmarks)))
    (is (zerop (gb::add-suggested-bookmarks)))
    (let ((bookmarks (gb:bookmarks)))
      (is (string= "Mine" (gb:bookmark-title (first bookmarks))))
      (is (string= "Floodgap, as I call it" (gb:bookmark-title (second bookmarks))))
      (is (= (1+ (length gb::+suggested-bookmarks+)) (length bookmarks))))))

(test suggested-bookmarks-are-well-formed
  (let ((urls (mapcar #'cdr gb::+suggested-bookmarks+)))
    (is (= (length urls) (length (remove-duplicates urls :test #'string=))))
    (dolist (pair gb::+suggested-bookmarks+)
      (is (plusp (length (car pair))))
      (is (g:parse-url (cdr pair)) "~a parses" (cdr pair))
      (is (member (g:url-scheme (g:parse-url (cdr pair))) (g:supported-schemes)
                  :test #'string-equal)))))

(test the-start-page-does-not-list-a-place-twice
  (with-data-directory ()
    (setf gb::*suggest-bookmarks* t)
    (let* ((urls (map 'list #'gb:link-url (gb:page-links (gb::about-page "about:start")))))
      (is (= (length urls) (length (remove-duplicates urls :test #'string=))))
      (is (member "gopher://gopherpedia.com/" urls :test #'string=)))))
