;;;; about.lisp -- the browser's own pages: about:start and about:bookmarks.
;;;;
;;;; Written as gemtext and parsed by the library, so they look like any
;;;; other page and their links are ordinary links.

(in-package #:gopher-browser)

(defparameter +start-places+
  '(("Gopher"
     ("gopher://gopher.floodgap.com/" "Floodgap Systems")
     ("gopher://gopher.floodgap.com/7/v2/vs" "Veronica-2: search Gopherspace")
     ("gopher://gopher.quux.org/" "Quux.org")
     ("gopher://sdf.org/" "SDF Public Access UNIX System"))
    ("Gemini"
     ("gemini://geminiprotocol.net/" "Project Gemini")
     ("gemini://kennedy.gemi.dev/" "Kennedy: search Geminispace")
     ("gemini://tlgs.one/" "TLGS: another Gemini search engine"))
    ("Elsewhere"
     ("spartan://mozz.us/" "Spartan, by mozz.us")
     ("nex://nightfall.city/" "Nightfall City, on Nex")))
  "What the start page offers, under headings.")

(defun about-url-p (url)
  (let ((string (string-downcase (string-trim " " url))))
    (and (> (length string) 6) (string= "about:" string :end2 6))))

(defun start-page-gemtext ()
  (with-output-to-string (out)
    (format out "# Gopher~%~%")
    (format out "A browser for Gopher and the small web around it: Gemini, Spartan, Nex, Guppy and Finger.~%~%")
    (format out "Type an address above, or start here.~%")
    (loop for (heading . places) in +start-places+
          do (format out "~%## ~a~%" heading)
             (loop for (url label) in places
                   do (format out "=> ~a ~a~%" url label)))
    ;; The bookmarks, but not the places just listed again.
    (let* ((places (loop for (nil . places) in +start-places+ append (mapcar #'first places)))
           (bookmarks (remove-if (lambda (b) (member (bookmark-url b) places :test #'string=))
                                 (bookmarks))))
      (when bookmarks
        (format out "~%## Bookmarks~%")
        (dolist (b bookmarks)
          (format out "=> ~a ~a~%" (bookmark-url b) (bookmark-title b)))))))

(defun bookmarks-gemtext ()
  (with-output-to-string (out)
    (format out "# Bookmarks~%~%")
    (let ((bookmarks (bookmarks)))
      (if bookmarks
          (dolist (b bookmarks)
            (format out "=> ~a ~a~%" (bookmark-url b) (bookmark-title b)))
          (format out "Nothing is bookmarked yet.  Bookmarks › Add Bookmark (⌘D) adds the page you are on.~%")))))

(defun about-page (url &optional query)
  "The page for about: URL, or NIL if there is no such page.  A query is
after a ? in URL, or QUERY: about:history?lisp is the history searched for
lisp."
  (let* ((rest (subseq (string-trim " " url) 6))
         (mark (position #\? rest))
         (name (string-downcase (subseq rest 0 mark)))
         (query (or query (and mark (g:percent-decode (subseq rest (1+ mark))))))
         (gemtext (cond ((string= name "start") (start-page-gemtext))
                        ((string= name "bookmarks") (bookmarks-gemtext))
                        ((string= name "history") (history-gemtext (get-universal-time) query))
                        ((string= name "blank") ""))))
    (when gemtext
      (let ((page (document-page (g:parse-gemtext gemtext)
                                 (if (plusp (length (or query "")))
                                     (format nil "about:~a?~a" name (g:percent-encode query))
                                     (format nil "about:~a" name)))))
        (unless (page-title page)
          (setf (page-title page) "Blank"))
        page))))
