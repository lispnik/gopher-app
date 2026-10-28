;;;; bookmarks.lisp -- a list of titled URLs, kept in bookmarks.sexp.

(in-package #:gopher-browser)

(defstruct (bookmark (:constructor make-bookmark (title url)))
  (title "" :type string)
  (url "" :type string))

;;; Suggested bookmarks: the places worth knowing in Gopherspace and Geminispace,
;;; and one each on Spartan, Nex and Finger.  Every one answered when the list
;;; was made (2026-10-28); sites that did not (geminispace.info, Antenna,
;;; gemlog.blue, grex.org and others) were left out.

(defparameter +suggested-bookmarks+
  '(;; Gopher
    ("Floodgap Systems" . "gopher://gopher.floodgap.com/")
    ("Veronica-2: search Gopherspace" . "gopher://gopher.floodgap.com/7/v2/vs")
    ("New Gopher servers (Floodgap)" . "gopher://gopher.floodgap.com/1/new")
    ("Every known Gopher server (Floodgap)" . "gopher://gopher.floodgap.com/1/world")
    ("Quux.org" . "gopher://gopher.quux.org/")
    ("SDF Public Access UNIX System" . "gopher://sdf.org/")
    ("Bitreich" . "gopher://bitreich.org/")
    ("Gopherpedia: Wikipedia over Gopher" . "gopher://gopherpedia.com/")
    ("Hacker News over Gopher" . "gopher://hngopher.com/")
    ("MetaFilter" . "gopher://gopher.metafilter.com/")
    ("Cosmic Voyage: collaborative science fiction" . "gopher://cosmic.voyage/")
    ("tilde.club" . "gopher://tilde.club/")
    ("Mare Tranquillitatis People's Circumlunar Zaibatsu" . "gopher://zaibatsu.circumlunar.space/")
    ;; Gemini
    ("Project Gemini" . "gemini://geminiprotocol.net/")
    ("Gemini documentation" . "gemini://geminiprotocol.net/docs/")
    ("Kennedy: search Geminispace" . "gemini://kennedy.gemi.dev/")
    ("TLGS: search Geminispace" . "gemini://tlgs.one/")
    ("Collaborative Directory of Geminispace" . "gemini://cdg.thegonz.net/")
    ("BBS: discussions on Gemini" . "gemini://bbs.geminispace.org/")
    ("Station: a Gemini social network" . "gemini://station.martinrue.com/")
    ("Midnight Pub" . "gemini://midnight.pub/")
    ("smol.pub: journals on the small net" . "gemini://smol.pub/")
    ("Circumlunar Gemini Outpost" . "gemini://gemini.circumlunar.space/")
    ("Lagrange, a Gemini browser" . "gemini://skyjake.fi/lagrange/")
    ("Ploum.net" . "gemini://ploum.net/")
    ;; The rest of the small web
    ("Spartan, by mozz.us" . "spartan://mozz.us/")
    ("Nightfall City, on Nex" . "nex://nightfall.city/")
    ("Happy Net Box, on Finger" . "finger://happynetbox.com/"))
  "The bookmarks a new browser starts with: (title . url), in menu order.")

(defvar *suggest-bookmarks* t
  "Whether a browser with no bookmarks file starts with +SUGGESTED-BOOKMARKS+.
The tests turn it off, to start from nothing.")

(defun load-bookmarks ()
  "The saved bookmarks, oldest first.  An entry that does not read as a title
and a URL is dropped.  With no bookmarks file at all -- a browser's first
run -- the suggested ones are saved and answered."
  (when (and *suggest-bookmarks* (not (probe-file (data-file "bookmarks.sexp"))))
    (write-data-file "bookmarks.sexp" +suggested-bookmarks+))
  (let ((form (read-data-file "bookmarks.sexp")))
    (loop for entry in (if (listp form) form '())
          when (and (consp entry) (stringp (car entry)) (stringp (cdr entry)))
            collect (make-bookmark (car entry) (cdr entry)))))

(defun save-bookmarks (bookmarks)
  (write-data-file "bookmarks.sexp"
                   (mapcar (lambda (b) (cons (bookmark-title b) (bookmark-url b)))
                           bookmarks))
  bookmarks)

(defun bookmarks ()
  "The bookmarks, oldest first."
  (load-bookmarks))

(defun add-bookmark (title url)
  "Bookmark URL as TITLE.  A URL already bookmarked takes the new title and
keeps its place.  Answers the bookmarks."
  (let* ((title (if (plusp (length (string-trim " " title))) title url))
         (bookmarks (load-bookmarks))
         (existing (find url bookmarks :key #'bookmark-url :test #'string=)))
    (if existing
        (setf (bookmark-title existing) title)
        (setf bookmarks (append bookmarks (list (make-bookmark title url)))))
    (save-bookmarks bookmarks)))

(defun remove-bookmark (url)
  "Forget the bookmark for URL.  Answers the bookmarks."
  (save-bookmarks (remove url (load-bookmarks) :key #'bookmark-url :test #'string=)))

;;; Editing, by position -----------------------------------------------------------
;;;
;;; The Edit Bookmarks window works by row, and a row is a position in the list.

(defun update-bookmark (index &key title url)
  "Change the title or the URL of the bookmark at INDEX.  An empty title or URL
is refused, and so is a URL another bookmark already has.  Answers true if it
changed."
  (let* ((bookmarks (load-bookmarks))
         (bookmark (nth index bookmarks)))
    (when (and bookmark
               (or (null title) (plusp (length (string-trim " " title))))
               (or (null url)
                   (and (plusp (length (string-trim " " url)))
                        (not (find-if (lambda (other)
                                        (and (not (eq other bookmark))
                                             (string= (bookmark-url other) url)))
                                      bookmarks)))))
      (when title (setf (bookmark-title bookmark) (string-trim " " title)))
      (when url (setf (bookmark-url bookmark) (string-trim " " url)))
      (save-bookmarks bookmarks)
      t)))

(defun move-bookmark (index delta)
  "Move the bookmark at INDEX DELTA places (-1 up, 1 down).  Answers its new
index, or INDEX if it could go no further."
  (let* ((bookmarks (load-bookmarks))
         (to (+ index delta)))
    (if (and (< -1 index (length bookmarks)) (< -1 to (length bookmarks)))
        (let* ((moving (nth index bookmarks))
               (rest (remove moving bookmarks)))
          (save-bookmarks (append (subseq rest 0 to) (list moving) (subseq rest to)))
          to)
        index)))

(defun delete-bookmark (index)
  "Delete the bookmark at INDEX.  Answers the bookmarks."
  (let ((bookmarks (load-bookmarks)))
    (if (< -1 index (length bookmarks))
        (save-bookmarks (append (subseq bookmarks 0 index) (subseq bookmarks (1+ index))))
        bookmarks)))

(defun add-suggested-bookmarks ()
  "Add every suggested bookmark not already bookmarked, after the others.
Answers how many were added."
  (let* ((bookmarks (load-bookmarks))
         (missing (remove-if (lambda (pair)
                               (find (cdr pair) bookmarks :key #'bookmark-url :test #'string=))
                             +suggested-bookmarks+)))
    (save-bookmarks (append bookmarks (mapcar (lambda (pair) (make-bookmark (car pair) (cdr pair)))
                                              missing)))
    (length missing)))
