;;;; page.lisp -- what a window shows: styled runs of text, and the links in them.
;;;;
;;;; A PAGE is the library's DOCUMENT made ready to draw.  Each document line
;;;; becomes one or more RUNs, a string and a style keyword, and every link a
;;;; LINK the runs point at by index.  The window turns the runs into an
;;;; attributed string; nothing here knows about AppKit, so the decisions about
;;;; what a line looks like are tested without a window.
;;;;
;;;; Styles:
;;;;   :text        body text                  :pre       monospaced, as sent
;;;;   :heading-1   :heading-2   :heading-3    :list      a bulleted item
;;;;   :quote       a quotation                :link      a link's label
;;;;   :menu-link   a Gopher menu link's label, monospaced like the rest
;;;;   :tag         a Gopher item's type, before its label
;;;;   :error       a Gopher error item, or a failure the browser reports
;;;;   :notice      the browser's own remarks (a download, an empty page)

(in-package #:gopher-browser)

(defstruct (link (:constructor make-link (url &key label (kind :follow) item-kind plus)))
  "Where a link goes.  KIND is :FOLLOW, or :INPUT for a link that asks for
text before it is followed (Spartan's =: lines).  PLUS is NIL, or :PLUS for a
Gopher+ item, or :ASK for a Gopher+ item with a form to fill in first."
  (url "" :type string)
  (label "" :type string)
  (kind :follow :type keyword)
  (item-kind nil)
  (plus nil))

(defstruct (run (:constructor make-run (text style &optional link)))
  "TEXT drawn in STYLE.  LINK is an index into the page's links, or NIL."
  (text "" :type string)
  (style :text :type keyword)
  (link nil))

(defstruct (page (:constructor %make-page))
  (url "" :type string)
  (title nil)
  (runs (make-array 0 :adjustable t :fill-pointer 0))
  (links (make-array 0 :adjustable t :fill-pointer 0))
  (line-count 0)
  (image nil)          ; the octets of an image, for an image page
  (mime-type nil))

(defun make-page (url &key title image mime-type)
  (%make-page :url url :title title :image image :mime-type mime-type))

;;; Gopher item types -------------------------------------------------------------

(defparameter +gopher-tags+
  '((:menu . "DIR") (:text . "TXT") (:search . "ASK") (:binary . "BIN")
    (:image . "IMG") (:sound . "SND") (:video . "VID") (:document . "DOC")
    (:html . "WWW") (:telnet . "TEL") (:mirror . "DIR") (:cso . "CSO")
    (:calendar . "CAL"))
  "The tag drawn before each Gopher menu link, by item kind.  All three
letters, so that the labels after them line up.")

(defparameter +tag-width+ 5
  "Columns a tag and its spacing take: info lines in a menu are indented this
far, so that they line up with the labels.")

(defun gopher-tag (item-kind)
  (or (cdr (assoc item-kind +gopher-tags+)) "???"))

;;; Gopher says what an item is before it is fetched: these are files to save,
;;; never pages, so the browser asks where first and streams them to disk.
(defparameter +download-item-kinds+ '(:binary :sound :video :document))

(defun download-link-p (link)
  "Whether following LINK means saving a file."
  (and (member (link-item-kind link) +download-item-kinds+) t))

;;; Lines to runs --------------------------------------------------------------------

(defun add-link (page link)
  "Add LINK to PAGE and answer its index."
  (vector-push-extend link (page-links page))
  (1- (length (page-links page))))

(defun menu-line-p (line)
  "Whether LINE came from a Gopher menu: menu lines carry an item type."
  (and (g:line-item-type line) t))

(defun line-runs (page line)
  "The runs for LINE, ending in a newline, with any link added to PAGE."
  (let ((text (g:line-text line))
        (menu (menu-line-p line)))
    (flet ((link-run (kind label &optional (style :link))
             (let ((index (add-link page (make-link (or (g:line-url line) "")
                                                    :label label :kind kind
                                                    :item-kind (g:line-item-kind line)
                                                    :plus (cond ((g:line-gopher-plus-ask-p line) :ask)
                                                                ((g:line-gopher-plus-p line) :plus))))))
               (make-run label style index))))
      (ecase (g:line-kind line)
        (:text
         (if menu
             (list (make-run (format nil "~v@a~a~%" +tag-width+ "" text) :pre))
             (list (make-run (format nil "~a~%" text) :text))))
        (:preformatted
         (list (make-run (format nil "~a~%" text) :pre)))
        (:heading
         (list (make-run (format nil "~a~%" text)
                         (case (g:line-level line)
                           (1 :heading-1) (2 :heading-2) (t :heading-3)))))
        (:list-item
         (list (make-run (format nil "•  ~a~%" text) :list)))
        (:quote
         (list (make-run (format nil "~a~%" text) :quote)))
        (:error
         (list (make-run (format nil "~:[~;~v@a~]~a~%" menu +tag-width+ "" text) :error)))
        (:input-prompt
         (list (make-run (format nil "~va" +tag-width+ "?") :tag)
               (link-run :input (format nil "~a" (link-label-text text (g:line-url line))))
               (make-run (string #\Newline) :text)))
        (:link
         (if menu
             (list (make-run (format nil "~va" +tag-width+
                                     ;; DIR+ for a Gopher+ item, TXT? for a form.
                                     (format nil "~a~a" (gopher-tag (g:line-item-kind line))
                                             (cond ((g:line-gopher-plus-ask-p line) "?")
                                                   ((g:line-gopher-plus-p line) "+")
                                                   (t ""))))
                             :tag)
                   (link-run :follow (link-label-text text (g:line-url line)) :menu-link)
                   (make-run (string #\Newline) :pre))
             (list (link-run :follow (link-label-text text (g:line-url line)))
                   (make-run (string #\Newline) :text))))))))

(defun link-label-text (text url)
  "What a link shows: its label, or its URL when it has none."
  (if (plusp (length (string-trim " " text))) text (or url "")))

(defun page-add-lines (page lines)
  "Add LINES (a sequence of DOCUMENT-LINEs) to PAGE.  Answers the new runs, in
order: what a window that is already showing PAGE must append."
  (let ((new '()))
    (map nil (lambda (line)
               (incf (page-line-count page))
               (unless (page-title page)
                 (when (eq (g:line-kind line) :heading)
                   (setf (page-title page) (g:line-text line))))
               (dolist (run (line-runs page line))
                 (vector-push-extend run (page-runs page))
                 (push run new)))
         lines)
    (nreverse new)))

(defun url-title (url)
  "A title for a page with nothing better: the host and path, as typed."
  (handler-case (let ((parsed (g:parse-url url)))
                  (format nil "~a~a" (g:url-display-host parsed)
                          (let ((path (or (g:url-path parsed) "")))
                            (if (member path '("" "/") :test #'string=) "" path))))
    (error () url)))

(defun document-page (document url)
  "A page of DOCUMENT, fetched from URL."
  (let ((page (make-page url :title (g:document-title document))))
    (page-add-lines page (g:document-lines document))
    page))

(defun page-display-title (page)
  (or (page-title page) (url-title (page-url page))))

(defun page-text (page)
  "PAGE's text, without styles: what Copy of the whole page gives."
  (with-output-to-string (out)
    (loop for run across (page-runs page) do (write-string (run-text run) out))))

(defun message-page (url title &rest paragraphs)
  "A page the browser writes itself: TITLE as a heading, then PARAGRAPHS.  A
paragraph is a string, or (:ERROR string), (:PRE string), (:NOTICE string)
or (:LINK url label)."
  (let ((page (make-page url :title title)))
    (flet ((add (text style &optional link)
             (vector-push-extend (make-run text style link) (page-runs page))))
      (add (format nil "~a~%" title) :heading-1)
      (dolist (paragraph paragraphs)
        (etypecase paragraph
          (string (add (format nil "~a~%" paragraph) :text))
          (cons
           (destructuring-bind (kind &rest arguments) paragraph
             (ecase kind
               ((:error :pre :notice :text)
                (add (format nil "~a~%" (first arguments)) kind))
               (:link
                (destructuring-bind (target &optional (label target)) arguments
                  (add label :link (add-link page (make-link target :label label)))
                  (add (string #\Newline) :text)))))))))
    page))
