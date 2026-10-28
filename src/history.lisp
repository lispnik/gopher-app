;;;; history.lisp -- every page visited, across windows and launches.
;;;;
;;;; This is the History menu's history, not Back and Forward's: those are a
;;;; window's own (SESSION-HISTORY).  Kept newest first in history.sexp, one
;;;; entry per URL, at most +HISTORY-LIMIT+ of them.

(in-package #:gopher-browser)

(defparameter +history-limit+ 1000)

(defstruct (visit (:constructor make-visit (url title time)))
  (url "" :type string)
  (title "" :type string)
  (time 0 :type integer))

(defvar *visits* :unloaded
  "The visits, newest first, once read.  Main thread only.")

(defun visits ()
  "Every visit remembered, newest first."
  (when (eq *visits* :unloaded)
    (let ((form (read-data-file "history.sexp")))
      (setf *visits*
            (loop for entry in (if (listp form) form '())
                  when (and (listp entry) (= 3 (length entry))
                            (stringp (first entry)) (stringp (second entry))
                            (integerp (third entry)))
                    collect (apply #'make-visit entry)))))
  *visits*)

(defun forget-loaded-history ()
  "Read history.sexp again next time (the tests change the data directory)."
  (setf *visits* :unloaded))

(defun save-visits ()
  (write-data-file "history.sexp"
                   (mapcar (lambda (v) (list (visit-url v) (visit-title v) (visit-time v)))
                           (visits))))

(defun record-visit (url title &optional (time (get-universal-time)))
  "Remember a visit to URL, titled TITLE, as the newest.  about: pages are
not remembered."
  (unless (about-url-p url)
    (setf *visits*
          (let ((rest (remove url (visits) :key #'visit-url :test #'string=)))
            (cons (make-visit url (or title url) time)
                  (if (>= (length rest) +history-limit+)
                      (subseq rest 0 (1- +history-limit+))
                      rest))))
    (save-visits))
  url)

(defun clear-visits ()
  (setf *visits* '())
  (save-visits))

(defun recent-visits (count)
  (let ((visits (visits)))
    (subseq visits 0 (min count (length visits)))))

(defun day-heading (time now)
  "Today, Yesterday, or the date of TIME; \"Earlier\" for a time that will
not decode."
  (handler-case (%day-heading time now)
    (error () "Earlier")))

(defun %day-heading (time now)
  (multiple-value-bind (s m h day month year) (decode-universal-time time)
    (declare (ignore s m h))
    (flet ((day-number (time)
             (multiple-value-bind (s m h d mo y) (decode-universal-time time)
               (declare (ignore s m h))
               (floor (encode-universal-time 0 0 12 d mo y) 86400))))
      (case (- (day-number now) (day-number time))
        (0 "Today")
        (1 "Yesterday")
        (t (format nil "~d ~a ~d" day
                   (nth (1- month) '("January" "February" "March" "April" "May" "June" "July"
                                     "August" "September" "October" "November" "December"))
                   year))))))

(defun visit-matches-p (visit words)
  "Whether every one of WORDS is in VISIT's title or address, in any case."
  (every (lambda (word)
           (or (search word (visit-title visit) :test #'char-equal)
               (search word (visit-url visit) :test #'char-equal)))
         words))

(defun search-words (query)
  (remove "" (uiop:split-string (or query "") :separator '(#\Space #\Tab)) :test #'string=))

(defun history-gemtext (&optional (now (get-universal-time)) query)
  "The history page; with QUERY, only the visits that have all its words."
  (with-output-to-string (out)
    (format out "# History~%")
    ;; A Spartan-style prompt: following it asks for words, and searches.
    (format out "=: about:history Search the history~%")
    (let* ((words (search-words query))
           (visits (if words
                       (remove-if-not (lambda (v) (visit-matches-p v words)) (visits))
                       (visits)))
           (heading nil))
      (when words
        (format out "~%~d page~:p with ~{“~a”~^ and ~}.~%" (length visits) words)
        (format out "=> about:history Show all of the history~%"))
      (if (null visits)
          (format out "~%~:[Nothing has been visited yet.~;Nothing visited matches.~]~%" words)
          (dolist (visit visits)
            (let ((this (day-heading (visit-time visit) now)))
              (unless (equal this heading)
                (setf heading this)
                (format out "~%## ~a~%" heading)))
            (format out "=> ~a ~a~%" (visit-url visit) (visit-title visit)))))))
