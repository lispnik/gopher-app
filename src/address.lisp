;;;; address.lisp -- what was typed into the address bar, and finishing it.

(in-package #:gopher-browser)

(defun search-url (query)
  "The URL that searches for QUERY with the chosen search engine."
  (format nil (third (assoc (search-engine) +search-engines+))
          (g:percent-encode query)))

(defun address-input-url (text)
  "The URL to go to for TEXT, as typed into the address bar: an address as it
is (the library adds gopher:// to one without a scheme), and words searched
for.  Words are anything with a space in it, or one word with no dot, colon
or slash that is not localhost."
  (let ((text (string-trim '(#\Space #\Tab) text)))
    (cond ((string= text "") nil)
          ((url-scheme-of text) text)
          ((find #\Space text) (search-url text))
          ((or (find #\. text) (find #\: text) (find #\/ text)
               (string-equal text "localhost"))
           text)
          (t (search-url text)))))

(defun without-scheme (url)
  "URL after its scheme's \"://\", or NIL if it has none."
  (let ((mark (search "://" url)))
    (and mark (subseq url (+ mark 3)))))

(defun address-candidates ()
  "What the address bar may complete to, best first: the history, newest
first, then the bookmarks, then the start page's places."
  (remove-duplicates
   (append (mapcar #'visit-url (visits))
           (mapcar #'bookmark-url (bookmarks))
           (loop for (nil . places) in +start-places+
                 append (mapcar #'first places)))
   :test #'string= :from-end t))

(defun completion-starts (url)
  "Where in URL a typed address may begin: its start, after its scheme, and
after each dot or slash -- so \"floodgap\" finds gopher.floodgap.com and
\"docs\" finds gemini://example.org/docs/."
  (let ((starts (list 0))
        (bare (search "://" url)))
    (when bare (push (+ bare 3) starts))
    (loop for i from (if bare (+ bare 3) 0) below (length url)
          when (and (find (char url i) "./") (< (1+ i) (length url)))
            do (push (1+ i) starts))
    (sort (remove-duplicates starts) #'<)))

(defun complete-address (typed)
  "The completion of TYPED, part of an address: (values TEXT URL), TEXT being
what the field should then hold (it begins with TYPED, in TYPED's case) and
URL where TEXT leads.  A candidate matches where any part of it begins --
the whole, after the scheme, after a dot or a slash -- so \"flood\" completes
to \"floodgap.com/\" and leads to gopher://gopher.floodgap.com/, if that was
visited.  An earlier match, and a newer candidate, win.  NIL when nothing
matches or TYPED is empty."
  (when (plusp (length typed))
    (let ((best nil))
      (dolist (url (address-candidates))
        (dolist (start (completion-starts url))
          (let ((end (+ start (length typed))))
            (when (and (< end (length url))
                       (string-equal typed url :start2 start :end2 end)
                       (or (null best) (< start (second best))))
              (setf best (list url start))
              (return)))))
      (when best
        (destructuring-bind (url start) best
          (values (concatenate 'string typed (subseq url (+ start (length typed)))) url))))))
