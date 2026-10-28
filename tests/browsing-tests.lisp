;;;; browsing-tests.lisp -- history, the address bar, bookmark editing,
;;;; downloads and text size, without a window.

(in-package #:gopher-browser/tests)

(def-suite browsing :in all-tests :description "What the browser remembers and works out.")
(in-suite browsing)

;;; History -------------------------------------------------------------------------------

(test visits-are-newest-first-once-each-and-survive
  (with-data-directory ()
    (gb::record-visit "gopher://a/" "A" 100)
    (gb::record-visit "gopher://b/" "B" 200)
    (gb::record-visit "gopher://a/" "A again" 300)
    (is (equal '("gopher://a/" "gopher://b/") (mapcar #'gb::visit-url (gb::visits))))
    (is (string= "A again" (gb::visit-title (first (gb::visits)))))
    ;; Read back from the file.
    (gb::forget-loaded-history)
    (is (equal '("gopher://a/" "gopher://b/") (mapcar #'gb::visit-url (gb::visits))))
    (gb::clear-visits)
    (gb::forget-loaded-history)
    (is (null (gb::visits)))))

(test about-pages-are-not-history
  (with-data-directory ()
    (gb::record-visit "about:start" "Gopher")
    (is (null (gb::visits)))))

(test history-is-limited
  (with-data-directory ()
    (let ((gb::+history-limit+ 5))
      (dotimes (i 8) (gb::record-visit (format nil "gopher://h/~d" i) "x" i))
      (is (= 5 (length (gb::visits))))
      (is (string= "gopher://h/7" (gb::visit-url (first (gb::visits))))))))

(test the-history-page-groups-by-day
  (with-data-directory ()
    (let ((now (encode-universal-time 0 0 12 28 10 2026)))
      (gb::record-visit "gopher://old/" "Old" (encode-universal-time 0 0 9 20 10 2026))
      (gb::record-visit "gopher://yesterday/" "Yesterday's" (- now 86400))
      (gb::record-visit "gopher://today/" "Today's" now)
      (let ((text (gb::history-gemtext now)))
        (is (search "## Today" text))
        (is (search "## Yesterday" text))
        (is (search "## 20 October 2026" text))
        (is (< (search "Today's" text) (search "Yesterday's" text) (search "Old" text)))))))

;;; The address bar ---------------------------------------------------------------------------

(test what-the-address-bar-does-with-text
  (with-data-directory ()
    (is (string= "gemini://example.org/" (gb::address-input-url "gemini://example.org/")))
    (is (string= "gopher.floodgap.com" (gb::address-input-url "  gopher.floodgap.com ")))
    (is (string= "localhost:7070" (gb::address-input-url "localhost:7070")))
    (is (string= "localhost" (gb::address-input-url "localhost")))
    (is (string= "about:history" (gb::address-input-url "about:history")))
    (is (string= "gemini://kennedy.gemi.dev/search?common%20lisp"
                 (gb::address-input-url "common lisp")))
    (is (string= "gemini://kennedy.gemi.dev/search?lisp" (gb::address-input-url "lisp")))
    (is (null (gb::address-input-url "   ")))
    (gb::set-search-engine :veronica)
    (is (string= "gopher://gopher.floodgap.com/7/v2/vs%09lisp" (gb::address-input-url "lisp")))))

(test addresses-complete-from-history-then-bookmarks
  (with-data-directory ()
    (gb:add-bookmark "Mine" "gemini://my.example/")
    (gb::record-visit "gopher://gopher.floodgap.com/1/world" "World" 100)
    ;; With the scheme.
    (multiple-value-bind (text url) (gb::complete-address "gopher://gopher.fl")
      (is (string= "gopher://gopher.floodgap.com/1/world" text))
      (is (string= "gopher://gopher.floodgap.com/1/world" url)))
    ;; Without it: the field shows what was typed, finished, and it leads to the URL.
    (multiple-value-bind (text url) (gb::complete-address "my.ex")
      (is (string= "my.example/" text))
      (is (string= "gemini://my.example/" url)))
    ;; In the case it was typed in.
    (is (string= "GOPHER.floodgap.com/1/world" (gb::complete-address "GOPHER.")))
    ;; The start page's places too.
    (is (string= "geminiprotocol.net/" (gb::complete-address "geminip")))
    (is (null (gb::complete-address "nothing-like-this")))
    (is (null (gb::complete-address "")))
    ;; Already whole: nothing to add.
    (is (null (gb::complete-address "gemini://my.example/")))))

;;; Bookmarks, by position -----------------------------------------------------------------------

(test bookmarks-are-edited-by-position
  (with-data-directory ()
    (gb:add-bookmark "One" "gopher://one/")
    (gb:add-bookmark "Two" "gopher://two/")
    (gb:add-bookmark "Three" "gopher://three/")
    (is (gb::update-bookmark 0 :title "Uno"))
    (is (gb::update-bookmark 1 :url "gemini://two/"))
    ;; Refused: empty, or a URL another bookmark has.
    (is (not (gb::update-bookmark 0 :title "  ")))
    (is (not (gb::update-bookmark 0 :url "gopher://three/")))
    (is (= 2 (gb::move-bookmark 0 2)) "a move of two is a move of two")
    (is (equal '("Two" "Three" "Uno") (mapcar #'gb:bookmark-title (gb:bookmarks))))
    (is (= 0 (gb::move-bookmark 0 -1)) "the top cannot go up")
    (gb::delete-bookmark 1)
    (is (equal '(("Two" . "gemini://two/") ("Uno" . "gopher://one/"))
               (mapcar (lambda (b) (cons (gb:bookmark-title b) (gb:bookmark-url b)))
                       (gb:bookmarks))))))

;;; Downloads ----------------------------------------------------------------------------------

(test download-records
  (let ((gb::*downloads* '()))
    (let ((download (gb::make-download "gopher://h/9/a.zip" "/tmp/x/a.zip")))
      (is (string= "a.zip" (gb::download-name download)))
      (setf (gb::download-bytes download) 2048 (gb::download-total download) 4096)
      (is (string= "2.0 KB of 4.0 KB" (gb::download-status-text download)))
      (gb::download-finished download (gb::make-outcome :kind :saved :message "4096"))
      (is (string= "Done, 4.0 KB" (gb::download-status-text download)))
      (let ((failed (gb::make-download "gopher://h/9/b" "/tmp/b")))
        (gb::download-finished failed (gb::make-outcome :kind :error :message "No route"))
        (is (string= "Failed: No route" (gb::download-status-text failed))))
      (let ((running (gb::make-download "gopher://h/9/c" "/tmp/c")))
        (is (= 3 (length gb::*downloads*)))
        (gb::clear-finished-downloads)
        (is (equal (list running) gb::*downloads*))))))

;;; Text size ----------------------------------------------------------------------------------

(test text-size-steps-and-stops
  (with-data-directory ()
    (is (= 1 (gb::text-scale)))
    (is (= 1.1d0 (gb::next-text-scale 1)))
    (is (= 0.9d0 (gb::next-text-scale -1)))
    (gb::set-text-scale 2.5)
    (is (= 2.5d0 (gb::next-text-scale 1)) "the biggest stays")
    (gb::set-text-scale 0.7)
    (is (= 0.7d0 (gb::next-text-scale -1)) "the smallest stays")))

(test sessions-are-saved-and-read
  (with-data-directory ()
    (is (null (gb::saved-session)))
    (gb::save-session '((:frame "1 2 3 4" :urls ("gopher://a/" "gopher://b/") :selected 1)
                        (:urls ())         ; nothing in it: dropped
                        (:frame "x" :urls ("about:start"))))
    (is (equal '((:frame "1 2 3 4" :urls ("gopher://a/" "gopher://b/") :selected 1)
                 (:frame "x" :urls ("about:start")))
               (gb::saved-session)))))

;;; Swipes -----------------------------------------------------------------------------------

(defun swipe (tracker steps &key (inverted t))
  "Feed TRACKER a gesture: STEPS of (dx dy), begun, changed and ended."
  (let ((result nil))
    (loop for (dx dy) in steps
          for phase = gb::+phase-began+ then gb::+phase-changed+
          do (gb::track-scroll tracker phase 0 dx dy inverted))
    (setf result (gb::track-scroll tracker gb::+phase-ended+ 0 0 0 inverted))
    result))

(test two-finger-swipes
  (let ((tracker (gb::make-swipe-tracker)))
    ;; Natural scrolling: the deltas follow the fingers; right is back.
    (is (eq :back (swipe tracker '((20 0) (60 2) (60 -1)))))
    (is (eq :forward (swipe tracker '((-20 0) (-60 2) (-60 -1)))))
    ;; Without natural scrolling the deltas are the other way round.
    (is (eq :back (swipe tracker '((-20 0) (-60 2) (-60 -1)) :inverted nil)))
    ;; Too short, or mostly up and down: a scroll, not a swipe.
    (is (null (swipe tracker '((20 0) (30 0)))))
    (is (null (swipe tracker '((100 0) (60 200)))))
    ;; A cancelled gesture, momentum, and a wheel with no phase are not swipes.
    (gb::track-scroll tracker gb::+phase-began+ 0 200 0 t)
    (is (null (gb::track-scroll tracker gb::+phase-cancelled+ 0 0 0 t)))
    (is (null (gb::track-scroll tracker 0 4 500 0 t)))
    (is (null (gb::track-scroll tracker 0 0 500 0 t)))
    ;; An end with no beginning is nothing.
    (is (null (gb::track-scroll tracker gb::+phase-ended+ 0 500 0 t)))))

;;; Completion anywhere a part begins -----------------------------------------------------------

(test addresses-complete-from-any-part
  (with-data-directory ()
    (gb::record-visit "gopher://gopher.floodgap.com/1/world" "World" 100)
    (gb::record-visit "gemini://example.org/docs/guide.gmi" "Guide" 200)
    (multiple-value-bind (text url) (gb::complete-address "floodgap")
      (is (string= "floodgap.com/1/world" text))
      (is (string= "gopher://gopher.floodgap.com/1/world" url)))
    (multiple-value-bind (text url) (gb::complete-address "docs")
      (is (string= "docs/guide.gmi" text))
      (is (string= "gemini://example.org/docs/guide.gmi" url)))
    ;; Not from the middle of a word.
    (is (null (gb::complete-address "oodgap")))
    ;; The beginning wins over a later part.
    (gb::record-visit "gemini://world.example/" "Another world" 300)
    (multiple-value-bind (text url) (gb::complete-address "world")
      (is (string= "world.example/" text))
      (is (string= "gemini://world.example/" url)))))

;;; Searching the history ---------------------------------------------------------------------

(test the-history-can-be-searched
  (with-data-directory ()
    (gb::record-visit "gopher://gopher.floodgap.com/" "Floodgap Systems" 100)
    (gb::record-visit "gemini://geminiprotocol.net/" "Project Gemini" 200)
    (gb::record-visit "gemini://example.org/lisp" "Common Lisp notes" 300)
    (let* ((outcome (gb:session-fetch (gb:make-session) "about:history" :query "gemini"))
           (page (gb:outcome-page outcome))
           (urls (map 'list #'gb:link-url (gb:page-links page))))
      (is (string= "about:history?gemini" (gb:page-url page)))
      (is (member "gemini://geminiprotocol.net/" urls :test #'string=))
      (is (member "gemini://example.org/lisp" urls :test #'string=) "the address matches too")
      (is (not (member "gopher://gopher.floodgap.com/" urls :test #'string=)))
      (is (search "2 pages with “gemini”" (gb:page-text page))))
    ;; Every word must match; and the query can be in the address.
    (let ((page (gb::about-page "about:history?common%20notes")))
      (is (search "1 page with “common” and “notes”" (gb:page-text page))))
    ;; The page offers the search itself, as a prompt.
    (let ((prompt (find :input (gb:page-links (gb::about-page "about:history"))
                        :key #'gb:link-kind)))
      (is (not (null prompt)))
      (is (string= "about:history" (gb:link-url prompt))))))

;;; Gopher+ form replies -----------------------------------------------------------------------

(test form-replies
  (let* ((questions (list (g::make-gopher-plus-question :kind :ask :prompt "Name")
                          (g::make-gopher-plus-question :kind :note :prompt "Hello")
                          (g::make-gopher-plus-question :kind :ask-long :prompt "Lines")
                          (g::make-gopher-plus-question :kind :choose-file :prompt "Send")
                          (g::make-gopher-plus-question :kind :ask-file :prompt "Keep")))
         (reply (gb::form-reply questions
                                (list "Ada" nil (format nil "one~%two~%")
                                      "/Users/someone/private/notes.txt"
                                      "/Users/someone/Downloads/reply.txt"))))
    ;; The note has no answer; the paths are sent as names only.
    (is (equal '("Ada" ("one" "two") "notes.txt" "reply.txt") (gb::form-reply-answers reply)))
    (is (string= "/Users/someone/private/notes.txt" (gb::form-reply-file reply)))
    (is (string= "/Users/someone/Downloads/reply.txt" (gb::form-reply-save-to reply)))))

;;; Identities -------------------------------------------------------------------------------

(test identities-are-shown-where-they-fit-best
  (with-data-directory ()
    (gb::create-identity "example.org" :name "Everywhere")
    (gb::create-identity "example.org" :path "/club" :name "Club")
    (gb::create-identity "other.example" :name "Elsewhere")
    (flet ((shown (url) (let ((i (gb::identity-for-url url))) (and i (gb::identity-name i)))))
      (is (string= "Everywhere" (shown "gemini://example.org/")))
      (is (string= "Club" (shown "gemini://example.org/club/room")))
      (is (string= "Everywhere" (shown "gemini://example.org/clubhouse-not")) "prefix, not word")
      (is (string= "Elsewhere" (shown "gemini://OTHER.example/x")))
      (is (null (shown "gemini://stranger.example/"))))
    ;; Kept, and read back.
    (gb::forget-loaded-identities)
    (is (equal '("Everywhere" "Club" "Elsewhere") (mapcar #'gb::identity-name (gb::identities))))
    (is (gb::update-identity 1 :name "The club" :path "club/members"))
    (is (string= "/club/members" (gb::identity-path (second (gb::identities)))))
    (is (not (gb::update-identity 1 :name "  ")))
    (let ((files (list (gb::identity-certificate (first (gb::identities)))
                       (gb::identity-key (first (gb::identities))))))
      (is (gb::delete-identity 0))
      (is (notany #'probe-file files) "its files are gone too"))
    (is (equal '("The club" "Elsewhere") (mapcar #'gb::identity-name (gb::identities))))))

(test identities-from-before-the-list-are-kept
  (with-data-directory ()
    (let ((certificate (gb::data-file "identities/old.example-cert.pem"))
          (key (gb::data-file "identities/old.example-key.pem")))
      (ensure-directories-exist certificate)
      (g:make-identity certificate key :common-name "old.example")
      (gb::forget-loaded-identities)
      (let ((identity (first (gb::identities))))
        (is (string= "old.example" (gb::identity-host identity)))
        (is (string= "/" (gb::identity-path identity)))
        (is (string= "old.example" (gb::identity-name (gb::identity-for-url "gemini://old.example/a"))))))))

(test the-timeout-setting
  (with-data-directory ()
    (is (= 30 (gb::request-timeout)))
    (gb::set-request-timeout "12")
    (is (= 12 (gb::request-timeout)))
    (signals error (gb::set-request-timeout "soon"))
    (signals error (gb::set-request-timeout 2))
    (signals error (gb::set-request-timeout 10000))
    (is (= 12 (gb::request-timeout)))))
