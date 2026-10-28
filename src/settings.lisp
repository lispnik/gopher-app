;;;; settings.lisp -- where the browser keeps things, and the few settings.
;;;;
;;;; Everything lives in one directory: ~/Library/Application Support/Gopher/,
;;;; or wherever GOPHER_BROWSER_HOME says (the tests point it at a temporary
;;;; directory, so they never touch the real one).

(in-package #:gopher-browser)

(defparameter *default-home-page* "about:start")

(defun data-directory ()
  "The directory the browser's files are in, as a directory pathname.  Made if
it is not there."
  (let* ((override (uiop:getenv "GOPHER_BROWSER_HOME"))
         (directory (if (and override (plusp (length override)))
                        (uiop:ensure-directory-pathname override)
                        (merge-pathnames "Library/Application Support/Gopher/"
                                         (user-homedir-pathname)))))
    (ensure-directories-exist directory)
    directory))

(defun data-file (name)
  "The pathname of NAME in the data directory."
  (merge-pathnames name (data-directory)))

;;; Reading and writing Lisp data -------------------------------------------------

(defun read-data-file (name)
  "The one form in data file NAME, or NIL if there is no such file or it does
not read.  Never evaluates anything."
  (let ((path (data-file name)))
    (when (probe-file path)
      (ignore-errors
       (with-open-file (in path :external-format :utf-8)
         (with-standard-io-syntax
           (let ((*read-eval* nil)
                 (*package* (find-package :keyword)))
             (read in nil nil))))))))

(defun write-data-file (name form)
  "Write FORM to data file NAME, replacing it whole: written beside it, then
renamed over it, so a crash leaves the old file or the new one."
  (let* ((path (data-file name))
         (temporary (make-pathname :name (format nil "~a.new" (pathname-name path))
                                   :defaults path)))
    (with-open-file (out temporary :direction :output :if-exists :supersede
                                   :external-format :utf-8)
      (with-standard-io-syntax
        ;; Not readably: a base-string would print as #A((19) BASE-CHAR . "..."),
        ;; which reads back but is no longer a file a person can read.
        (let ((*package* (find-package :keyword))
              (*print-readably* nil))
          (prin1 form out)
          (terpri out))))
    (rename-file temporary path)
    path))

;;; Settings ---------------------------------------------------------------------

(defun settings ()
  (let ((form (read-data-file "settings.sexp")))
    (if (and (listp form) (evenp (length form))) form '())))

(defun setting (key &optional default)
  (getf (settings) key default))

(defun (setf setting) (value key)
  (let ((settings (copy-list (settings))))
    (setf (getf settings key) value)
    (write-data-file "settings.sexp" settings)
    value))

(defun home-page ()
  "The URL New Window and Home go to."
  (let ((url (setting :home-page)))
    (if (and (stringp url) (plusp (length url))) url *default-home-page*)))

(defun set-home-page (url)
  (setf (setting :home-page) url))

;;; Text size -------------------------------------------------------------------

(defparameter +text-scales+ '(0.7d0 0.8d0 0.9d0 1d0 1.1d0 1.25d0 1.5d0 1.75d0 2d0 2.5d0)
  "The sizes Make Text Bigger and Smaller step through.")

(defun text-scale ()
  "How much bigger than standard the text is drawn: 1.0 is standard."
  (let ((scale (setting :text-scale)))
    (if (and (realp scale) (<= 0.5 scale 3)) (float scale 1d0) 1d0)))

(defun set-text-scale (scale)
  "Set the text scale to the step nearest SCALE."
  (setf (setting :text-scale)
        (reduce (lambda (a b) (if (< (abs (- a scale)) (abs (- b scale))) a b))
                +text-scales+)))

(defun next-text-scale (direction)
  "The scale one step bigger (DIRECTION 1) or smaller (-1) than now, or the
same at either end."
  (let* ((now (text-scale))
         (steps (if (plusp direction) +text-scales+ (reverse +text-scales+))))
    (float (or (find-if (lambda (s) (if (plusp direction) (> s (+ now 0.01)) (< s (- now 0.01))))
                        steps)
               now)
           1d0)))

;;; Link numbers ------------------------------------------------------------------

(defun link-numbers-p ()
  "Whether each link is drawn after its number, for following it by typing."
  ;; T as the browser writes it, or :T as a person might (the file is read
  ;; in the keyword package).
  (and (member (setting :link-numbers) '(t :t)) t))

(defun set-link-numbers (on)
  (setf (setting :link-numbers) (and on t)))

;;; Searching from the address bar -----------------------------------------------------

(defparameter +search-engines+
  '((:kennedy "Kennedy" "gemini://kennedy.gemi.dev/search?~a")
    (:tlgs "TLGS" "gemini://tlgs.one/search?~a")
    (:veronica "Veronica-2" "gopher://gopher.floodgap.com/7/v2/vs%09~a"))
  "What words typed into the address bar are searched with: a key, a name for
the menu, and the URL with ~a where the query goes.")

(defun search-engine ()
  (let ((key (setting :search-engine)))
    (if (assoc key +search-engines+) key :kennedy)))

(defun set-search-engine (key)
  (setf (setting :search-engine) key))

;;; Windows -----------------------------------------------------------------------

(defun saved-window-frame ()
  "The frame, as AppKit writes one, of the last window closed, or NIL."
  (let ((frame (setting :window-frame)))
    (and (stringp frame) (plusp (length frame)) frame)))

(defun save-window-frame (frame)
  (setf (setting :window-frame) frame))

(defun saved-session ()
  "The windows open when the browser last quit: a list of groups, each a plist
(:FRAME string :URLS list :SELECTED index) -- a window and the tabs in it."
  (let ((form (read-data-file "session.sexp")))
    (loop for group in (if (listp form) form '())
          for urls = (and (listp group) (getf group :urls))
          when (and urls (every #'stringp urls))
            collect group)))

(defun save-session (groups)
  (write-data-file "session.sexp" groups))

;;; The network --------------------------------------------------------------------

(defun proxy ()
  "The SOCKS5 proxy every request goes through, as a socks5:// URL, or NIL."
  (let ((proxy (setting :proxy)))
    (and (stringp proxy) (plusp (length (string-trim " " proxy)))
         (string-trim " " proxy))))

(defun set-proxy (url)
  "Use the SOCKS5 proxy URL (\"socks5://[user:pass@]host:port\"), or none if
URL is NIL or empty.  Signals an error for anything that is not a socks5:// URL."
  (let ((url (and url (string-trim " " url))))
    (when (and url (plusp (length url)))
      (unless (and (> (length url) 9) (string-equal "socks5://" url :end2 9))
        (error "A proxy is a socks5:// address, such as socks5://127.0.0.1:9050 for Tor."))
      (g:parse-url url))
    (setf (setting :proxy) (and url (plusp (length url)) url))))

(defun gopher-tls-p ()
  "Whether gopher:// addresses try TLS first (and fall back to plain Gopher
when the server does not speak it).  On unless turned off."
  (not (member (setting :gopher-tls :default) '(nil :off))))

(defun set-gopher-tls (on)
  (setf (setting :gopher-tls) (if on t :off)))

(defparameter +timeout-range+ '(5 . 600)
  "The fewest and most seconds the timeout may be set to.")

(defun request-timeout ()
  "Seconds to wait for a server to connect, and for each read: 30 unless set."
  (let ((seconds (setting :timeout)))
    (if (and (integerp seconds) (<= (car +timeout-range+) seconds (cdr +timeout-range+)))
        seconds
        30)))

(defun set-request-timeout (seconds)
  "Set the timeout.  Signals an error for anything but a whole number of
seconds in +TIMEOUT-RANGE+."
  (let ((n (if (stringp seconds)
               (ignore-errors (parse-integer (string-trim " " seconds)))
               seconds)))
    (unless (and (integerp n) (<= (car +timeout-range+) n (cdr +timeout-range+)))
      (error "The timeout is a number of seconds from ~d to ~d."
             (car +timeout-range+) (cdr +timeout-range+)))
    (setf (setting :timeout) n)))
