;;;; window.lisp -- one browser window: the address bar, the page, the status line.
;;;;
;;;; One controller per window is the target of its controls, the delegate of
;;;; its window and of its text view, and so -- by the responder chain -- the
;;;; receiver of the menu's Back, Reload, Add Bookmark and the rest when its
;;;; window is the key one.
;;;;
;;;; A fetch runs on a thread of its own.  Everything it reports comes back
;;;; through POST-TO-MAIN-THREAD tagged with the GENERATION it was started
;;;; under; a newer navigation, or Stop, moves the generation on, and anything
;;;; still arriving for an older one is dropped.

(in-package #:gopher-browser)

(objc:define-objc-class browser-controller ()
  ((session :initarg :session :accessor controller-session)
   (window :initform nil :accessor controller-window)
   (address-field :initform nil :accessor controller-address-field)
   (back-button :initform nil :accessor controller-back-button)
   (forward-button :initform nil :accessor controller-forward-button)
   (reload-button :initform nil :accessor controller-reload-button)
   (text-view :initform nil :accessor controller-text-view)
   (status-label :initform nil :accessor controller-status-label)
   (spinner :initform nil :accessor controller-spinner)
   (page :initform nil :accessor controller-page
         :documentation "The page on show, whose links the text's links index.")
   (outcome :initform nil :accessor controller-outcome
            :documentation "The outcome that page came from, for Save Page As.")
   (streaming-page :initform nil :accessor controller-streaming-page
                   :documentation "The page being drawn as its lines arrive.")
   (generation :initform 0 :accessor controller-generation)
   (handle :initform nil :accessor controller-handle)
   (loading :initform nil :accessor controller-loading-p)
   (loading-url :initform nil :accessor controller-loading-url)
   (link-ranges :initform (make-array 0 :adjustable t :fill-pointer 0)
                :accessor controller-link-ranges
                :documentation "Link index -> (START . LENGTH) of its text, in UTF-16 units.")
   (focused-link :initform nil :accessor controller-focused-link
                 :documentation "The link Tab last moved to, which Return follows.")
   (typed-number :initform "" :accessor controller-typed-number
                 :documentation "Digits typed on the page: the number of a link to follow.")
   (status-text :initform "" :accessor controller-status-text
                :documentation "What the status line says when no link is pointed at.")
   (hover-link :initform nil :accessor controller-hover-link)
   (completion :initform nil :accessor controller-completion
               :documentation "(TEXT . URL): the address field's completion, and where it leads.")
   (closed :initform nil :accessor controller-closed-p)
   (toolbar-items :initform '() :accessor controller-toolbar-items
                  :documentation "Toolbar identifier -> its NSToolbarItem, made with the window.")
   (typed-length :initform 0 :accessor controller-typed-length
                 :documentation "How long the address field's text was, to tell typing from deleting."))
  (:objc-class-name "GopherBrowserWindowController"))

(defvar *closed-pages* '()
  "The addresses of the tabs and windows closed, the latest first, for
Reopen Closed Tab.")
(defparameter +closed-pages-limit+ 20)

(defvar *controllers* '()
  "Every open window's controller.  Held here because nothing in AppKit
retains a target or a delegate.")

(defmacro define-controller-method ((selector result-type &key on-error)
                                    (&rest arguments) &body body)
  "A method of the controller.  Nothing may unwind into AppKit, so whatever
BODY signals is logged and ON-ERROR is answered instead."
  `(objc:define-objc-method (,selector ,result-type)
       ((self browser-controller) ,@arguments)
     (declare (ignorable self ,@(mapcar #'first arguments)))
     (handler-case (progn ,@body)
       (error (condition)
         (note "~a: ~a" ,selector condition)
         ,on-error))))

;;; The controls --------------------------------------------------------------------

(defun show-status-line (controller text)
  (objc:invoke (controller-status-label controller) "setStringValue:" text))

(defun set-status (controller control &rest arguments)
  "Say something in the status line.  While a link is pointed at, its address
is on show instead, and this comes back when the pointer leaves."
  (let ((text (if control (apply #'format nil control arguments) "")))
    (setf (controller-status-text controller) text)
    (unless (controller-hover-link controller)
      (show-status-line controller text))))

(defun current-url (controller)
  "The URL of the page on show, or of the one loading if nothing is shown yet."
  (or (and (controller-page controller) (page-url (controller-page controller)))
      (controller-loading-url controller)))

(defun display-url (url)
  "URL as a person would type it: an internationalised host in its own letters."
  (handler-case (if (about-url-p url)
                    url
                    (g:url-display-string (g:parse-url url)))
    (error () url)))

(defun show-address (controller url)
  (objc:invoke (controller-address-field controller) "setStringValue:"
               (display-url (or url ""))))

(defun refresh-controls (controller)
  "Make the buttons agree with the history and with whether a page is loading."
  (let ((history (session-history (controller-session controller)))
        (loading (controller-loading-p controller)))
    (objc:invoke (controller-back-button controller) "setEnabled:" (g:history-back-p history))
    (objc:invoke (controller-forward-button controller) "setEnabled:"
                 (g:history-forward-p history))
    (objc:invoke (controller-reload-button controller) "setImage:"
                 (symbol-image (if loading "xmark" "arrow.clockwise")
                               (if loading "Stop" "Reload")))
    (objc:invoke (controller-reload-button controller) "setAction:"
                 (objc:coerce-to-selector (if loading "stopLoading:" "reload:")))
    (if loading
        (objc:invoke (controller-spinner controller) "startAnimation:" (cffi:null-pointer))
        (objc:invoke (controller-spinner controller) "stopAnimation:" (cffi:null-pointer)))))

(defparameter +reading-width+ 700d0
  "The widest a column of text is drawn, at standard size, in points: about
the eighty columns of a Gopher menu, and a comfortable measure for prose.")

(defparameter +least-margin+ 24d0)

(defun page-margin (controller)
  "The margin either side of the text: enough to keep the column at its
reading width, and never less than +LEAST-MARGIN+."
  (let ((width (aref (objc:invoke (objc:invoke (controller-text-view controller)
                                               "enclosingScrollView")
                                  "contentSize")
                     0)))
    (max +least-margin+ (/ (- width (* +reading-width+ (text-scale))) 2))))

(defun update-layout (controller)
  "Centre the column of text in the window, at its reading width."
  (objc:invoke (controller-text-view controller) "setTextContainerInset:"
               (vector (float (page-margin controller) 1d0) 18d0)))

(defun text-width (controller)
  "How wide a picture may be drawn: the column's width."
  (let ((frame (objc:invoke (controller-text-view controller) "frame")))
    (max 100d0 (- (aref frame 2) (* 2 (page-margin controller)) 10d0))))

(defun text-storage (controller)
  (objc:invoke (controller-text-view controller) "textStorage"))

(defun page-string (controller)
  "The text on show, as a Lisp string."
  (or (objc:invoke-into 'string (text-storage controller) "string") ""))

(defun note-link-ranges (controller ranges)
  "Remember where links are in the text: RANGES as RUNS-ATTRIBUTED-STRING
gives them."
  (let ((vector (controller-link-ranges controller)))
    (loop for (index start . length) in ranges
          do (when (>= index (length vector))
               (adjust-array vector (1+ index) :fill-pointer (1+ index) :initial-element nil))
             (setf (aref vector index) (cons start length)))))

(defun forget-page-state (controller)
  (setf (fill-pointer (controller-link-ranges controller)) 0
        (controller-focused-link controller) nil
        (controller-typed-number controller) ""
        (controller-hover-link controller) nil))

(defun show-page (controller page &key (scroll-to-top t))
  "Draw PAGE in place of whatever was there, scrolled to the top."
  (forget-page-state controller)
  (objc:with-autorelease-pool ()
    (let ((storage (text-storage controller)))
      (multiple-value-bind (string ranges) (page-attributed-string page (text-width controller))
        (objc:invoke storage "beginEditing")
        (objc:invoke storage "setAttributedString:" string)
        (objc:invoke storage "endEditing")
        (note-link-ranges controller ranges))))
  (when scroll-to-top
    (objc:invoke (controller-text-view controller) "scrollPoint:" #(0d0 0d0)))
  (setf (controller-page controller) page)
  (objc:invoke (controller-window controller) "setTitle:" (page-display-title page)))

(defun append-runs (controller runs links)
  (objc:with-autorelease-pool ()
    (let ((storage (text-storage controller)))
      (multiple-value-bind (string ranges)
          (runs-attributed-string runs links :offset (objc:invoke storage "length"))
        (objc:invoke storage "beginEditing")
        (objc:invoke storage "appendAttributedString:" string)
        (objc:invoke storage "endEditing")
        (note-link-ranges controller ranges)))))

(defun redraw-page (controller)
  "Draw the page on show again, as the text size or link numbers now say,
keeping its place."
  (let ((page (controller-page controller)))
    (update-layout controller)
    (when page
      (let* ((clip (objc:invoke (objc:invoke (controller-text-view controller)
                                             "enclosingScrollView")
                                "contentView"))
             (y (aref (objc:invoke clip "bounds") 1)))
        (show-page controller page :scroll-to-top nil)
        (objc:invoke (controller-text-view controller) "scrollPoint:" (vector 0d0 y))))))

;;; Loading --------------------------------------------------------------------------

(defun current-generation-p (controller generation)
  (= generation (controller-generation controller)))

(defun stream-lines (controller url lines)
  "On the main thread: LINES of the page loading from URL have arrived.  The
first batch replaces what was on show."
  (let ((page (controller-streaming-page controller)))
    (unless page
      (setf page (make-page url)
            (controller-streaming-page controller) page)
      (show-page controller page))
    (append-runs controller (page-add-lines page lines) (page-links page))
    (setf (controller-page controller) page)
    (objc:invoke (controller-window controller) "setTitle:" (page-display-title page))))

(defun make-progress-reporter (controller generation)
  "An :ON-PROGRESS callback for a fetch thread that tells the status line at
most ten times a second."
  (let ((last 0))
    (lambda (bytes total)
      (let ((now (get-internal-real-time)))
        (when (> (- now last) (floor internal-time-units-per-second 10))
          (setf last now)
          (post-to-main-thread
           (lambda ()
             (when (current-generation-p controller generation)
               (set-status controller "Loading… ~a~@[ of ~a~]"
                           (size-text bytes) (and total (size-text total)))))))))))

(defun navigate (controller url &key query accept-certificate (record t) reload (hops 0)
                                    form answers upload)
  "Go to URL.  Returns at once; the page arrives later.

QUERY answers a server's request for input.  RECORD says what showing the
page does to the history: T makes it a new entry, :BACK and :FORWARD step
there (only once the page has arrived, so a failed fetch leaves the history
where it was), and NIL leaves it alone, as Reload does.  HOPS counts redirects
followed across schemes, which the library does not follow itself.

FORM fetches a Gopher+ item's form, to be asked before the item; ANSWERS
submits answers to it.  UPLOAD is (DATA MIME-TYPE TOKEN): send DATA to URL
with Titan, and show what the server answers."
  (let* ((url (string-trim " " url))
         (generation (incf (controller-generation controller)))
         (handle (g:make-request-handle))
         (session (controller-session controller)))
    (let ((previous (controller-handle controller)))
      (when previous (g:cancel-request previous)))
    (setf (controller-handle controller) handle
          (controller-loading-p controller) t
          (controller-loading-url controller) url
          (controller-streaming-page controller) nil)
    (show-address controller url)
    (set-status controller "Loading…")
    (refresh-controls controller)
    (sb-thread:make-thread
     (lambda ()
       (let ((outcome
               (if upload
                   (destructuring-bind (data mime-type token) upload
                     (session-upload session url data :mime-type mime-type :token token
                                                      :handle handle
                                                      :accept-certificate accept-certificate))
                   (session-fetch
                       session url
                       :query query :handle handle :reload reload
                       :form form :answers answers
                       :accept-certificate accept-certificate
                       :on-progress (make-progress-reporter controller generation)
                       :on-lines (lambda (lines)
                                   (let ((lines (coerce lines 'list)))
                                     (post-to-main-thread
                                      (lambda ()
                                        (when (current-generation-p controller generation)
                                          (stream-lines controller url lines))))))))))
         (post-to-main-thread
          (lambda ()
            (when (current-generation-p controller generation)
              (finish-navigation controller outcome
                                 :query query :record record :hops hops
                                 :answers answers :upload upload))))))
     :name "gopher-browser fetch")
    generation))

(defun stop-loading (controller)
  "Abandon the fetch in progress, if there is one."
  (when (controller-loading-p controller)
    (incf (controller-generation controller))
    (let ((handle (controller-handle controller)))
      (when handle (g:cancel-request handle)))
    (setf (controller-loading-p controller) nil
          (controller-handle controller) nil
          (controller-streaming-page controller) nil)
    (show-address controller (current-url controller))
    (set-status controller "Stopped.")
    (refresh-controls controller)))

(defun loaded (controller)
  "The fetch is over, whatever came of it."
  (setf (controller-loading-p controller) nil
        (controller-handle controller) nil))

(defun show-outcome-page (controller outcome record)
  "Show OUTCOME's page, unless every line of it is already on show from
streaming, and make it the current history entry."
  (let* ((page (outcome-page outcome))
         (streamed (controller-streaming-page controller)))
    (if (and streamed (not (page-image page))
             (= (page-line-count streamed) (page-line-count page)))
        ;; Already drawn.  The finished page has the same links in the same
        ;; order, and the title from the whole document.
        (progn (setf (controller-page controller) page)
               (objc:invoke (controller-window controller) "setTitle:"
                            (page-display-title page)))
        (show-page controller page))
    (setf (controller-streaming-page controller) nil
          (controller-outcome controller) outcome)
    (let ((session (controller-session controller)))
      (case record
        (:back (session-back session))
        (:forward (session-forward session))
        ((nil))
        (t (session-record session (page-url page)))))
    (record-visit (page-url page) (page-display-title page))
    (rebuild-history-menu)
    (show-address controller (page-url page))
    (let ((links (length (page-links page)))
          (secure (let ((response (outcome-response outcome)))
                    (and response (g:response-certificate response) t))))
      (set-status controller "~{~a~^ · ~}"
                  (remove nil (list (and secure "Encrypted (TLS)")
                                    (and (plusp links) (format nil "~d link~:p" links))))))))

;;; Downloads ----------------------------------------------------------------------------
;;;
;;; A download streams into the file it is saved as, so its size is limited by
;;; the disk rather than by memory.  It belongs to the Downloads window, not to
;;; the browser window it started in: that window can go elsewhere, or close,
;;; and the download carries on.  Stop it from the Downloads window.

(defun start-download (controller url path &key accept-certificate)
  "Fetch URL into the file PATH, and show it in the Downloads window."
  (let ((download (make-download url path))
        (session (controller-session controller)))
    (set-status controller "Downloading ~a…" (download-name download))
    (show-downloads-window)
    (sb-thread:make-thread
     (lambda ()
       (let* ((last 0)
              (outcome (session-download
                        session url path
                        :handle (download-handle download)
                        :accept-certificate accept-certificate
                        :on-progress (lambda (bytes total)
                                       (let ((now (get-internal-real-time)))
                                         (when (> (- now last)
                                                  (floor internal-time-units-per-second 5))
                                           (setf last now)
                                           (post-to-main-thread
                                            (lambda ()
                                              (setf (download-bytes download) bytes
                                                    (download-total download) total)
                                              (refresh-downloads-window)))))))))
         (post-to-main-thread
          (lambda () (finish-download controller download outcome)))))
     :name "gopher-browser download")
    download))

(defun finish-download (controller download outcome)
  (download-finished download outcome)
  (refresh-downloads-window)
  (case (outcome-kind outcome)
    (:certificate-changed
     (when (confirm-new-certificate outcome)
       (start-download controller (download-url download) (download-path download)
                       :accept-certificate t)))
    (t
     ;; The window may have gone on to other things, or be gone.
     (unless (controller-closed-p controller)
       (set-status controller "~a: ~a" (download-name download)
                   (download-status-text download))))))

(defun download-link (controller link)
  "Ask where to save LINK's file, then fetch it there."
  (let ((path (choose-save-path (suggested-file-name (link-url link) nil))))
    (when path
      (start-download controller (link-url link) path))))

(defun confirm-new-certificate (outcome)
  (confirm (format nil "The certificate of ~a has changed." (outcome-host outcome))
           (format nil "~a~%~%A server's certificate changes when it is renewed, but it also changes when someone is impersonating the server.  Trust the new one only if you expected the change."
                   (outcome-message outcome))
           "Trust New Certificate"))

(defun show-error (controller outcome title)
  "Show a page saying what went wrong, and leave the history alone."
  (let ((page (message-page (outcome-url outcome) title
                            (list :error (or (outcome-message outcome) "Unknown error."))
                            (list :link (outcome-url outcome) "Try again"))))
    (show-page controller page)
    (setf (controller-outcome controller) outcome)
    (show-address controller (outcome-url outcome))
    (set-status controller "~a" (or (outcome-message outcome) title))))

(defun finish-navigation (controller outcome &key query record (hops 0) answers upload)
  "On the main thread: act on OUTCOME, the end of a fetch."
  (loaded controller)
  (let ((url (outcome-url outcome)))
    (ecase (outcome-kind outcome)
      (:page
       (show-outcome-page controller outcome record)
       ;; A Gopher+ form that asked where to keep the reply.
       (let ((save-to (and (form-reply-p answers) (form-reply-save-to answers)))
             (response (outcome-response outcome)))
         (when (and save-to response (g:response-body response))
           (write-octets save-to (g:response-body response))
           (set-status controller "The reply is saved in ~a." save-to))))
      (:redirect
       (if (< hops 5)
           (navigate controller (outcome-target outcome) :record record :hops (1+ hops))
           (show-error controller (make-outcome :kind :error :url url
                                                :message "Too many redirects.")
                       "Could not load the page")))
      (:external
       (show-address controller (current-url controller))
       (if (open-externally (outcome-target outcome))
           (set-status controller "Opened ~a in another application." (outcome-target outcome))
           (set-status controller "No application opens ~a." (outcome-target outcome))))
      (:input
       (show-address controller (current-url controller))
       (set-status controller nil)
       (let ((answer (ask-for-text (outcome-prompt outcome)
                                   :detail (display-url url)
                                   :sensitive (outcome-sensitive-p outcome)
                                   :button (if (outcome-sensitive-p outcome) "Send" "Search"))))
         (when answer
           (navigate controller url :query answer :record record :hops hops))))
      (:too-large
       (show-address controller (current-url controller))
       (let ((path (choose-save-path (outcome-suggested-name outcome)
                                     "This is too large to show.  Save it?")))
         (if path
             (start-download controller url path)
             (set-status controller nil))))
      (:download
       (show-address controller (current-url controller))
       (let* ((body (g:response-body (outcome-response outcome)))
              (path (choose-save-path (outcome-suggested-name outcome))))
         (cond ((null path) (set-status controller nil))
               (t (write-octets path body)
                  (set-status controller "Saved ~a to ~a." (size-text (length body)) path)))))
      (:certificate-changed
       (if (confirm-new-certificate outcome)
           (navigate controller url :query query :accept-certificate t :record record :hops hops
                                    :answers answers :upload upload)
           (show-error controller outcome "Certificate changed")))
      (:certificate-required
       (if (confirm (format nil "~a asks who you are." (outcome-host outcome))
                    (format nil "~@[~a~%~%~]Make an identity -- a client certificate -- for ~a?  The browser will show it to ~a from now on, and to no other host."
                            (let ((message (outcome-message outcome)))
                              (and (plusp (length message)) message))
                            (outcome-host outcome) (outcome-host outcome))
                    "Make Identity")
           (handler-case
               (progn (session-create-identity (controller-session controller)
                                               (outcome-host outcome))
                      (navigate controller url :query query :record record :hops hops))
             (error (condition)
               (show-error controller (make-outcome :kind :error :url url
                                                    :message (format nil "The identity could not be made: ~a" condition))
                           "No identity")))
           (show-error controller outcome "Identity required")))
      (:form
       (show-address controller (current-url controller))
       (set-status controller nil)
       (let ((answers (ask-gopher-plus-form (or (link-label-for controller url) "Form")
                                            (outcome-questions outcome))))
         (when answers
           (navigate controller url :answers answers :record record))))
      (:error
       (show-error controller outcome "Could not load the page"))
      (:cancelled
       (set-status controller "Stopped."))))
  (refresh-controls controller))

;;; Following links ---------------------------------------------------------------------

(defun command-key-down-p ()
  (let ((event (objc:invoke (objc.runloop:shared-application :activation-policy nil)
                            "currentEvent")))
    (and (not (null-object-p event))
         (logtest +ns-event-modifier-flag-command+ (objc:invoke event "modifierFlags")))))

(defun follow-link (controller index &key new-window)
  "Follow link INDEX of the page on show."
  (let* ((page (controller-page controller))
         (link (and page (< -1 index (length (page-links page)))
                    (aref (page-links page) index))))
    (when link
      (ecase (link-kind link)
        (:follow
         (cond ((eq (link-plus link) :ask)
                (navigate controller (link-url link) :form t))
               ((download-link-p link) (download-link controller link))
               (new-window (open-tab controller (link-url link) :background t))
               (t (navigate controller (link-url link)))))
        (:input
         (let ((answer (ask-for-text (link-label link) :detail (display-url (link-url link)))))
           (when answer
             (navigate controller (link-url link) :query answer))))))))

(define-controller-method ("textView:clickedOnLink:atIndex:" objc:objc-bool :on-error nil)
    ((text-view objc:objc-object-pointer)
     (link objc:objc-object-pointer)
     (character-index (:unsigned :long)))
  (let ((index (link-index link)))
    (when index
      (follow-link self index :new-window (command-key-down-p))
      t)))

;;; Actions: the buttons, the address field and the menu --------------------------------

(defun address-field-target (controller)
  "Where the address field's text leads: the URL of the completion it holds,
if it holds one; otherwise the text as typed, an address or words to search."
  (let ((text (string-trim " " (or (objc:invoke-into 'string (controller-address-field controller)
                                                     "stringValue")
                                   "")))
        (completion (controller-completion controller)))
    (if (and completion (string= text (car completion)))
        (cdr completion)
        (address-input-url text))))

(define-controller-method ("go:" :void) ((sender objc:objc-object-pointer))
  (let ((url (address-field-target self)))
    (setf (controller-completion self) nil)
    (when url
      (navigate self url)
      ;; Back to the page, so that the arrow keys scroll it.
      (objc:invoke (controller-window self) "makeFirstResponder:" (controller-text-view self)))))

(defun go-back (controller)
  (let ((url (session-back-url (controller-session controller))))
    (when url (navigate controller url :record :back))))

(defun go-forward (controller)
  (let ((url (session-forward-url (controller-session controller))))
    (when url (navigate controller url :record :forward))))

(define-controller-method ("goBack:" :void) ((sender objc:objc-object-pointer))
  (go-back self))

(define-controller-method ("goForward:" :void) ((sender objc:objc-object-pointer))
  (go-forward self))

(define-controller-method ("reload:" :void) ((sender objc:objc-object-pointer))
  (let ((url (current-url self)))
    (when url (navigate self url :record nil :reload t))))

(define-controller-method ("stopLoading:" :void) ((sender objc:objc-object-pointer))
  (stop-loading self))

(define-controller-method ("goHome:" :void) ((sender objc:objc-object-pointer))
  (navigate self (home-page)))

(define-controller-method ("openLocation:" :void) ((sender objc:objc-object-pointer))
  (objc:invoke (controller-window self) "makeFirstResponder:" (controller-address-field self))
  (objc:invoke (controller-address-field self) "selectText:" (cffi:null-pointer)))

(define-controller-method ("setHomePage:" :void) ((sender objc:objc-object-pointer))
  (let ((url (current-url self)))
    (when url
      (set-home-page url)
      (set-status self "The home page is now ~a." (display-url url)))))

(define-controller-method ("addBookmark:" :void) ((sender objc:objc-object-pointer))
  (let ((page (controller-page self)))
    (when (and page (not (about-url-p (page-url page))))
      (add-bookmark (page-display-title page) (page-url page))
      (rebuild-bookmarks-menu)
      (set-status self "Bookmarked ~a." (page-display-title page)))))

(define-controller-method ("removeBookmark:" :void) ((sender objc:objc-object-pointer))
  (let ((url (current-url self)))
    (when url
      (remove-bookmark url)
      (rebuild-bookmarks-menu)
      (set-status self "Removed the bookmark for ~a." (display-url url)))))

(define-controller-method ("showBookmarks:" :void) ((sender objc:objc-object-pointer))
  (navigate self "about:bookmarks"))

(define-controller-method ("openBookmark:" :void) ((sender objc:objc-object-pointer))
  (let ((url (bookmark-url-for-item sender)))
    (when url (navigate self url))))

;;; Right-clicking a link ----------------------------------------------------------------

(defun link-label-for (controller url)
  "The label of the link on show that goes to URL, or NIL."
  (let ((page (controller-page controller)))
    (and page
         (let ((link (find url (page-links page) :key #'link-url :test #'string=)))
           (and link (link-label link))))))

(defun link-at-character (controller index)
  "The index of the link whose text holds character INDEX, or NIL."
  (position-if (lambda (range)
                 (and range (<= (car range) index (+ (car range) (cdr range) -1))))
               (controller-link-ranges controller)))

(defun page-link (controller index)
  (let ((page (controller-page controller)))
    (and page index (< -1 index (length (page-links page)))
         (aref (page-links page) index))))

(defun link-menu-items (link)
  "The items a right-click on LINK adds to the text view's menu: (TITLE ACTION)."
  (append '(("Open Link in New Tab" "openLinkInNewTab:")
            ("Open Link in New Window" "openLinkInNewWindow:")
            ("Download Linked File…" "downloadLinkedFile:")
            ("Copy Link Address" "copyLinkAddress:"))
          (when (link-plus link)
            '(("Item Information (Gopher+)" "showItemInformation:")))))

(define-controller-method ("textView:menu:forEvent:atIndex:" objc:objc-object-pointer
                           :on-error menu)
    ((text-view objc:objc-object-pointer) (menu objc:objc-object-pointer)
     (event objc:objc-object-pointer) (character-index (:unsigned :long)))
  (let* ((index (link-at-character self character-index))
         (link (page-link self index)))
    (when link
      (objc:invoke menu "insertItem:atIndex:" (objc:invoke "NSMenuItem" "separatorItem") 0)
      (loop for (title action) in (reverse (link-menu-items link))
            do (let ((item (objc:invoke (objc:invoke "NSMenuItem" "alloc")
                                        "initWithTitle:action:keyEquivalent:"
                                        title (objc:coerce-to-selector action) "")))
                 (objc:invoke item "setTarget:" (objc:objc-object-pointer self))
                 (objc:invoke item "setTag:" index)
                 (objc:invoke menu "insertItem:atIndex:" item 0)
                 (objc:release item))))
    menu))

(defun sender-link (controller sender)
  "The link a context-menu item stands for, by its tag."
  (page-link controller (objc:invoke sender "tag")))

(define-controller-method ("openLinkInNewTab:" :void) ((sender objc:objc-object-pointer))
  (let ((link (sender-link self sender)))
    (when link (open-tab self (link-url link)))))

(define-controller-method ("openLinkInNewWindow:" :void) ((sender objc:objc-object-pointer))
  (let ((link (sender-link self sender)))
    (when link (open-window (link-url link)))))

(define-controller-method ("downloadLinkedFile:" :void) ((sender objc:objc-object-pointer))
  (let ((link (sender-link self sender)))
    (when link (download-link self link))))

(define-controller-method ("copyLinkAddress:" :void) ((sender objc:objc-object-pointer))
  (let ((link (sender-link self sender)))
    (when link (copy-to-pasteboard (link-url link)))))

(define-controller-method ("showItemInformation:" :void) ((sender objc:objc-object-pointer))
  (let ((link (sender-link self sender)))
    (when link (open-tab self (gopher-plus-info-url (link-url link))))))

(defun copy-to-pasteboard (string)
  (let ((pasteboard (objc:invoke "NSPasteboard" "generalPasteboard")))
    (objc:invoke pasteboard "clearContents")
    (objc:invoke pasteboard "setString:forType:" string "public.utf8-plain-text")))

;;; Page Info, View Source and uploading -------------------------------------------------------

(define-controller-method ("showPageInfo:" :void) ((sender objc:objc-object-pointer))
  (show-info (format nil "Page Info: ~a" (if (controller-page self)
                                             (page-display-title (controller-page self))
                                             "nothing"))
             (page-info-text (controller-session self) (controller-outcome self))))

(define-controller-method ("viewSource:" :void) ((sender objc:objc-object-pointer))
  (let ((url (current-url self)))
    (when (and url (not (about-url-p url)) (not (view-source-url-p url)))
      (open-tab self (concatenate 'string +view-source-prefix+ url)))))

(defun titan-url-for (url)
  "The titan:// address that would replace the gemini:// page at URL, or NIL."
  (let ((parsed (and url (ignore-errors (g:parse-url url)))))
    (when (and parsed (member (g:url-scheme parsed) '("gemini" "titan") :test #'string-equal))
      (let ((copy (g:copy-url parsed)))
        (setf (g:url-scheme copy) "titan"
              (g:url-query copy) nil
              (g:url-fragment copy) nil)
        (g:url-string copy)))))

(defun upload-fields (url text)
  (list (make-field :text "Upload to (a titan:// or gemini:// address)"
                    :value (or (titan-url-for url) "")
                    :placeholder "titan://example.org/path/page.gmi")
        (make-field :text "Type" :value "text/gemini")
        (make-field :password "Token, if the server asks for one")
        (make-field :lines "Text to upload" :value (or text ""))
        (make-field :file "Or upload a file instead of the text"
                    :placeholder "Choose a file to send it in place of the text")))

(defun ask-for-upload (controller &key text)
  "Ask what to upload, and where, then upload it in CONTROLLER's window."
  (let ((answers (run-form "Upload with Titan" (upload-fields (current-url controller) text)
                           :kind :upload)))
    (when answers
      (destructuring-bind (url mime-type token body file) answers
        (let ((url (string-trim " " url)))
          (cond ((zerop (length url))
                 (set-status controller "Nothing was uploaded: there was no address."))
                ((plusp (length (string-trim " " file)))
                 (navigate controller url
                           :upload (list (sb-ext:parse-native-namestring (string-trim " " file))
                                         mime-type token)))
                (t
                 (navigate controller url :upload (list body mime-type token)))))))))

(define-controller-method ("uploadWithTitan:" :void) ((sender objc:objc-object-pointer))
  (ask-for-upload self))

(define-controller-method ("editPageWithTitan:" :void) ((sender objc:objc-object-pointer))
  (let* ((outcome (controller-outcome self))
         (response (and outcome (outcome-response outcome))))
    (ask-for-upload self :text (and response (g:response-text response)))))

(define-controller-method ("showAllHistory:" :void) ((sender objc:objc-object-pointer))
  (navigate self "about:history"))

(define-controller-method ("openHistoryItem:" :void) ((sender objc:objc-object-pointer))
  (let ((url (history-url-for-item sender)))
    (when url (navigate self url))))

(define-controller-method ("savePage:" :void) ((sender objc:objc-object-pointer))
  (let* ((page (controller-page self))
         (outcome (controller-outcome self))
         (response (and outcome (outcome-response outcome)))
         (body (and response (g:response-body response))))
    (when page
      (let ((path (choose-save-path (suggested-file-name (page-url page)
                                                         (page-mime-type page))
                                    "Save the page as it was sent")))
        (when path
          (write-octets path (or body (babel:string-to-octets (page-text page)
                                                              :encoding :utf-8)))
          (set-status self "Saved to ~a." path))))))

;;; The responder chain asks before enabling a menu item.  Back and Forward
;;; are greyed when there is nowhere to go; Stop when nothing is loading.
(define-controller-method ("validateMenuItem:" objc:objc-bool :on-error t)
    ((item objc:objc-object-pointer))
  (let ((action (objc:selector-name (objc:invoke item "action")))
        (history (session-history (controller-session self))))
    (cond ((string= action "goBack:") (g:history-back-p history))
          ((string= action "reopenClosedTab:") (and *closed-pages* t))
          ((string= action "goForward:") (g:history-forward-p history))
          ((string= action "stopLoading:") (controller-loading-p self))
          ((member action '("addBookmark:" "savePage:") :test #'string=)
           (and (controller-page self) t))
          ((string= action "editPageWithTitan:")
           (and (titan-url-for (current-url self)) (controller-outcome self) t))
          ((string= action "viewSource:")
           (let ((url (current-url self)))
             (and url (not (about-url-p url)) (not (view-source-url-p url)))))
          ((string= action "toggleLinkNumbers:")
           (objc:invoke item "setState:" (if (link-numbers-p) 1 0))
           t)
          ((string= action "makeTextBigger:") (< (text-scale) (reduce #'max +text-scales+)))
          ((string= action "makeTextSmaller:") (> (text-scale) (reduce #'min +text-scales+)))
          (t t))))

(define-controller-method ("windowWillClose:" :void) ((notification objc:objc-object-pointer))
  (stop-loading self)
  (setf (controller-closed-p self) t)
  (let ((url (current-url self)))
    (when url
      (push url *closed-pages*)
      (when (> (length *closed-pages*) +closed-pages-limit+)
        (setf *closed-pages* (subseq *closed-pages* 0 +closed-pages-limit+)))))
  (ignore-errors
   (save-window-frame (objc:invoke-into 'string (controller-window self) "stringWithSavedFrame")))
  (setf *controllers* (remove self *controllers*)))

(define-controller-method ("windowDidResize:" :void) ((notification objc:objc-object-pointer))
  (update-layout self))

;;; Tabs: the system's own.  The tab bar's + button sends this.
(define-controller-method ("newWindowForTab:" :void) ((sender objc:objc-object-pointer))
  (open-tab self nil))

(define-controller-method ("newTab:" :void) ((sender objc:objc-object-pointer))
  (open-tab self nil))

(define-controller-method ("reopenClosedTab:" :void) ((sender objc:objc-object-pointer))
  (let ((url (pop *closed-pages*)))
    (when url (open-tab self url))))

;;; Text size and link numbers apply to every window, so each is redrawn.
(defun redraw-all-pages (&optional also)
  "Redraw every window's page, and ALSO's (a controller that may not be in
*CONTROLLERS*, as a test's is not)."
  (reset-text-styles)
  (dolist (controller (if also (adjoin also *controllers*) *controllers*))
    (redraw-page controller)))

(define-controller-method ("makeTextBigger:" :void) ((sender objc:objc-object-pointer))
  (set-text-scale (next-text-scale 1))
  (redraw-all-pages self))

(define-controller-method ("makeTextSmaller:" :void) ((sender objc:objc-object-pointer))
  (set-text-scale (next-text-scale -1))
  (redraw-all-pages self))

(define-controller-method ("makeTextStandardSize:" :void) ((sender objc:objc-object-pointer))
  (set-text-scale 1)
  (redraw-all-pages self))

(define-controller-method ("toggleLinkNumbers:" :void) ((sender objc:objc-object-pointer))
  (set-link-numbers (not (link-numbers-p)))
  (redraw-all-pages self))

;;; The address field ---------------------------------------------------------------------
;;;
;;; As letters are typed, the field finishes the address from the history and
;;; the bookmarks, the finished part selected, so typing on replaces it and
;;; Return accepts it.  Deleting never completes: what was deleted would come
;;; straight back.

(define-controller-method ("controlTextDidChange:" :void) ((notification objc:objc-object-pointer))
  (let* ((editor (objc:invoke (objc:invoke notification "userInfo") "objectForKey:"
                              "NSFieldEditor"))
         (typed (or (objc:invoke-into 'string editor "string") ""))
         (longer (> (length typed) (controller-typed-length self))))
    (setf (controller-typed-length self) (length typed)
          (controller-completion self) nil)
    (when longer
      (multiple-value-bind (text url) (complete-address typed)
        (when text
          (setf (controller-completion self) (cons text url))
          (objc:invoke editor "setString:" text)
          (objc:invoke editor "setSelectedRange:"
                       (cons (utf-16-length typed)
                             (- (utf-16-length text) (utf-16-length typed)))))))))

(define-controller-method ("controlTextDidBeginEditing:" :void)
    ((notification objc:objc-object-pointer))
  (setf (controller-typed-length self) 0
        (controller-completion self) nil))

;;; Pointing at links -----------------------------------------------------------------------
;;;
;;; A tracking area over the page tells the controller where the pointer is,
;;; and the status line shows the address of the link under it.

(defun link-at-point (controller point)
  "The index of the link at POINT, in the text view's coordinates, or NIL.
The point must be on the link's text itself, not in the empty space past the
end of its line, which the layout manager would also call its last character."
  (let* ((text-view (controller-text-view controller))
         (layout (objc:invoke text-view "layoutManager"))
         (container (objc:invoke text-view "textContainer"))
         (origin (objc:invoke text-view "textContainerOrigin"))
         (local (vector (- (aref point 0) (aref origin 0)) (- (aref point 1) (aref origin 1))))
         (length (objc:invoke (text-storage controller) "length")))
    (when (plusp length)
      (let* ((glyph (objc:invoke layout "glyphIndexForPoint:inTextContainer:" local container))
             (rect (objc:invoke layout "boundingRectForGlyphRange:inTextContainer:"
                                (cons glyph 1) container))
             (index (objc:invoke layout "characterIndexForGlyphAtIndex:" glyph)))
        (when (and (< index length)
                   (<= (aref rect 0) (aref local 0) (+ (aref rect 0) (aref rect 2)))
                   (<= (aref rect 1) (aref local 1) (+ (aref rect 1) (aref rect 3))))
          (position-if (lambda (range)
                         (and range (<= (car range) index (+ (car range) (cdr range) -1))))
                       (controller-link-ranges controller)))))))

(defun hover (controller index)
  "The pointer is over link INDEX now, or over no link if INDEX is NIL."
  (unless (eql index (controller-hover-link controller))
    (setf (controller-hover-link controller) index)
    (let* ((page (controller-page controller))
           (link (and index page (< index (length (page-links page)))
                      (aref (page-links page) index))))
      (show-status-line controller (if link
                                       (display-url (link-url link))
                                       (controller-status-text controller))))))

(define-controller-method ("mouseMoved:" :void) ((event objc:objc-object-pointer))
  (let ((text-view (controller-text-view self)))
    (hover self (link-at-point self (objc:invoke text-view "convertPoint:fromView:"
                                                 (objc:invoke event "locationInWindow")
                                                 (cffi:null-pointer))))))

(define-controller-method ("mouseExited:" :void) ((event objc:objc-object-pointer))
  (hover self nil))

(define-controller-method ("mouseEntered:" :void) ((event objc:objc-object-pointer))
  nil)

;;; The keyboard on the page ------------------------------------------------------------------
;;;
;;; Tab and Shift-Tab move from link to link, selecting each; Return follows
;;; the one selected.  Digits typed on the page make the number of a link
;;; (View › Show Link Numbers draws them), and Return follows it; Escape
;;; forgets them.

(defconstant +key-tab+ 48)
(defconstant +key-return+ 36)
(defconstant +key-enter+ 76)
(defconstant +key-escape+ 53)
(defconstant +modifier-shift+ (ash 1 17))
(defconstant +modifier-control+ (ash 1 18))
(defconstant +modifier-option+ (ash 1 19))

(defun focus-link (controller index)
  "Select link INDEX, scroll it into view, and say where it goes."
  (let ((range (aref (controller-link-ranges controller) index))
        (text-view (controller-text-view controller)))
    (setf (controller-focused-link controller) index)
    (objc:invoke text-view "setSelectedRange:" range)
    (objc:invoke text-view "scrollRangeToVisible:" range)
    (objc:invoke text-view "showFindIndicatorForRange:" range)
    (let ((link (aref (page-links (controller-page controller)) index)))
      (set-status controller "~a" (display-url (link-url link))))))

(defun next-link-index (controller direction)
  "The link after (DIRECTION 1) or before (-1) the focused one, wrapping."
  (let* ((ranges (controller-link-ranges controller))
         (indexes (loop for i below (length ranges) when (aref ranges i) collect i))
         (current (controller-focused-link controller)))
    (when indexes
      (if (null current)
          (if (plusp direction) (first indexes) (car (last indexes)))
          (let ((position (or (position current indexes) 0)))
            (nth (mod (+ position direction) (length indexes)) indexes))))))

(defun page-key (controller key-code characters modifiers)
  "Act on a key pressed on the page.  True if it was the browser's to act on;
NIL to let the text view have it."
  (let ((page (controller-page controller)))
    (when (and page (not (logtest modifiers (logior +ns-event-modifier-flag-command+
                                                    +modifier-control+ +modifier-option+))))
      (cond
        ((= key-code +key-tab+)
         (let ((index (next-link-index controller (if (logtest modifiers +modifier-shift+) -1 1))))
           (when index (focus-link controller index) t)))
        ((member key-code (list +key-return+ +key-enter+))
         (let* ((typed (controller-typed-number controller))
                (index (if (plusp (length typed))
                           (1- (parse-integer typed))
                           (controller-focused-link controller))))
           (setf (controller-typed-number controller) "")
           (cond ((and index (< -1 index (length (page-links page))))
                  (follow-link controller index)
                  t)
                 ((plusp (length typed))
                  (set-status controller "There is no link ~a." typed)
                  t))))
        ((= key-code +key-escape+)
         (when (plusp (length (controller-typed-number controller)))
           (setf (controller-typed-number controller) "")
           (set-status controller nil)
           t))
        ((and (= 1 (length characters)) (digit-char-p (char characters 0)))
         (setf (controller-typed-number controller)
               (concatenate 'string (controller-typed-number controller) characters))
         (set-status controller "Link ~a — Return to follow it, Escape to forget it."
                     (controller-typed-number controller))
         t)))))

(defun page-has-focus-p (controller)
  (cffi:pointer-eq (objc:invoke (controller-window controller) "firstResponder")
                   (controller-text-view controller)))

;;; The mouse's Back and Forward buttons, and swipes -------------------------------------
;;;
;;; A mouse's side buttons arrive as "other mouse" events, buttons 3 and 4, at
;;; whichever view is under the pointer -- the text view, a button, the address
;;; field -- and none of them does anything with them.  So the window takes
;;; them first: every event a window gets passes through its -sendEvent:, and
;;; the browser's window is a subclass whose -sendEvent: looks before handing
;;; the event on.  A trackpad swipe (Swipe between pages, in System Settings)
;;; arrives the same way.

(defconstant +ns-event-type-other-mouse-down+ 25)
(defconstant +ns-event-type-other-mouse-up+ 26)
(defconstant +ns-event-type-swipe+ 31)
(defconstant +back-button+ 3)
(defconstant +forward-button+ 4)

(defun event-navigation (type button delta-x)
  "What an event of TYPE means to the browser: :BACK, :FORWARD, :IGNORE (the
release of a button whose press already acted), or NIL for an event that is
none of its business.  BUTTON matters for mouse events and DELTA-X for swipes,
where a negative one is a swipe to the right: back."
  (cond ((= type +ns-event-type-other-mouse-down+)
         (cond ((eql button +back-button+) :back)
               ((eql button +forward-button+) :forward)))
        ((= type +ns-event-type-other-mouse-up+)
         (and (member button (list +back-button+ +forward-button+)) :ignore))
        ;; A three-finger swipe.  deltaX is positive for a swipe to the left
        ;; (Apple's own example reads it so); fingers moving right go back.
        ((= type +ns-event-type-swipe+)
         (cond ((minusp delta-x) :back)
               ((plusp delta-x) :forward)))))

(defun event-action (event)
  "EVENT-NAVIGATION for an NSEvent.  Asks the event only what its type has:
-buttonNumber of a key press raises an exception."
  (let ((type (objc:invoke event "type")))
    (event-navigation type
                      (if (member type (list +ns-event-type-other-mouse-down+
                                             +ns-event-type-other-mouse-up+))
                          (objc:invoke event "buttonNumber")
                          0)
                      (if (= type +ns-event-type-swipe+)
                          (objc:invoke event "deltaX")
                          0d0))))

(defconstant +ns-event-type-scroll-wheel+ 22)

(defun scroll-action (window event)
  "Feed a scroll EVENT to WINDOW's swipe tracker: :BACK, :FORWARD or NIL.
Only when the system says swipes between pages are made by scrolling."
  (when (objc:invoke-bool "NSEvent" "isSwipeTrackingFromScrollEventsEnabled")
    (track-scroll (window-swipe-tracker window)
                  (objc:invoke event "phase") (objc:invoke event "momentumPhase")
                  (objc:invoke event "scrollingDeltaX") (objc:invoke event "scrollingDeltaY")
                  (objc:invoke-bool event "isDirectionInvertedFromDevice"))))

(objc:define-objc-class browser-window ()
  ((controller :initform nil :accessor window-controller)
   (swipe-tracker :initform (make-swipe-tracker) :accessor window-swipe-tracker))
  (:objc-class-name "GopherBrowserWindow")
  (:objc-superclass-name "NSWindow"))

(objc:define-objc-method ("sendEvent:" :void)
    ((self browser-window) (event objc:objc-object-pointer))
  (let* ((controller (window-controller self))
         (action (and controller
                      (handler-case (event-action event)
                        (error (condition)
                          (note "sendEvent: ~a" condition)
                          nil)))))
    (when (and controller (null action)
               (= 10 (objc:invoke event "type"))       ; NSEventTypeKeyDown
               (handler-case
                   (and (page-has-focus-p controller)
                        (page-key controller (objc:invoke event "keyCode")
                                  (or (objc:invoke-into 'string event
                                                        "charactersIgnoringModifiers")
                                      "")
                                  (objc:invoke event "modifierFlags")))
                 (error (condition) (note "keyDown: ~a" condition) nil)))
      (setf action :ignore))
    ;; Two-finger swipes are scrolls: they go on to the page as ever (it
    ;; never scrolls sideways), and the tracker watches them pass.
    (when (and controller (null action)
               (= +ns-event-type-scroll-wheel+ (objc:invoke event "type")))
      (let ((swipe (handler-case (scroll-action self event)
                     (error (condition) (note "scroll: ~a" condition) nil))))
        (case swipe
          (:back (ignore-errors (go-back controller)))
          (:forward (ignore-errors (go-forward controller))))))
    (case action
      (:back (handler-case (go-back controller)
               (error (condition) (note "Back: ~a" condition))))
      (:forward (handler-case (go-forward controller)
                  (error (condition) (note "Forward: ~a" condition))))
      (:ignore)
      (t (objc:invoke (objc:current-super) "sendEvent:" event)))))

;;; Building it -------------------------------------------------------------------------

(defun symbol-image (name description)
  "The SF Symbol NAME, or NIL on a system without it."
  (let ((image (objc:invoke "NSImage" "imageWithSystemSymbolName:accessibilityDescription:"
                            name description)))
    (if (null-object-p image) (cffi:null-pointer) image)))

;;; The toolbar ----------------------------------------------------------------------------
;;;
;;; Back, Forward, Reload, Home and the address are a real NSToolbar, so the
;;; window has one unified bar along its top and macOS puts its tab bar under
;;; it, as Safari and Finder do.  The items are made with the window and kept by
;;; the controller -- they exist before the window is ever shown, which the
;;; tests rely on -- and the toolbar's delegate (the controller) hands them over
;;; when the toolbar asks.

(defparameter +toolbar-identifiers+ '("back" "forward" "reload" "home" "address"))

(defun make-toolbar-button (identifier symbol label action target)
  "A toolbar item that is a button showing SF Symbol SYMBOL."
  (let ((item (objc:invoke (objc:invoke "NSToolbarItem" "alloc") "initWithItemIdentifier:"
                           identifier)))
    (objc:invoke item "setImage:" (symbol-image symbol label))
    (objc:invoke item "setLabel:" label)
    (objc:invoke item "setToolTip:" label)
    (objc:invoke item "setTarget:" target)
    (objc:invoke item "setAction:" (objc:coerce-to-selector action))
    (objc:invoke item "setBordered:" t)
    ;; Enabled as REFRESH-CONTROLS says, not as AppKit guesses.
    (objc:invoke item "setAutovalidates:" nil)
    ;; Back and Forward go at the leading edge, where macOS keeps navigation.
    (when (and (member identifier '("back" "forward") :test #'string=)
               (objc:can-invoke-p item "setNavigational:"))
      (objc:invoke item "setNavigational:" t))
    item))

(defun make-address-field (target)
  (let ((field (objc:invoke (objc:invoke "NSTextField" "alloc") "initWithFrame:"
                            #(0d0 0d0 480d0 24d0))))
    (objc:invoke field "setPlaceholderString:" "Address: gopher://, gemini://, spartan://, nex://, finger://")
    (objc:invoke field "setTarget:" target)
    (objc:invoke field "setAction:" (objc:coerce-to-selector "go:"))
    ;; For completing addresses as they are typed.
    (objc:invoke field "setDelegate:" target)
    (objc:invoke (objc:invoke field "cell") "setScrollable:" t)
    (objc:autorelease field)))

(defun make-toolbar-items (controller target)
  "The toolbar's items, kept on CONTROLLER, and its buttons and field set
where the rest of the controller looks for them."
  (let* ((back (make-toolbar-button "back" "chevron.left" "Back" "goBack:" target))
         (forward (make-toolbar-button "forward" "chevron.right" "Forward" "goForward:" target))
         (reload (make-toolbar-button "reload" "arrow.clockwise" "Reload" "reload:" target))
         (home (make-toolbar-button "home" "house" "Home" "goHome:" target))
         (field (make-address-field target))
         (address (objc:invoke (objc:invoke "NSToolbarItem" "alloc") "initWithItemIdentifier:"
                               "address")))
    (objc:invoke address "setView:" field)
    (objc:invoke address "setLabel:" "Address")
    ;; As wide as the toolbar lets it be.
    (objc:invoke address "setMinSize:" #(240d0 24d0))
    (objc:invoke address "setMaxSize:" #(4000d0 24d0))
    (objc:invoke address "setVisibilityPriority:" 1000)
    (setf (controller-back-button controller) back
          (controller-forward-button controller) forward
          (controller-reload-button controller) reload
          (controller-address-field controller) field
          (controller-toolbar-items controller)
          (list (cons "back" back) (cons "forward" forward) (cons "reload" reload)
                (cons "home" home) (cons "address" address)))))

(defun identifier-array ()
  (objc:invoke "NSArray" "arrayWithArray:" (coerce +toolbar-identifiers+ 'vector)))

(define-controller-method ("toolbar:itemForItemIdentifier:willBeInsertedIntoToolbar:"
                           objc:objc-object-pointer :on-error (cffi:null-pointer))
    ((toolbar objc:objc-object-pointer) (identifier objc:objc-object-pointer)
     (inserted objc:objc-bool))
  (or (cdr (assoc (ns-string identifier) (controller-toolbar-items self) :test #'equal))
      (cffi:null-pointer)))

(define-controller-method ("toolbarDefaultItemIdentifiers:" objc:objc-object-pointer
                           :on-error (cffi:null-pointer))
    ((toolbar objc:objc-object-pointer))
  (identifier-array))

(define-controller-method ("toolbarAllowedItemIdentifiers:" objc:objc-object-pointer
                           :on-error (cffi:null-pointer))
    ((toolbar objc:objc-object-pointer))
  (identifier-array))

(defun install-toolbar (controller window target)
  (make-toolbar-items controller target)
  (let ((toolbar (objc:invoke (objc:invoke "NSToolbar" "alloc") "initWithIdentifier:"
                              "GopherBrowserToolbar")))
    (objc:invoke toolbar "setDelegate:" target)
    ;; NSToolbarDisplayModeIconOnly: the address field and four icons.
    (objc:invoke toolbar "setDisplayMode:" 2)
    (objc:invoke toolbar "setAllowsUserCustomization:" nil)
    (objc:invoke window "setToolbar:" toolbar)
    (objc:release toolbar))
  ;; NSWindowToolbarStyleUnified, with the title in the tab bar and the
  ;; Window menu rather than taking the toolbar's room, as Safari does.
  (objc:invoke window "setToolbarStyle:" 3)
  (objc:invoke window "setTitleVisibility:" 1))

(defparameter +window-width+ 860d0)
(defparameter +window-height+ 720d0)
(defparameter +status-height+ 26d0)

(defvar *cascade-point* nil
  "Where the last window's top left went, so that the next is offset from it.")

(defun make-browser-controller ()
  "A controller with its window built and not shown, on a new session."
  (let* ((controller (make-instance 'browser-controller :session (make-session)))
         (target (objc:objc-object-pointer controller))
         (width +window-width+)
         (height +window-height+)
         (window (objc:invoke (objc:invoke "GopherBrowserWindow" "alloc")
                              "initWithContentRect:styleMask:backing:defer:"
                              (vector 0d0 0d0 width height)
                              +ns-window-style-mask+ +ns-backing-store-buffered+ t))
         (content (objc:invoke window "contentView")))
    ;; Or the close button frees a window this still points at.
    (objc:invoke window "setReleasedWhenClosed:" nil)
    (objc:invoke window "setTitle:" "Gopher")
    (objc:invoke window "setContentMinSize:" #(420d0 300d0))
    (objc:invoke window "setDelegate:" target)
    ;; Every browser window may be a tab of every other.
    (objc:invoke window "setTabbingIdentifier:" "GopherBrowser")
    ;; So that its -sendEvent: knows whose Back to press.
    (setf (window-controller (objc:objc-object-from-pointer window)) controller)
    (setf (controller-window controller) window)

    ;; The toolbar along the top: Back, Forward, Reload, Home, and the address.
    (install-toolbar controller window target)

    ;; The page, from the toolbar down to the status line.
    (let ((scroll (objc:invoke "NSTextView" "scrollableTextView")))
      (objc:invoke scroll "setFrame:" (vector 0d0 +status-height+ width
                                              (- height +status-height+)))
      (objc:invoke scroll "setAutoresizingMask:" (logior +flexible-width+ +flexible-height+))
      (objc:invoke scroll "setBorderType:" 0)
      (let ((text-view (objc:invoke scroll "documentView")))
        (objc:invoke text-view "setEditable:" nil)
        (objc:invoke text-view "setSelectable:" t)
        (objc:invoke text-view "setRichText:" t)
        (objc:invoke text-view "setImportsGraphics:" nil)
        (objc:invoke text-view "setUsesFindBar:" t)
        (objc:invoke text-view "setIncrementalSearchingEnabled:" t)
        (objc:invoke text-view "setTextContainerInset:" #(24d0 18d0))
        (objc:invoke text-view "setDelegate:" target)
        ;; Links in the link colour, without the underline: a Gopher menu is
        ;; mostly links, and underlining them all is noise.
        (let ((attributes (objc:invoke "NSMutableDictionary" "dictionary")))
          (objc:invoke attributes "setObject:forKey:" (objc:invoke "NSColor" "linkColor")
                       (attribute-key "NSForegroundColorAttributeName"))
          (objc:invoke attributes "setObject:forKey:"
                       (objc:invoke "NSCursor" "pointingHandCursor")
                       (attribute-key "NSCursorAttributeName"))
          (objc:invoke text-view "setLinkTextAttributes:" attributes))
        ;; The pointer over the page, for the address of the link under it.
        ;; MouseEnteredAndExited | MouseMoved | ActiveInKeyWindow | InVisibleRect.
        (let ((area (objc:invoke (objc:invoke "NSTrackingArea" "alloc")
                                 "initWithRect:options:owner:userInfo:"
                                 #(0d0 0d0 0d0 0d0) (logior #x01 #x02 #x20 #x200)
                                 target (cffi:null-pointer))))
          (objc:invoke text-view "addTrackingArea:" area)
          (objc:release area))
        (setf (controller-text-view controller) text-view))
      (objc:invoke content "addSubview:" scroll))

    ;; The status line along the bottom, with the spinner at its end.
    (let ((label (objc:invoke "NSTextField" "labelWithString:" "")))
      (objc:invoke label "setFrame:" (vector 12d0 5d0 (- width 48d0) 16d0))
      (objc:invoke label "setAutoresizingMask:" (logior +flexible-width+ +flexible-top+))
      (objc:invoke label "setFont:" (system-font 11d0))
      (objc:invoke label "setTextColor:" (objc:invoke "NSColor" "secondaryLabelColor"))
      (objc:invoke label "setLineBreakMode:" 4)
      (objc:invoke content "addSubview:" label)
      (setf (controller-status-label controller) label))
    (let ((spinner (objc:invoke (objc:invoke "NSProgressIndicator" "alloc") "initWithFrame:"
                                (vector (- width 28d0) 5d0 16d0 16d0))))
      (objc:invoke spinner "setStyle:" +ns-progress-indicator-style-spinning+)
      (objc:invoke spinner "setControlSize:" 1)
      (objc:invoke spinner "setDisplayedWhenStopped:" nil)
      (objc:invoke spinner "setAutoresizingMask:" (logior +flexible-left+ +flexible-top+))
      (objc:invoke content "addSubview:" spinner)
      (objc:release spinner)
      (setf (controller-spinner controller) spinner))

    (refresh-controls controller)
    (update-layout controller)
    controller))

(defvar *show-new-windows* t
  "Whether a new browser window is put on screen.  The tests turn it off: a
window on screen takes the keyboard from whoever is typing.")

(defun show-window (controller)
  (unless *show-new-windows*
    (return-from show-window controller))
  (let ((window (controller-window controller)))
    (if *cascade-point*
        (setf *cascade-point* (objc:invoke window "cascadeTopLeftFromPoint:" *cascade-point*))
        (progn (let ((frame (saved-window-frame)))
                 (if frame
                     (objc:invoke window "setFrameFromString:" frame)
                     (objc:invoke window "center")))
               (setf *cascade-point* (objc:invoke window "cascadeTopLeftFromPoint:" #(0d0 0d0)))))
    (update-layout controller)
    (objc:invoke window "makeKeyAndOrderFront:" (cffi:null-pointer))
    (objc:invoke window "makeFirstResponder:" (controller-text-view controller)))
  controller)

(defun open-window (&optional url)
  "Open a new browser window on URL, or on the home page."
  (let ((controller (make-browser-controller)))
    (push controller *controllers*)
    (show-window controller)
    (navigate controller (or url (home-page)))
    controller))

(defun open-tab (controller url &key background)
  "Open URL, or the home page, in a new tab beside CONTROLLER's.  BACKGROUND
leaves CONTROLLER's tab in front, as Command-click does."
  (let ((new (make-browser-controller))
        (window (controller-window controller)))
    (push new *controllers*)
    ;; NSWindowAbove: just after this tab.
    (objc:invoke window "addTabbedWindow:ordered:" (controller-window new) 1)
    (update-layout new)
    (if background
        (objc:invoke window "makeKeyAndOrderFront:" (cffi:null-pointer))
        (progn (objc:invoke (controller-window new) "makeKeyAndOrderFront:" (cffi:null-pointer))
               (objc:invoke (controller-window new) "makeFirstResponder:"
                            (controller-address-field new))))
    (navigate new (or url (home-page)))
    new))
