;;;; ui-tests.lisp -- the real window, driven without a click.
;;;;
;;;; The window is built and never shown.  Its controls are sent the messages a
;;;; click would send, the event loop is run until the page has arrived, and
;;;; then the text view is asked what it shows.  Dialogs are answered by
;;;; *DIALOG-ANSWERER*, since no test can click.
;;;;
;;;; Skipped where there is no window server, which the count of skips says.

(in-package #:gopher-browser/tests)

(def-suite ui :in all-tests :description "The browser window.  Needs a window server.")
(in-suite ui)

(defun call-with-controller (function)
  (with-data-directory ()
    (let ((controller nil)
          (gb::*show-downloads-window* nil)
          (gb::*downloads* '()))
      (unwind-protect
           (objc:with-autorelease-pool ()
             ;; No policy, so no Dock icon and no stolen focus.
             (objc.runloop:shared-application :activation-policy nil)
             (gb::reset-text-styles)
             (gb::ensure-main-thread-target)
             (setf controller (gb::make-browser-controller))
             (funcall function controller))
        (when controller
          (gb::stop-loading controller)
          (objc:invoke (gb::controller-window controller) "close"))))))

(defmacro with-controller ((controller) &body body)
  "Run BODY with CONTROLLER a browser window's controller, or skip."
  `(cond ((not (ignore-errors (gb::ensure-frameworks) t))
          (skip "Objective-C runtime not available"))
         ((not (objc.runloop:window-server-p))
          (skip "no window server"))
         (t (call-with-controller (lambda (,controller) ,@body)))))

(defun wait-until-loaded (controller &key (seconds 10))
  "Run the event loop until CONTROLLER has nothing loading.  True if it got there."
  (objc.runloop:pump-events :until (lambda () (not (gb::controller-loading-p controller)))
                            :max-seconds seconds)
  (not (gb::controller-loading-p controller)))

(defun visit (controller url)
  (gb::navigate controller url)
  (is-true (wait-until-loaded controller) "~a did not finish loading" url))

(defun wait-for-downloads (&key (seconds 10))
  "Run the event loop until no download is running.  True if none is."
  (flet ((done () (notany (lambda (d) (eq :running (gb::download-state d))) gb::*downloads*)))
    (objc.runloop:pump-events :until #'done :max-seconds seconds)
    (done)))

(defun shown (controller)
  "The text on show."
  (gb::page-string controller))

(defun address (controller)
  (objc:invoke-into 'string (gb::controller-address-field controller) "stringValue"))

(defun status (controller)
  (objc:invoke-into 'string (gb::controller-status-label controller) "stringValue"))

(defun title (controller)
  (objc:invoke-into 'string (gb::controller-window controller) "title"))

(defun click-link (controller index)
  "What a click on link INDEX does: the text view tells its delegate."
  (objc:invoke-bool (objc:objc-object-pointer controller) "textView:clickedOnLink:atIndex:"
                    (gb::controller-text-view controller)
                    (gb::link-value index) 0))

(defun send-action (controller selector)
  (objc:invoke (objc:objc-object-pointer controller) selector (cffi:null-pointer)))

(defun enabled-p (control)
  (objc:invoke-bool control "isEnabled"))

(defun link-index-of (controller label)
  (position label (gb:page-links (gb::controller-page controller))
            :key #'gb:link-label :test #'string=))

(test the-start-page-shows
  (with-controller (controller)
    (visit controller "about:start")
    (is (search "Floodgap Systems" (shown controller)))
    (is (string= "Gopher" (title controller)))
    (is (string= "about:start" (address controller)))))

(test a-gopher-menu-is-drawn-and-its-links-work
  (with-controller (controller)
    (with-gopher-server (port)
      (visit controller (gopher-url port))
      (is (search "DIR  A menu" (shown controller)))
      (is (search "6 links" (status controller)))
      (is (not (enabled-p (gb::controller-back-button controller))))
      (is-true (click-link controller (link-index-of controller "A menu")))
      (is-true (wait-until-loaded controller))
      (is (search "The submenu" (shown controller)))
      (is (string= (gopher-url port "/1/sub") (address controller)))
      (is (enabled-p (gb::controller-back-button controller)))
      ;; Back, and Forward again.
      (send-action controller "goBack:")
      (is-true (wait-until-loaded controller))
      (is (search "Welcome to the fixture" (shown controller)))
      (is (enabled-p (gb::controller-forward-button controller)))
      (send-action controller "goForward:")
      (is-true (wait-until-loaded controller))
      (is (search "The submenu" (shown controller))))))

(test the-address-field-goes-where-it-says
  (with-controller (controller)
    (with-gopher-server (port)
      (objc:invoke (gb::controller-address-field controller) "setStringValue:"
                   (gopher-url port "/0/hello.txt"))
      (send-action controller "go:")
      (is-true (wait-until-loaded controller))
      (is (search "Hello, Gopher." (shown controller))))))

(test a-search-asks-for-its-terms
  (with-controller (controller)
    (with-gopher-server (port)
      (let ((asked '()))
        (let ((gb::*dialog-answerer* (lambda (kind message)
                                       (push (list kind message) asked)
                                       "lisp")))
          (visit controller (gopher-url port))
          (click-link controller (link-index-of controller "Search"))
          (is-true (wait-until-loaded controller))
          ;; Asked, answered, and fetched again with the answer.
          (is-true (wait-until-loaded controller)))
        (is (equal :text (first (first asked))))
        (is (search "You searched for lisp" (shown controller)))))))

(test cancelling-a-search-stays-put
  (with-controller (controller)
    (with-gopher-server (port)
      (let ((gb::*dialog-answerer* (constantly nil)))
        (visit controller (gopher-url port))
        (click-link controller (link-index-of controller "Search"))
        (is-true (wait-until-loaded controller))
        (is (search "Welcome to the fixture" (shown controller)))
        (is (string= (gopher-url port) (address controller)))))))

(test a-download-is-saved-where-asked
  (with-controller (controller)
    (with-gopher-server (port)
      (let* ((path (namestring (gb::data-file "saved.bin")))
             (gb::*dialog-answerer* (lambda (kind name)
                                      (is (eq :save kind))
                                      (is (string= "file.bin" name))
                                      path)))
        (visit controller (gopher-url port))
        (click-link controller (link-index-of controller "A binary"))
        (is-true (wait-for-downloads))
        (is (= 256 (with-open-file (in path :element-type '(unsigned-byte 8))
                     (file-length in))))
        (is (search "Done, 256 bytes" (status controller)))
        ;; The page stays as it was.
        (is (search "Welcome to the fixture" (shown controller)))))))

(test an-image-is-shown
  (with-controller (controller)
    (with-gopher-server (port)
      (visit controller (gopher-url port "/I/pic.png"))
      ;; One attachment character, and no complaint that it could not be shown.
      (is (= 1 (length (shown controller))))
      (is (= (char-code (char (shown controller) 0)) #xFFFC)))))

(test lines-appear-before-the-page-is-finished
  (with-controller (controller)
    (with-gopher-server (port)
      (gb::navigate controller (gopher-url port "/1/slow"))
      (objc.runloop:pump-events :until (lambda () (search "First, quickly" (shown controller)))
                                :max-seconds 5)
      (is (search "First, quickly" (shown controller)))
      (is (gb::controller-loading-p controller) "the server is still sending")
      (is-true (wait-until-loaded controller))
      (is (search "Then, slowly" (shown controller)))
      ;; Drawn once: what streamed is not drawn again on top.
      (is (= 1 (count #\F (shown controller)))))))

(test stop-stops
  (with-controller (controller)
    (with-gopher-server (port)
      (visit controller "about:start")
      (gb::navigate controller (gopher-url port "/1/stall"))
      (objc.runloop:pump-events :max-seconds 0.3)
      (is (gb::controller-loading-p controller))
      (send-action controller "stopLoading:")
      (is (not (gb::controller-loading-p controller)))
      (is (string= "Stopped." (status controller)))
      ;; Still the page that was there, at its own address.
      (is (search "Floodgap" (shown controller)))
      (is (string= "about:start" (address controller))))))

(test a-failure-is-a-page-saying-so
  (with-controller (controller)
    (let ((port (with-gopher-server (port) port)))
      (visit controller (gopher-url port))
      (is (search "Could not load the page" (shown controller)))
      (is (search "Try again" (shown controller))))))

(test a-changed-certificate-is-trusted-only-when-confirmed
  (with-controller (controller)
    (with-gemini-server (port)
      (g:store-known-host (gb::shared-known-hosts)
                          (g:make-known-host :host "127.0.0.1" :port port
                                             :fingerprint "sha256:00"))
      (let ((gb::*dialog-answerer* (constantly nil)))
        (visit controller (gemini-url port)))
      (is (search "Certificate changed" (shown controller)))
      (let ((gb::*dialog-answerer* (constantly t)))
        (visit controller (gemini-url port))
        (is-true (wait-until-loaded controller)))
      (is (search "Welcome" (shown controller))))))

(test an-identity-is-made-on-request
  (with-controller (controller)
    (with-gemini-server (port)
      (let ((gb::*dialog-answerer* (constantly t)))
        (visit controller (gemini-url port "/identity"))
        (is-true (wait-until-loaded controller)))
      (is (search "with a certificate" (shown controller))))))

(test bookmarks-from-the-window
  (with-controller (controller)
    (with-gemini-server (port)
      (visit controller (gemini-url port))
      (send-action controller "addBookmark:")
      (is (equal (list (gemini-url port)) (mapcar #'gb:bookmark-url (gb:bookmarks))))
      (is (equal '("Welcome") (mapcar #'gb:bookmark-title (gb:bookmarks))))
      (send-action controller "showBookmarks:")
      (is-true (wait-until-loaded controller))
      (is (search "Welcome" (shown controller)))
      (is (string= "about:bookmarks" (address controller))))))

(test a-cross-scheme-redirect-is-followed
  (with-controller (controller)
    (with-gopher-server (gopher-port)
      (with-gemini-server (port)
        (progn
          ;; Set, not bound: the server reads it on a thread of its own.
          (setf *gopher-port-for-redirect* gopher-port)
          (visit controller (gemini-url port "/to-gopher"))
          (is-true (wait-until-loaded controller))
          (is (search "Hello, Gopher." (shown controller)))
          (is (string= (gopher-url gopher-port "/0/hello.txt") (address controller))))))))

(test a-failed-back-leaves-the-history-where-it-was
  (with-controller (controller)
    (let ((first-port nil))
      (with-gopher-server (port)
        (setf first-port port)
        (visit controller (gopher-url port)))
      ;; The first server is gone; the second page comes from another.
      (with-gopher-server (port)
        (visit controller (gopher-url port "/1/sub"))
        ;; Or Back is answered from the cache, server or no server.
        (g:cache-clear (gb::session-cache (gb::controller-session controller)))
        (send-action controller "goBack:")
        (is-true (wait-until-loaded controller))
        (is (search "Could not load the page" (shown controller)))
        ;; Still on the second page as far as the history knows: Back still
        ;; goes to the first, and there is nothing ahead.
        (let ((session (gb::controller-session controller)))
          (is (string= (gopher-url port "/1/sub")
                       (g:history-current (gb:session-history session))))
          (is (string= (gopher-url first-port) (gb:session-back-url session)))
          (is (null (gb:session-forward-url session))))
        (is (enabled-p (gb::controller-back-button controller)))
        (is (not (enabled-p (gb::controller-forward-button controller))))))))

(test too-large-to-show-is-downloaded-instead
  (with-controller (controller)
    (with-gopher-server (port)
      (let ((saved g:*default-max-size*)
            (path (namestring (gb::data-file "hello.txt"))))
        ;; Set, not bound: the fetch reads it on a thread of its own.
        (setf g:*default-max-size* 10)
        (unwind-protect
             (let ((gb::*dialog-answerer* (lambda (kind name)
                                            (is (eq :save kind))
                                            (is (string= "hello.txt" name))
                                            path)))
               (visit controller (gopher-url port "/0/hello.txt"))
               (is-true (wait-for-downloads)))
          (setf g:*default-max-size* saved))
        (is (search "Hello, Gopher." (uiop:read-file-string path)))
        (is (search "Done" (status controller)))))))

;;; The mouse's side buttons --------------------------------------------------------------

(test what-events-mean
  (is (eq :back (gb::event-navigation 25 3 0)))
  (is (eq :forward (gb::event-navigation 25 4 0)))
  (is (eq :ignore (gb::event-navigation 26 3 0)))
  (is (eq :ignore (gb::event-navigation 26 4 0)))
  ;; The middle button, and the others, are not the browser's.
  (is (null (gb::event-navigation 25 2 0)))
  (is (null (gb::event-navigation 26 2 0)))
  ;; deltaX is positive for a swipe to the left: fingers moving right, back.
  (is (eq :back (gb::event-navigation 31 0 -1d0)))
  (is (eq :forward (gb::event-navigation 31 0 1d0)))
  (is (null (gb::event-navigation 31 0 0d0)))
  ;; A left click, a key press.
  (is (null (gb::event-navigation 1 0 0)))
  (is (null (gb::event-navigation 10 0 0))))

(defun mouse-button-event (type button)
  "An NSEvent of TYPE for mouse BUTTON, made the way the window server makes
one: a CGEvent, whose button number can be set, as an NSEvent's cannot."
  (let ((cg-event (cffi:foreign-funcall "CGEventCreate" :pointer (cffi:null-pointer) :pointer)))
    (unwind-protect
         (progn
           (cffi:foreign-funcall "CGEventSetType" :pointer cg-event :uint32 type :void)
           ;; kCGMouseEventButtonNumber
           (cffi:foreign-funcall "CGEventSetIntegerValueField"
                                 :pointer cg-event :uint32 3 :int64 button :void)
           (objc:invoke "NSEvent" "eventWithCGEvent:" cg-event))
      (cffi:foreign-funcall "CFRelease" :pointer cg-event :void))))

(defun press-mouse-button (controller button)
  "Send the window what the mouse sends for a press and release of BUTTON."
  (let ((window (gb::controller-window controller)))
    (objc:invoke window "sendEvent:" (mouse-button-event 25 button))
    (objc:invoke window "sendEvent:" (mouse-button-event 26 button))))

(test the-mouse-back-and-forward-buttons
  (with-controller (controller)
    (with-gopher-server (port)
      (is (= 3 (objc:invoke (mouse-button-event 25 3) "buttonNumber"))
          "the synthesised event carries its button")
      (visit controller (gopher-url port))
      (visit controller (gopher-url port "/1/sub"))
      (press-mouse-button controller 3)
      (is-true (wait-until-loaded controller))
      (is (search "Welcome to the fixture" (shown controller)))
      (is (string= (gopher-url port) (address controller)))
      (press-mouse-button controller 4)
      (is-true (wait-until-loaded controller))
      (is (search "The submenu" (shown controller)))
      ;; Nothing ahead: Forward again does nothing, and loads nothing.
      (press-mouse-button controller 4)
      (is (not (gb::controller-loading-p controller)))
      (is (search "The submenu" (shown controller))))))

(test other-buttons-are-left-alone
  (with-controller (controller)
    (with-gopher-server (port)
      (visit controller (gopher-url port))
      (visit controller (gopher-url port "/1/sub"))
      ;; The middle button reaches AppKit as before, and is not Back.
      (press-mouse-button controller 2)
      (is (not (gb::controller-loading-p controller)))
      (is (search "The submenu" (shown controller))))))

;;; Tabs -------------------------------------------------------------------------------------

(defun tab-windows (controller)
  (let ((group (objc:invoke (gb::controller-window controller) "tabGroup")))
    (if (gb::null-object-p group) '() (gb::ns-array-list (objc:invoke group "windows")))))

(test a-new-tab-joins-the-window
  (with-controller (controller)
    (let ((gb::*controllers* (list controller)))
      (let ((tab (gb::open-tab controller "about:bookmarks" :background t)))
        (unwind-protect
             (progn
               (is-true (wait-until-loaded tab))
               (is (= 2 (length (tab-windows controller))))
               (is (search "Bookmarks" (shown tab)))
               ;; The session saved at quitting is one window of two tabs, in order.
               (let ((groups (gb::current-session-groups)))
                 (is (= 1 (length groups)))
                 (is (equal '("about:start" "about:bookmarks")
                            (mapcar (lambda (url) (if (search "about:" url) url "?"))
                                    (getf (first groups) :urls))))))
          (objc:invoke (gb::controller-window tab) "close"))))))

(test command-click-opens-a-background-tab
  (with-controller (controller)
    (let ((gb::*controllers* (list controller)))
      (visit controller "about:start")
      (gb::follow-link controller 0 :new-window t)
      (let ((tab (find controller gb::*controllers* :test-not #'eq)))
        (unwind-protect
             (progn
               (is (not (null tab)))
               (is (= 2 (length (tab-windows controller))))
               ;; The page clicked on is still this tab's.
               (is (search "Floodgap Systems" (shown controller))))
          (when tab (objc:invoke (gb::controller-window tab) "close")))))))

;;; History --------------------------------------------------------------------------------

(test pages-shown-are-history
  (with-controller (controller)
    (with-gemini-server (port)
      (visit controller (gemini-url port))
      (visit controller (gemini-url port "/two"))
      (is (equal (list (gemini-url port "/two") (gemini-url port))
                 (mapcar #'gb::visit-url (gb::visits))))
      (is (equal '("Two" "Welcome") (mapcar #'gb::visit-title (gb::visits))))
      (send-action controller "showAllHistory:")
      (is-true (wait-until-loaded controller))
      (is (search "Today" (shown controller)))
      (is (search "Welcome" (shown controller))))))

;;; Pointing at links -------------------------------------------------------------------------

(defun link-point (controller index)
  "The middle of link INDEX's first character, in the text view's coordinates."
  (let* ((text-view (gb::controller-text-view controller))
         (layout (objc:invoke text-view "layoutManager"))
         (start (car (aref (gb::controller-link-ranges controller) index)))
         (glyph (objc:invoke layout "glyphIndexForCharacterAtIndex:" start))
         (rect (objc:invoke layout "boundingRectForGlyphRange:inTextContainer:"
                            (cons glyph 1) (objc:invoke text-view "textContainer")))
         (origin (objc:invoke text-view "textContainerOrigin")))
    (vector (+ (aref origin 0) (aref rect 0) (/ (aref rect 2) 2))
            (+ (aref origin 1) (aref rect 1) (/ (aref rect 3) 2)))))

(test pointing-at-a-link-shows-where-it-goes
  (with-controller (controller)
    (with-gopher-server (port)
      (visit controller (gopher-url port))
      (let ((index (link-index-of controller "A menu")))
        (is (eql index (gb::link-at-point controller (link-point controller index))))
        ;; Far to the right of the line's end is not on the link.
        (let ((beside (link-point controller index)))
          (setf (aref beside 0) (+ (aref beside 0) 600d0))
          (is (null (gb::link-at-point controller beside))))
        (gb::hover controller index)
        (is (string= (gopher-url port "/1/sub") (status controller)))
        ;; What the status line said before comes back.
        (gb::hover controller nil)
        (is (search "links" (status controller)))
        ;; A status said while pointing waits until the pointer leaves.
        (gb::hover controller index)
        (gb::set-status controller "Something else")
        (is (string= (gopher-url port "/1/sub") (status controller)))
        (gb::hover controller nil)
        (is (string= "Something else" (status controller)))))))

;;; Text size and the column ------------------------------------------------------------------

(defun font-size-at (controller index)
  (objc:invoke (objc:invoke (gb::text-storage controller) "attribute:atIndex:effectiveRange:"
                            (gb::attribute-key "NSFontAttributeName") index (cffi:null-pointer))
               "pointSize"))

(test text-size-changes-and-is-kept
  (with-controller (controller)
    (visit controller "about:start")
    (let ((before (font-size-at controller 0)))
      (send-action controller "makeTextBigger:")
      (is (> (font-size-at controller 0) before))
      (is (= 1.1d0 (gb::text-scale)))
      (send-action controller "makeTextSmaller:")
      (send-action controller "makeTextSmaller:")
      (is (< (font-size-at controller 0) before))
      (send-action controller "makeTextStandardSize:")
      (is (= before (font-size-at controller 0)))
      ;; The page is the same page, links and all.
      (is (search "Floodgap Systems" (shown controller)))
      (is (eql 0 (gb::link-at-point controller (link-point controller 0)))))))

(test the-text-is-a-column-in-a-wide-window
  (with-controller (controller)
    (let ((window (gb::controller-window controller)))
      (objc:invoke window "setContentSize:" #(1600d0 800d0))
      (gb::update-layout controller)
      (let ((wide (aref (objc:invoke (gb::controller-text-view controller) "textContainerInset") 0)))
        (is (> wide 400d0) "a wide window has wide margins")
        (objc:invoke window "setContentSize:" #(500d0 800d0))
        (gb::update-layout controller)
        (is (= gb::+least-margin+
               (aref (objc:invoke (gb::controller-text-view controller) "textContainerInset") 0)))))))

;;; The keyboard on the page ----------------------------------------------------------------------

(defun key (controller key-code &key (characters "") (modifiers 0))
  (gb::page-key controller key-code characters modifiers))

(test tab-moves-between-links-and-return-follows
  (with-controller (controller)
    (with-gopher-server (port)
      (visit controller (gopher-url port))
      (is-true (key controller gb::+key-tab+))
      (is (eql 0 (gb::controller-focused-link controller)))
      (is (string= (gopher-url port "/0/hello.txt") (status controller)))
      ;; The link's text is selected.
      (is (equal (aref (gb::controller-link-ranges controller) 0)
                 (objc:invoke (gb::controller-text-view controller) "selectedRange")))
      (key controller gb::+key-tab+)
      (is (eql 1 (gb::controller-focused-link controller)))
      ;; Shift-Tab back, and round from the first to the last.
      (key controller gb::+key-tab+ :modifiers gb::+modifier-shift+)
      (key controller gb::+key-tab+ :modifiers gb::+modifier-shift+)
      (is (eql 5 (gb::controller-focused-link controller)))
      (key controller gb::+key-tab+)
      (key controller gb::+key-tab+)
      (is-true (key controller gb::+key-return+))
      (is-true (wait-until-loaded controller))
      (is (search "The submenu" (shown controller))))))

(test typing-a-number-follows-that-link
  (with-controller (controller)
    (with-gopher-server (port)
      (visit controller (gopher-url port))
      (is-true (key controller 19 :characters "2"))
      (is (search "Link 2" (status controller)))
      (key controller gb::+key-return+)
      (is-true (wait-until-loaded controller))
      (is (search "The submenu" (shown controller)))
      ;; No such link.
      (visit controller (gopher-url port))
      (key controller 25 :characters "9")
      (key controller 25 :characters "9")
      (key controller gb::+key-return+)
      (is (search "no link 99" (status controller)))
      (is (not (gb::controller-loading-p controller)))
      ;; Escape forgets the digits; keys with Command are the menu's.
      (key controller 19 :characters "2")
      (is-true (key controller gb::+key-escape+))
      (is (string= "" (gb::controller-typed-number controller)))
      (is (null (key controller 19 :characters "2"
                                   :modifiers gb::+ns-event-modifier-flag-command+))))))

(test link-numbers-are-drawn-when-asked
  (with-controller (controller)
    (with-gopher-server (port)
      (visit controller (gopher-url port))
      (is (not (search "[1]" (shown controller))))
      (send-action controller "toggleLinkNumbers:")
      (is (search "[1] A text file" (shown controller)))
      (is (search "[2] A menu" (shown controller)))
      ;; The links are where they are said to be, numbers and all.
      (let ((range (aref (gb::controller-link-ranges controller) 1)))
        (is (string= "A menu" (subseq (shown controller) (car range)
                                      (+ (car range) (cdr range))))))
      (send-action controller "toggleLinkNumbers:")
      (is (not (search "[1]" (shown controller)))))))

(test link-ranges-count-as-nsstring-does
  (with-controller (controller)
    ;; A link after an emoji: two UTF-16 units, one Lisp character.
    (let ((page (gb:message-page "about:x" "Title" (format nil "Smile ~a" (code-char #x1F600))
                                 '(:link "gopher://h/" "Here"))))
      (gb::show-page controller page)
      (let ((range (aref (gb::controller-link-ranges controller) 0)))
        (is (= 4 (cdr range)))
        (is (string= "Here" (objc:invoke-into 'string
                                              (objc:invoke (gb::text-storage controller) "string")
                                              "substringWithRange:" range)))))))

(test keys-reach-the-page-through-the-window
  (with-controller (controller)
    (with-gopher-server (port)
      (visit controller (gopher-url port))
      (let ((window (gb::controller-window controller)))
        (objc:invoke window "makeFirstResponder:" (gb::controller-text-view controller))
        (objc:invoke window "sendEvent:"
                     (objc:invoke "NSEvent"
                                  "keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:"
                                  10 #(0d0 0d0) 0 0d0 (objc:invoke window "windowNumber")
                                  (cffi:null-pointer) (string #\Tab) (string #\Tab) nil
                                  gb::+key-tab+))
        (is (eql 0 (gb::controller-focused-link controller)))))))

;;; The address bar ---------------------------------------------------------------------------

(test the-address-bar-completes-and-searches
  (with-controller (controller)
    (with-gopher-server (port)
      (visit controller (gopher-url port "/1/sub"))
      (let* ((window (gb::controller-window controller))
             (field (gb::controller-address-field controller)))
        (objc:invoke window "makeFirstResponder:" field)
        (let ((editor (objc:invoke window "fieldEditor:forObject:" t field)))
          (objc:invoke editor "setString:" "")
          (objc:invoke editor "insertText:" "127.0.0.1")
          (is (string= (format nil "127.0.0.1:~d/1/sub" port)
                       (objc:invoke-into 'string editor "string")))
          (is (equal (cons 9 (length (format nil ":~d/1/sub" port)))
                     (objc:invoke editor "selectedRange")))
          ;; Return goes where the completion leads.
          (is (string= (gopher-url port "/1/sub") (gb::address-field-target controller)))
          ;; Deleting the completed part does not bring it back.
          (objc:invoke editor "deleteBackward:" (cffi:null-pointer))
          (is (string= "127.0.0.1" (objc:invoke-into 'string editor "string")))))
      (objc:invoke (gb::controller-address-field controller) "setStringValue:" "common lisp")
      (setf (gb::controller-completion controller) nil)
      (is (string= "gemini://kennedy.gemi.dev/search?common%20lisp"
                   (gb::address-field-target controller))))))

;;; Bookmarks and downloads windows --------------------------------------------------------

(defun bookmarks-cell (key row)
  (let ((controller (gb::ensure-bookmarks-controller)))
    (objc:invoke-into 'string (objc:objc-object-pointer controller)
                      "tableView:objectValueForTableColumn:row:"
                      (gb::bookmarks-table controller)
                      (objc:invoke (gb::bookmarks-table controller) "tableColumnWithIdentifier:" key)
                      row)))

(test the-bookmarks-window-edits
  (with-controller (controller)
    (let ((gb::*bookmarks-controller* nil))
      (gb:add-bookmark "One" "gopher://one/")
      (gb:add-bookmark "Two" "gopher://two/")
      (let* ((bookmarks (gb::ensure-bookmarks-controller))
             (target (objc:objc-object-pointer bookmarks))
             (table (gb::bookmarks-table bookmarks)))
        (unwind-protect
             (progn
               (gb::refresh-bookmarks-window)
               (is (= 2 (objc:invoke table "numberOfRows")))
               (is (string= "Two" (bookmarks-cell "title" 1)))
               (is (string= "gopher://two/" (bookmarks-cell "url" 1)))
               ;; Renamed, as an edit in the table renames.
               (objc:invoke target "tableView:setObjectValue:forTableColumn:row:"
                            table "Deux" (objc:invoke table "tableColumnWithIdentifier:" "title") 1)
               (is (string= "Deux" (gb:bookmark-title (second (gb:bookmarks)))))
               ;; Moved up, and still selected.
               (gb::select-table-row table 1)
               (objc:invoke target "moveBookmarkUp:" (cffi:null-pointer))
               (is (equal '("Deux" "One") (mapcar #'gb:bookmark-title (gb:bookmarks))))
               (is (= 0 (objc:invoke table "selectedRow")))
               (objc:invoke target "deleteBookmark:" (cffi:null-pointer))
               (is (equal '("One") (mapcar #'gb:bookmark-title (gb:bookmarks)))))
          (objc:invoke (gb::bookmarks-window bookmarks) "close"))))))

(test the-downloads-window-lists-downloads
  (with-controller (controller)
    (with-gopher-server (port)
      (let* ((gb::*downloads-controller* nil)
             (path (namestring (gb::data-file "file.bin")))
             (gb::*dialog-answerer* (constantly path)))
        (visit controller (gopher-url port))
        (click-link controller (link-index-of controller "A binary"))
        (is-true (wait-for-downloads))
        ;; The page went on loading nothing: a download is not the page's.
        (is (not (gb::controller-loading-p controller)))
        (let* ((downloads (gb::ensure-downloads-controller))
               (table (gb::downloads-table downloads)))
          (unwind-protect
               (flet ((cell (key)
                        (objc:invoke-into 'string (objc:objc-object-pointer downloads)
                                          "tableView:objectValueForTableColumn:row:"
                                          table
                                          (objc:invoke table "tableColumnWithIdentifier:" key)
                                          0)))
                 (gb::refresh-downloads-window)
                 (is (= 1 (objc:invoke table "numberOfRows")))
                 (is (string= "file.bin" (cell "name")))
                 (is (string= "Done, 256 bytes" (cell "status")))
                 (objc:invoke (objc:objc-object-pointer downloads) "clearDownloads:"
                              (cffi:null-pointer))
                 (is (= 0 (objc:invoke table "numberOfRows"))))
            (objc:invoke (gb::downloads-window downloads) "close")))))))

(test a-download-can-be-stopped
  (with-controller (controller)
    (with-gopher-server (port)
      (let ((path (namestring (gb::data-file "stalled.bin"))))
        (gb::start-download controller (gopher-url port "/9/stall") path)
        (objc.runloop:pump-events :max-seconds 0.3)
        (let ((download (first gb::*downloads*)))
          (is (eq :running (gb::download-state download)))
          (is-true (gb::stop-download download))
          (is-true (wait-for-downloads))
          (is (eq :stopped (gb::download-state download)))
          (is (null (probe-file path))))))))

;;; Gopher+, Titan, Page Info, View Source and Settings in the window -------------------------

(test a-gopher-plus-form-in-the-window
  (with-controller (controller)
    (with-gopher-server (port)
      (let ((asked nil))
        (let ((gb::*dialog-answerer*
                (lambda (kind fields)
                  (setf asked (list kind (mapcar #'gb::field-kind fields)
                                    (gb::field-initial (first fields))))
                  ;; One value per field: the note has none.
                  (list "Ada" "red" (format nil "first~%second") nil))))
          (visit controller (gopher-url port "/1/plus"))
          (click-link controller (link-index-of controller "Survey"))
          (is-true (wait-until-loaded controller))
          (is-true (wait-until-loaded controller)))
        (is (equal '(:gopher-plus-form (:text :choice :lines :note) "Anonymous") asked))
        (is (search "You said: +-1|Ada|red|2|first|second" (shown controller)))))))

(test the-link-menu
  (with-controller (controller)
    (with-gopher-server (port)
      (visit controller (gopher-url port "/1/plus"))
      (flet ((titles (link-index)
               (let ((menu (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" "")))
                 (objc:invoke (objc:objc-object-pointer controller)
                              "textView:menu:forEvent:atIndex:"
                              (gb::controller-text-view controller) menu (cffi:null-pointer)
                              (car (aref (gb::controller-link-ranges controller) link-index)))
                 (prog1 (loop for i below (objc:invoke menu "numberOfItems")
                              for title = (objc:invoke-into 'string (objc:invoke menu "itemAtIndex:" i)
                                                            "title")
                              when (plusp (length title)) collect title)
                   (objc:release menu)))))
        (let ((titles (titles 0)))
          (is (equal "Open Link in New Tab" (first titles)))
          (is (member "Copy Link Address" titles :test #'string=))
          (is (member "Item Information (Gopher+)" titles :test #'string=))))
      (visit controller (gopher-url port))
      (let ((menu (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" "")))
        (objc:invoke (objc:objc-object-pointer controller) "textView:menu:forEvent:atIndex:"
                     (gb::controller-text-view controller) menu (cffi:null-pointer)
                     (car (aref (gb::controller-link-ranges controller) 0)))
        (is (null (loop for i below (objc:invoke menu "numberOfItems")
                        when (string= "Item Information (Gopher+)"
                                      (objc:invoke-into 'string (objc:invoke menu "itemAtIndex:" i)
                                                        "title"))
                          collect i))
            "not for an item that is not Gopher+")
        (objc:release menu)))))

(test page-info-and-the-tls-mark
  (with-controller (controller)
    (with-gemini-server (port)
      (visit controller (gemini-url port))
      (is (search "Encrypted (TLS)" (status controller)))
      (let ((said nil))
        (let ((gb::*dialog-answerer* (lambda (kind text) (setf said (list kind text)))))
          (send-action controller "showPageInfo:"))
        (is (eq :info (first said)))
        (is (search "Certificate: sha256:" (second said)))))))

(test view-source-opens-a-tab
  (with-controller (controller)
    (let ((gb::*controllers* (list controller)))
      (with-gemini-server (port)
        (visit controller (gemini-url port))
        (send-action controller "viewSource:")
        (let ((tab (find controller gb::*controllers* :test-not #'eq)))
          (unwind-protect
               (progn
                 (is-true (wait-until-loaded tab))
                 (is (search "=> /two The second page" (shown tab)))
                 (is (string= (concatenate 'string "view-source:" (gemini-url port))
                              (address tab))))
            (when tab (objc:invoke (gb::controller-window tab) "close"))))))))

(test uploading-from-the-window
  (with-controller (controller)
    (with-gemini-server (port)
      (setf *last-upload* nil)
      (visit controller (gemini-url port "/two"))
      (let ((offered nil))
        (let ((gb::*dialog-answerer*
                (lambda (kind fields)
                  (setf offered (list kind (mapcar #'gb::field-initial fields)))
                  (list (format nil "titan://127.0.0.1:~d/two" port) "text/gemini" ""
                        (format nil "# Two, edited~%New words.~%") ""))))
          (send-action controller "editPageWithTitan:")
          (is-true (wait-until-loaded controller)))
        (is (eq :upload (first offered)))
        ;; Offered: the page's titan:// address, and its own text to edit.
        (is (string= (format nil "titan://127.0.0.1:~d/two" port) (first (second offered))))
        (is (search "The second page." (fourth (second offered)))))
      (is (search "New words." (shown controller)))
      (is (string= (gemini-url port "/uploaded") (address controller))))))

(test settings-are-asked-and-kept
  (with-controller (controller)
    (let* ((offered nil)
           (gb::*dialog-answerer*
             (lambda (kind fields)
               (when (eq kind :settings)
                 (setf offered (mapcar #'gb::field-initial fields))
                 (list "gopher://home.example/" "Veronica-2" t "45"
                       "socks5://127.0.0.1:9050" nil nil)))))
      (is-true (gb::edit-settings))
      ;; What was offered: what was set before.
      (is (equal '("about:start" "Kennedy" nil "30" "") (subseq offered 0 5)))
      (is (string= "gopher://home.example/" (gb:home-page)))
      (is (eq :veronica (gb::search-engine)))
      (is (gb::link-numbers-p))
      (is (= 45 (gb::request-timeout)))
      (is (string= "socks5://127.0.0.1:9050" (gb::proxy)))
      (is (not (gb::gopher-tls-p))))
    (dolist (bad (list (list "about:start" "Kennedy" nil "45" "http://nope" nil t)
                       (list "about:start" "Kennedy" nil "a while" "" nil t)
                       (list "about:start" "Kennedy" nil "9999" "" nil t)))
      (let* ((said nil)
             (gb::*dialog-answerer*
               (lambda (kind thing)
                 (case kind
                   (:settings bad)
                   (:info (setf said thing) nil)))))
        (is (not (gb::edit-settings)))
        (is (plusp (length said)))
        ;; Nothing kept from a refused form.
        (is (string= "socks5://127.0.0.1:9050" (gb::proxy)))
        (is (= 45 (gb::request-timeout)))
        (is (eq :veronica (gb::search-engine)))))))

(test reopening-a-closed-tab
  (with-controller (controller)
    (let ((gb::*controllers* (list controller))
          (gb::*closed-pages* '()))
      (visit controller "about:start")
      (let ((tab (gb::open-tab controller "about:bookmarks" :background t)))
        (is-true (wait-until-loaded tab))
        (objc:invoke (gb::controller-window tab) "close")
        (is (equal '("about:bookmarks") gb::*closed-pages*))
        (send-action controller "reopenClosedTab:")
        (let ((again (find-if (lambda (c) (and (not (eq c controller)) (not (eq c tab))))
                              gb::*controllers*)))
          (unwind-protect
               (progn (is-true (wait-until-loaded again))
                      (is (search "Bookmarks" (shown again)))
                      (is (null gb::*closed-pages*)))
            (when again (objc:invoke (gb::controller-window again) "close"))))))))

(test searching-the-history-from-its-page
  (with-controller (controller)
    (gb::record-visit "gemini://geminiprotocol.net/" "Project Gemini")
    (gb::record-visit "gopher://gopher.floodgap.com/" "Floodgap Systems")
    (visit controller "about:history")
    (let ((gb::*dialog-answerer* (constantly "floodgap")))
      (click-link controller (position :input (gb:page-links (gb::controller-page controller))
                                       :key #'gb:link-kind))
      (is-true (wait-until-loaded controller)))
    (is (search "1 page with “floodgap”" (shown controller)))
    (is (not (search "Project Gemini" (shown controller))))
    (is (string= "about:history?floodgap" (address controller)))))

(test back-from-a-search-result-goes-to-the-results
  ;; Reported: search, follow a result, Back -- and the search asked again.
  (with-controller (controller)
    (with-gopher-server (port)
      (let ((asked 0))
        (let ((gb::*dialog-answerer* (lambda (kind message)
                                       (declare (ignore kind message))
                                       (incf asked)
                                       "lisp")))
          (visit controller (gopher-url port))
          (click-link controller (link-index-of controller "Search"))
          (is-true (wait-until-loaded controller))
          (is-true (wait-until-loaded controller))
          (is (search "You searched for lisp" (shown controller)))
          ;; The results are at an address that searches again.
          (is (string= (gopher-url port "/7/search%09lisp") (address controller)))
          (visit controller (gopher-url port "/1/sub"))
          (send-action controller "goBack:")
          (is-true (wait-until-loaded controller))
          (is (search "You searched for lisp" (shown controller)))
          (send-action controller "goBack:")
          (is-true (wait-until-loaded controller))
          (is (search "Welcome to the fixture" (shown controller))))
        (is (= 1 asked) "asked once, when searching, and never again")))))

;;; Trusted certificates and identities, in their windows -------------------------------------

(defun table-cell (controller table key row)
  (objc:invoke-into 'string (objc:objc-object-pointer controller)
                    "tableView:objectValueForTableColumn:row:"
                    table (objc:invoke table "tableColumnWithIdentifier:" key) row))

(test the-trusted-certificates-window
  (with-controller (controller)
    (with-gemini-server (port)
      (visit controller (gemini-url port))
      (let* ((gb::*certificates-controller* nil)
             (certificates (gb::ensure-certificates-controller))
             (table (gb::certificates-table certificates)))
        (unwind-protect
             (progn
               (gb::refresh-certificates-window)
               (is (= 1 (objc:invoke table "numberOfRows")))
               (is (string= "127.0.0.1" (table-cell certificates table "host" 0)))
               (is (string= (princ-to-string port) (table-cell certificates table "port" 0)))
               (is (search "sha256:" (table-cell certificates table "fingerprint" 0)))
               ;; Forget it, and say no first.
               (gb::select-table-row table 0)
               (let ((gb::*dialog-answerer* (constantly nil)))
                 (objc:invoke (objc:objc-object-pointer certificates) "forgetCertificate:"
                              (cffi:null-pointer)))
               (is (= 1 (objc:invoke table "numberOfRows")))
               (let ((gb::*dialog-answerer* (constantly t)))
                 (objc:invoke (objc:objc-object-pointer certificates) "forgetCertificate:"
                              (cffi:null-pointer)))
               (is (= 0 (objc:invoke table "numberOfRows")))
               (is (null (g:lookup-known-host (gb::shared-known-hosts) "127.0.0.1" port))))
          (objc:invoke (gb::certificates-window certificates) "close"))))))

(test the-identities-window
  (with-controller (controller)
    (let* ((gb::*identities-controller* nil)
           (identities (gb::ensure-identities-controller))
           (target (objc:objc-object-pointer identities))
           (table (gb::identities-table identities)))
      (unwind-protect
           (progn
             (let ((gb::*dialog-answerer*
                     (lambda (kind fields)
                       (declare (ignore fields))
                       (when (eq kind :new-identity)
                         (list "example.org" "/private" "Me, privately")))))
               (objc:invoke target "newIdentity:" (cffi:null-pointer)))
             (is (= 1 (objc:invoke table "numberOfRows")))
             (is (string= "Me, privately" (table-cell identities table "name" 0)))
             (is (string= "/private" (table-cell identities table "path" 0)))
             (is (search "sha256:" (table-cell identities table "fingerprint" 0)))
             ;; Renamed and rescoped in the table.
             (objc:invoke target "tableView:setObjectValue:forTableColumn:row:"
                          table "Me" (objc:invoke table "tableColumnWithIdentifier:" "name") 0)
             (objc:invoke target "tableView:setObjectValue:forTableColumn:row:"
                          table "everywhere" (objc:invoke table "tableColumnWithIdentifier:" "path") 0)
             (is (string= "Me" (gb::identity-name (first (gb::identities)))))
             (is (string= "/everywhere" (gb::identity-path (first (gb::identities)))))
             (gb::select-table-row table 0)
             (let ((gb::*dialog-answerer* (constantly t)))
               (objc:invoke target "deleteIdentity:" (cffi:null-pointer)))
             (is (= 0 (objc:invoke table "numberOfRows"))))
        (objc:invoke (gb::identities-window identities) "close")))))

;;; What had no test: the form window itself, menus, and links from elsewhere -----------------

(test the-form-window-is-built-and-read
  (with-controller (controller)
    (let* ((fields (list (gb::make-field :text "Name" :value "Ada")
                         (gb::make-field :password "Secret")
                         (gb::make-field :lines "Lines" :value (format nil "one~%two"))
                         (gb::make-field :choice "Colour" :choices '("red" "green") :value "green")
                         (gb::make-field :check "Agreed" :value t)
                         (gb::make-field :note "A note.")
                         (gb::make-field :file "Send")
                         (gb::make-field :save-file "Keep")))
           (target (make-instance 'gb::form-target))
           (window (progn (setf (gb::form-fields target) (coerce fields 'vector))
                          (gb::build-form-window "A form" fields
                                                 (objc:objc-object-pointer target)))))
      (unwind-protect
           (progn
             (is (string= "A form" (objc:invoke-into 'string window "title")))
             (is (equal '("Ada" "" "one
two" "green" t nil "" "") (mapcar #'gb::field-value fields)))
             ;; Typed into, as a person would.
             (objc:invoke (gb::field-control (second fields)) "setStringValue:" "hunter2")
             (objc:invoke (gb::field-control (fourth fields)) "selectItemWithTitle:" "red")
             (objc:invoke (gb::field-control (fifth fields)) "setState:" 0)
             (is (equal '("Ada" "hunter2" "one
two" "red" nil nil "" "") (mapcar #'gb::field-value fields)))
             ;; The secret field hides what is typed.
             (is (string= "NSSecureTextField"
                          (objc:objc-class-name (objc:invoke (gb::field-control (second fields))
                                                             "class")))))
        (objc:release window)))))

(defun menu-item (action &key (tag 0))
  (let ((item (objc:invoke (objc:invoke "NSMenuItem" "alloc")
                           "initWithTitle:action:keyEquivalent:" "x"
                           (objc:coerce-to-selector action) "")))
    (objc:invoke item "setTag:" tag)
    (objc:autorelease item)))

(defun enabled-for (target action &key (tag 0))
  (objc:invoke-bool target "validateMenuItem:" (menu-item action :tag tag)))

(test menu-items-are-enabled-when-they-can-act
  (with-controller (controller)
    (with-gopher-server (port)
      (let ((target (objc:objc-object-pointer controller))
            (gb::*closed-pages* '()))
        (visit controller (gopher-url port))
        (is (not (enabled-for target "goBack:")))
        (is (not (enabled-for target "goForward:")))
        (is (not (enabled-for target "stopLoading:")))
        (is (not (enabled-for target "reopenClosedTab:")))
        (is (enabled-for target "addBookmark:"))
        (is (enabled-for target "viewSource:"))
        (is (not (enabled-for target "editPageWithTitan:")) "Gopher pages are not Titan's")
        (visit controller (gopher-url port "/1/sub"))
        (is (enabled-for target "goBack:"))
        (setf gb::*closed-pages* (list "about:start"))
        (is (enabled-for target "reopenClosedTab:"))
        (gb::navigate controller (gopher-url port "/1/stall"))
        (is (enabled-for target "stopLoading:"))
        (gb::stop-loading controller)
        (visit controller "about:start")
        (is (not (enabled-for target "viewSource:")) "the browser's own pages have no source")
        ;; Link numbers are a ticked item.
        (let ((item (menu-item "toggleLinkNumbers:")))
          (objc:invoke target "validateMenuItem:" item)
          (is (= 0 (objc:invoke item "state")))
          (gb::set-link-numbers t)
          (objc:invoke target "validateMenuItem:" item)
          (is (= 1 (objc:invoke item "state"))))))))

(test the-search-engine-menu-ticks-the-one-in-use
  (with-controller (controller)
    (let ((delegate (objc:objc-object-pointer (make-instance 'gb::application-delegate))))
      (flet ((ticked ()
               (loop for tag from 1 to (length gb::+search-engines+)
                     for item = (menu-item "setSearchEngine:" :tag tag)
                     do (objc:invoke delegate "validateMenuItem:" item)
                     collect (objc:invoke item "state"))))
        (is (equal '(1 0 0) (ticked)))
        (objc:invoke delegate "setSearchEngine:" (menu-item "setSearchEngine:" :tag 3))
        (is (eq :veronica (gb::search-engine)))
        (is (equal '(0 0 1) (ticked)))))))

(test links-from-other-applications-open-windows
  (with-controller (controller)
    (with-gopher-server (port)
      (let ((gb::*controllers* '())
            (gb::*show-new-windows* nil)
            (delegate (objc:objc-object-pointer (make-instance 'gb::application-delegate))))
        (objc:invoke delegate "application:openURLs:" (cffi:null-pointer)
                     (vector (objc:invoke "NSURL" "URLWithString:" (gopher-url port))
                             (objc:invoke "NSURL" "URLWithString:" (gopher-url port "/1/sub"))))
        (unwind-protect
             (progn
               (is (= 2 (length gb::*controllers*)))
               (dolist (c gb::*controllers*) (is-true (wait-until-loaded c)))
               (is (equal (list (gopher-url port) (gopher-url port "/1/sub"))
                          (sort (mapcar #'gb::current-url gb::*controllers*) #'string<))))
          (dolist (c gb::*controllers*)
            (objc:invoke (gb::controller-window c) "close")))))))

(test the-controls-are-a-toolbar
  ;; So that the tab bar goes under them, as in Safari.
  (with-controller (controller)
    (let* ((window (gb::controller-window controller))
           (toolbar (objc:invoke window "toolbar"))
           (items (gb::ns-array-list (objc:invoke toolbar "items"))))
      (is (equal '("back" "forward" "reload" "home" "address")
                 (mapcar (lambda (item) (objc:invoke-into 'string item "itemIdentifier")) items)))
      (is (cffi:pointer-eq (gb::controller-address-field controller)
                           (objc:invoke (fifth items) "view")))
      (is (cffi:pointer-eq (gb::controller-back-button controller) (first items)))
      ;; NSWindowToolbarStyleUnified.
      (is (= 3 (objc:invoke window "toolbarStyle")))
      ;; Back is off with nowhere to go; the controller turns it on.
      (is (not (enabled-p (gb::controller-back-button controller)))))))
