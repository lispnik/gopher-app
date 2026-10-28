;;;; app.lisp -- the application: its delegate, its menus, and MAIN.

(in-package #:gopher-browser)

(defvar *application-delegate* nil)
(defvar *bookmarks-menu* nil
  "The Bookmarks menu, rebuilt whenever the bookmarks change.")
(defvar *history-menu* nil
  "The History menu, rebuilt whenever a page is visited.")
(defparameter +history-menu-length+ 15)
(defconstant +command+ (ash 1 20))
(defconstant +option+ (ash 1 19))
(defconstant +shift+ (ash 1 17))

(objc:define-objc-class application-delegate ()
  ()
  (:objc-class-name "GopherBrowserApplicationDelegate"))

(defmacro define-delegate-method ((selector result-type &key on-error)
                                  (&rest arguments) &body body)
  `(objc:define-objc-method (,selector ,result-type)
       ((self application-delegate) ,@arguments)
     (declare (ignorable self ,@(mapcar #'first arguments)))
     (handler-case (progn ,@body)
       (error (condition)
         (note "~a: ~a" ,selector condition)
         ,on-error))))

;;; A browser stays open with no windows, as browsers on the Mac do.
(define-delegate-method ("applicationShouldTerminateAfterLastWindowClosed:" objc:objc-bool)
    ((application objc:objc-object-pointer))
  nil)

;;; A click on the Dock icon with no window open opens one.
(define-delegate-method ("applicationShouldHandleReopen:hasVisibleWindows:" objc:objc-bool
                         :on-error t)
    ((application objc:objc-object-pointer) (visible objc:objc-bool))
  (when (member visible (quote (0 nil)))
    (open-window))
  t)

;;; A gopher:, gemini: ... link clicked in another application.
(define-delegate-method ("application:openURLs:" :void)
    ((application objc:objc-object-pointer) (urls objc:objc-object-pointer))
  (loop for index below (objc:invoke urls "count")
        for url = (objc:invoke-into 'string (objc:invoke urls "objectAtIndex:" index)
                                    "absoluteString")
        when url do (open-window url)))

;;; These reach the delegate only when no browser window is key: with one, its
;;; controller answers first.
(define-delegate-method ("newWindow:" :void) ((sender objc:objc-object-pointer))
  (open-window))

(define-delegate-method ("openLocation:" :void) ((sender objc:objc-object-pointer))
  (let ((controller (open-window)))
    (objc:invoke (controller-window controller) "makeFirstResponder:"
                 (controller-address-field controller))))

(define-delegate-method ("goHome:" :void) ((sender objc:objc-object-pointer))
  (open-window))

(define-delegate-method ("showBookmarks:" :void) ((sender objc:objc-object-pointer))
  (open-window "about:bookmarks"))

(define-delegate-method ("openBookmark:" :void) ((sender objc:objc-object-pointer))
  (let ((url (bookmark-url-for-item sender)))
    (when url (open-window url))))

(define-delegate-method ("newTab:" :void) ((sender objc:objc-object-pointer))
  (open-window))

(define-delegate-method ("reopenClosedTab:" :void) ((sender objc:objc-object-pointer))
  (let ((url (pop *closed-pages*)))
    (when url (open-window url))))

(define-delegate-method ("showAllHistory:" :void) ((sender objc:objc-object-pointer))
  (open-window "about:history"))

(define-delegate-method ("openHistoryItem:" :void) ((sender objc:objc-object-pointer))
  (let ((url (history-url-for-item sender)))
    (when url (open-window url))))

;;; These are the delegate's alone: they are about the whole application.

(define-delegate-method ("clearHistory:" :void) ((sender objc:objc-object-pointer))
  (when (confirm "Clear the history?"
                 "Every page visited is forgotten.  Bookmarks, and each window's Back and Forward, are kept."
                 "Clear History")
    (clear-visits)
    (rebuild-history-menu)))

(define-delegate-method ("editBookmarks:" :void) ((sender objc:objc-object-pointer))
  (show-bookmarks-window))

(define-delegate-method ("addSuggestedBookmarks:" :void) ((sender objc:objc-object-pointer))
  (let ((added (add-suggested-bookmarks)))
    (refresh-bookmarks-window)
    (show-info (if (zerop added) "Nothing to add" "Bookmarks added")
               (if (zerop added)
                   "Every suggested bookmark is already in your bookmarks."
                   (format nil "~d suggested bookmark~:p added after your own." added)))))

(define-delegate-method ("showDownloads:" :void) ((sender objc:objc-object-pointer))
  (show-downloads-window :force t))

(define-delegate-method ("setSearchEngine:" :void) ((sender objc:objc-object-pointer))
  (let ((engine (nth (1- (objc:invoke sender "tag")) +search-engines+)))
    (when engine (set-search-engine (first engine)))))

(define-delegate-method ("validateMenuItem:" objc:objc-bool :on-error t)
    ((item objc:objc-object-pointer))
  (let ((action (objc:selector-name (objc:invoke item "action"))))
    (cond ((string= "reopenClosedTab:" action) (and *closed-pages* t))
          (t
           (when (string= "setSearchEngine:" action)
             (objc:invoke item "setState:"
                          (if (eq (first (nth (1- (objc:invoke item "tag")) +search-engines+))
                                  (search-engine))
                              1 0)))
           t))))

(defun search-engine-name (key)
  (second (assoc key +search-engines+)))

(defun settings-fields ()
  "The Settings window's rows, in order, filled in with what is set now."
  (list (make-field :text "Home page (New Window and Home go here)" :value (home-page)
                    :placeholder "about:start")
        (make-field :choice "Search words typed in the address bar with"
                    :choices (mapcar #'second +search-engines+)
                    :value (search-engine-name (search-engine)))
        (make-field :check "Show link numbers, to follow a link by typing it"
                    :value (link-numbers-p))
        (make-field :text (format nil "Timeout: seconds to wait for a server (~d to ~d)"
                                  (car +timeout-range+) (cdr +timeout-range+))
                    :value (princ-to-string (request-timeout)))
        (make-field :text "SOCKS5 proxy for every request (empty for none)"
                    :value (or (proxy) "")
                    :placeholder "socks5://127.0.0.1:9050 for Tor")
        (make-field :note "Guppy runs over UDP, which a SOCKS5 proxy cannot carry: with a proxy set, guppy:// addresses are refused rather than sent around it.")
        (make-field :check "Try TLS first on gopher:// servers, and fall back to plain Gopher"
                    :value (gopher-tls-p))))

(defun apply-settings (values)
  "Keep the Settings window's VALUES.  Signals an error, and keeps nothing,
if the timeout or the proxy is not one."
  (destructuring-bind (home engine numbers timeout proxy note tls) values
    (declare (ignore note))
    ;; Checked before anything is kept, so a bad one changes nothing.
    (let ((seconds (ignore-errors (parse-integer (string-trim " " timeout)))))
      (unless (and seconds (<= (car +timeout-range+) seconds (cdr +timeout-range+)))
        (error "The timeout is a number of seconds from ~d to ~d."
               (car +timeout-range+) (cdr +timeout-range+))))
    (let ((before-proxy (proxy))
          (before-numbers (link-numbers-p)))
      (set-proxy proxy)
      (set-request-timeout timeout)
      (set-home-page (let ((home (string-trim " " home)))
                       (if (plusp (length home)) home "about:start")))
      (let ((key (first (find engine +search-engines+ :key #'second :test #'equal))))
        (when key (set-search-engine key)))
      (set-link-numbers numbers)
      (set-gopher-tls tls)
      (g:forget-gopher-tls-status)
      ;; Pages fetched without the proxy (or through another) are not shown
      ;; from the cache as if they had come through this one.
      (unless (equal before-proxy (proxy))
        (clear-shared-cache))
      (unless (eq before-numbers (link-numbers-p))
        (redraw-all-pages))
      t)))

(defun edit-settings ()
  "Ask for the settings, and keep them.  What cannot be used is said so, and
asked again."
  (loop
    (let ((answers (run-form "Settings" (settings-fields) :kind :settings)))
      (unless answers (return nil))
      (handler-case (return (apply-settings answers))
        (error (condition)
          (show-info "Those settings cannot be used." (princ-to-string condition))
          (when *dialog-answerer* (return nil)))))))

(define-delegate-method ("showSettings:" :void) ((sender objc:objc-object-pointer))
  (edit-settings))

(define-delegate-method ("showTrustedCertificates:" :void) ((sender objc:objc-object-pointer))
  (show-certificates-window))

(define-delegate-method ("showIdentities:" :void) ((sender objc:objc-object-pointer))
  (show-identities-window))

(define-delegate-method ("applicationWillTerminate:" :void)
    ((notification objc:objc-object-pointer))
  (save-session (current-session-groups)))

;;; The menus -------------------------------------------------------------------------
;;;
;;; A program with no nib has no menu bar until it makes one.  Every target is
;;; nil: the responder chain finds the key window's controller, then the
;;; application, then this delegate.

(defun add-menu-item (menu title action key &key (tag 0) modifiers)
  (let ((item (objc:invoke (objc:invoke "NSMenuItem" "alloc")
                           "initWithTitle:action:keyEquivalent:"
                           title (objc:coerce-to-selector action) key)))
    (unless (zerop tag) (objc:invoke item "setTag:" tag))
    (when modifiers (objc:invoke item "setKeyEquivalentModifierMask:" modifiers))
    (objc:invoke menu "addItem:" item)
    (objc:release item)
    item))

(defun fill-menu (menu items)
  "Add ITEMS to MENU: each (TITLE ACTION [KEY [TAG [MODIFIERS]]]) or :SEPARATOR."
  (dolist (item items)
    (if (eq item :separator)
        (objc:invoke menu "addItem:" (objc:invoke "NSMenuItem" "separatorItem"))
        (destructuring-bind (title action &optional (key "") (tag 0) modifiers) item
          (add-menu-item menu title action key :tag tag :modifiers modifiers))))
  menu)

(defun add-submenu (main title items)
  "A top-level menu of ITEMS.  Answers the menu."
  (let ((menu (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" title))
        (holder (objc:invoke (objc:invoke "NSMenuItem" "alloc")
                             "initWithTitle:action:keyEquivalent:"
                             title (cffi:null-pointer) "")))
    (fill-menu menu items)
    (objc:invoke holder "setSubmenu:" menu)
    (objc:invoke main "addItem:" holder)
    (objc:release holder)
    (objc:release menu)
    menu))

(defun bookmark-url-for-item (item)
  "The URL of the bookmark a Bookmarks menu item stands for: its tag is the
bookmark's position, counted from 1."
  (let ((tag (if (null-object-p item) 0 (objc:invoke item "tag")))
        (bookmarks (bookmarks)))
    (and (<= 1 tag (length bookmarks))
         (bookmark-url (nth (1- tag) bookmarks)))))

(defun rebuild-bookmarks-menu ()
  "Make the Bookmarks menu list the bookmarks as they are now."
  (let ((menu *bookmarks-menu*))
    (when menu
      (objc:invoke menu "removeAllItems")
      (fill-menu menu `(("Add Bookmark" "addBookmark:" "d")
                        ("Remove Bookmark for This Page" "removeBookmark:")
                        ("Show All Bookmarks" "showBookmarks:" "B")
                        ("Edit Bookmarks…" "editBookmarks:" "b" 0 ,(logior +command+ +option+))
                        ("Add Suggested Bookmarks" "addSuggestedBookmarks:")))
      (let ((bookmarks (bookmarks)))
        (when bookmarks
          (objc:invoke menu "addItem:" (objc:invoke "NSMenuItem" "separatorItem"))
          (loop for bookmark in bookmarks
                for tag from 1
                do (add-menu-item menu (bookmark-title bookmark) "openBookmark:" ""
                                  :tag tag)))))))

(defun history-url-for-item (item)
  "The URL of the visit a History menu item stands for, by its tag, from 1."
  (let ((tag (if (null-object-p item) 0 (objc:invoke item "tag")))
        (visits (recent-visits +history-menu-length+)))
    (and (<= 1 tag (length visits))
         (visit-url (nth (1- tag) visits)))))

(defun menu-title (title)
  "TITLE short enough for a menu."
  (if (> (length title) 60) (concatenate 'string (subseq title 0 59) "…") title))

(defun rebuild-history-menu ()
  "Make the History menu list the latest visits."
  (let ((menu *history-menu*))
    (when menu
      (objc:invoke menu "removeAllItems")
      (fill-menu menu '(("Show All History" "showAllHistory:" "y")
                        ("Reopen Closed Tab" "reopenClosedTab:" "T")
                        ("Clear History…" "clearHistory:")))
      (let ((visits (recent-visits +history-menu-length+)))
        (when visits
          (objc:invoke menu "addItem:" (objc:invoke "NSMenuItem" "separatorItem"))
          (loop for visit in visits
                for tag from 1
                do (add-menu-item menu (menu-title (visit-title visit)) "openHistoryItem:" ""
                                  :tag tag)))))))

;;; Saving and restoring the windows -------------------------------------------------------

(defun controller-for-window (window)
  (find-if (lambda (c) (cffi:pointer-eq (controller-window c) window)) *controllers*))

(defun ns-array-list (array)
  (if (null-object-p array)
      '()
      (loop for i below (objc:invoke array "count")
            collect (objc:invoke array "objectAtIndex:" i))))

(defun current-session-groups ()
  "The open windows, as SAVE-SESSION wants them: one group per window, with
its tabs in order."
  (let ((seen '())
        (groups '()))
    (dolist (controller (reverse *controllers*))
      (let* ((window (controller-window controller))
             (group (objc:invoke window "tabGroup"))
             (windows (or (and (not (null-object-p group))
                               (ns-array-list (objc:invoke group "windows")))
                          (list window)))
             (first (first windows)))
        (unless (or (controller-closed-p controller)
                    (find first seen :test #'cffi:pointer-eq))
          (push first seen)
          (let* ((controllers (remove nil (mapcar #'controller-for-window windows)))
                 (selected (and (not (null-object-p group))
                                (objc:invoke group "selectedWindow")))
                 (urls (mapcar (lambda (c) (or (current-url c) (home-page))) controllers)))
            (when urls
              (push (list :frame (objc:invoke-into 'string first "stringWithSavedFrame")
                          :urls urls
                          :selected (or (and selected
                                             (position selected controllers
                                                       :key #'controller-window
                                                       :test #'cffi:pointer-eq))
                                        0))
                    groups))))))
    (nreverse groups)))

(defun restore-session (groups)
  "Open the windows and tabs GROUPS describes.  Answers how many windows."
  (dolist (group groups)
    (destructuring-bind (&key frame urls (selected 0)) group
      (let* ((first (open-window (first urls)))
             (controllers (list first)))
        (when (stringp frame)
          (objc:invoke (controller-window first) "setFrameFromString:" frame))
        (let ((previous first))
          (dolist (url (rest urls))
            (setf previous (open-tab previous url :background t))
            (push previous controllers)))
        (let ((front (nth selected (reverse controllers))))
          (when front
            (objc:invoke (controller-window front) "makeKeyAndOrderFront:"
                         (cffi:null-pointer)))))))
  (length groups))

(defun install-menu ()
  (let ((main (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" "Main"))
        (application (objc.runloop:shared-application)))
    (add-submenu main "Gopher"
                 `(("About Gopher" "orderFrontStandardAboutPanel:")
                   :separator
                   ("Settings…" "showSettings:" ",")
                   ("Trusted Certificates…" "showTrustedCertificates:")
                   ("Identities…" "showIdentities:")
                   :separator
                   ("Hide Gopher" "hide:" "h")
                   ;; Option-Command-H, leaving Shift-Command-H for Home.
                   ("Hide Others" "hideOtherApplications:" "h" 0 ,(logior (ash 1 19) (ash 1 20)))
                   ("Show All" "unhideAllApplications:")
                   :separator
                   ("Quit Gopher" "terminate:" "q")))
    (add-submenu main "File"
                 `(("New Tab" "newTab:" "t")
                   ("New Window" "newWindow:" "n")
                   ("Open Location…" "openLocation:" "l")
                   :separator
                   ("Save Page As…" "savePage:" "s")
                   :separator
                   ("Upload with Titan…" "uploadWithTitan:" "u" 0 ,(logior +command+ +shift+))
                   ("Edit This Page with Titan…" "editPageWithTitan:" "e" 0 ,(logior +command+ +shift+))
                   :separator
                   ("Close Window" "performClose:" "w")))
    (add-submenu main "Edit"
                 `(("Undo" "undo:" "z")
                   ("Redo" "redo:" "Z")
                   :separator
                   ("Cut" "cut:" "x")
                   ("Copy" "copy:" "c")
                   ("Paste" "paste:" "v")
                   ("Select All" "selectAll:" "a")
                   :separator
                   ;; NSTextFinderAction: show the find bar, next, previous.
                   ("Find…" "performTextFinderAction:" "f" 1)
                   ("Find Next" "performTextFinderAction:" "g" 2)
                   ("Find Previous" "performTextFinderAction:" "G" 3)))
    (let ((view (add-submenu main "View"
                             `(("Show Tab Bar" "toggleTabBar:")
                               ("Show All Tabs" "toggleTabOverview:" "\\")
                               :separator
                               ("Actual Size" "makeTextStandardSize:" "0")
                               ("Make Text Bigger" "makeTextBigger:" "+")
                               ("Make Text Smaller" "makeTextSmaller:" "-")
                               :separator
                               ("Show Link Numbers" "toggleLinkNumbers:" "k")
                               :separator
                               ("Page Info" "showPageInfo:" "i")
                               ("View Source" "viewSource:" "u" 0 ,(logior +command+ +option+))))))
      ;; Command-= as well as Command-+, which on most keyboards needs Shift.
      (let ((also (add-menu-item view "Make Text Bigger" "makeTextBigger:" "=")))
        (objc:invoke also "setHidden:" t)
        (objc:invoke also "setAllowsKeyEquivalentWhenHidden:" t)))
    (let ((go (add-submenu main "Go"
                           '(("Back" "goBack:" "[")
                             ("Forward" "goForward:" "]")
                             :separator
                             ("Reload Page" "reload:" "r")
                             ("Stop" "stopLoading:" ".")
                             :separator
                             ("Home" "goHome:" "H")
                             ("Set Home Page to This Page" "setHomePage:")
                             :separator))))
      ;; Words typed in the address bar are searched for with one of these.
      (let ((engines (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:"
                                  "Search With"))
            (holder (objc:invoke (objc:invoke "NSMenuItem" "alloc")
                                 "initWithTitle:action:keyEquivalent:"
                                 "Search Words With" (cffi:null-pointer) "")))
        (objc:invoke go "addItem:" holder)
        (objc:release holder)
        (loop for (nil name) in +search-engines+
              for tag from 1
              do (add-menu-item engines name "setSearchEngine:" "" :tag tag))
        (objc:invoke holder "setSubmenu:" engines)
        (objc:release engines)))
    (setf *history-menu* (add-submenu main "History" '()))
    (rebuild-history-menu)
    (setf *bookmarks-menu* (add-submenu main "Bookmarks" '()))
    (rebuild-bookmarks-menu)
    (objc:invoke application "setWindowsMenu:"
                 (add-submenu main "Window"
                              `(("Minimize" "performMiniaturize:" "m")
                                ("Zoom" "performZoom:")
                                :separator
                                ("Show Previous Tab" "selectPreviousTab:" "{")
                                ("Show Next Tab" "selectNextTab:" "}")
                                ("Move Tab to New Window" "moveTabToNewWindow:")
                                ("Merge All Windows" "mergeAllWindows:")
                                :separator
                                ("Downloads" "showDownloads:" "l" 0 ,(logior +command+ +option+))
                                :separator
                                ("Bring All to Front" "arrangeInFront:"))))
    (objc:invoke application "setMainMenu:" main)
    (objc:release main)
    main))

;;; Starting ------------------------------------------------------------------------------

(defun self-test-seconds ()
  "How long GOPHER_BROWSER_SELFTEST says to run for, or NIL.  With it set the
application quits by itself that many seconds after its window is up, which is
how a script checks that the bundle starts."
  (let ((value (uiop:getenv "GOPHER_BROWSER_SELFTEST")))
    (and value
         (let ((seconds (ignore-errors (parse-integer value))))
           (and seconds (plusp seconds) seconds)))))

(sb-alien:define-alien-type nil
    (sb-alien:struct cg-rect
      (x double-float) (y double-float) (width double-float) (height double-float)))

(defun capture-window-image (window-number)
  "The window WINDOW-NUMBER as it is on screen, a CGImageRef the caller
releases, or NIL.  A process may capture its own windows without asking for
Screen Recording, so this gets the real thing -- toolbar, tab bar and their
translucent materials -- where drawing the window offscreen cannot.  The
rectangle goes by value, which SB-ALIEN does and CFFI cannot."
  (sb-alien:with-alien ((bounds (sb-alien:struct cg-rect)))
    ;; CGRectNull: the smallest rectangle holding the window.
    (setf (sb-alien:slot bounds 'x) sb-ext:double-float-positive-infinity
          (sb-alien:slot bounds 'y) sb-ext:double-float-positive-infinity
          (sb-alien:slot bounds 'width) 0d0
          (sb-alien:slot bounds 'height) 0d0)
    (let ((image (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow :inexact)
                   (sb-alien:alien-funcall
                    (sb-alien:extern-alien "CGWindowListCreateImage"
                                           (function sb-sys:system-area-pointer
                                                     (sb-alien:struct cg-rect)
                                                     (sb-alien:unsigned 32)
                                                     (sb-alien:unsigned 32)
                                                     (sb-alien:unsigned 32)))
                    bounds
                    8                   ; kCGWindowListOptionIncludingWindow
                    window-number
                    ;; kCGWindowImageBoundsIgnoreFraming | kCGWindowImageBestResolution
                    9))))
      (if (zerop (sb-sys:sap-int image)) nil (cffi:make-pointer (sb-sys:sap-int image))))))

(defun write-png (rep path)
  (objc:invoke-bool (objc:invoke rep "representationUsingType:properties:" 4
                                 (objc:invoke "NSDictionary" "dictionary"))
                    "writeToFile:atomically:" path t))

(defun snapshot-window (controller path)
  "Write what CONTROLLER's window shows to PATH as a PNG: the window as it is
on screen if it can be captured, or else as its frame view draws it
offscreen (where toolbar items and the selected tab come out blank)."
  (let ((image (ignore-errors
                (capture-window-image
                 (objc:invoke (controller-window controller) "windowNumber")))))
    (if image
        (let ((rep (objc:autorelease
                    (objc:invoke (objc:invoke "NSBitmapImageRep" "alloc") "initWithCGImage:" image))))
          (cffi:foreign-funcall "CGImageRelease" :pointer image :void)
          (note "self test: captured the window from the screen")
          (write-png rep path))
        (snapshot-window-offscreen controller path))))

(defun snapshot-window-offscreen (controller path)
  (let* ((view (objc:invoke (objc:invoke (controller-window controller) "contentView")
                            "superview"))
         (bounds (objc:invoke view "bounds"))
         (rep (objc:invoke view "bitmapImageRepForCachingDisplayInRect:" bounds)))
    (objc:invoke view "cacheDisplayInRect:toBitmapImageRep:" bounds rep)
    (objc:invoke-bool (objc:invoke rep "representationUsingType:properties:" 4
                                   (objc:invoke "NSDictionary" "dictionary"))
                      "writeToFile:atomically:" path t)))

;;; The self test's last act: GOPHER_BROWSER_SNAPSHOT names a PNG to write the
;;; front window to, so that a script can see what the bundle drew.
(defun front-controller ()
  "The controller of the frontmost browser window, or of the newest."
  (or (loop for window in (ns-array-list (objc:invoke (objc.runloop:shared-application)
                                                      "orderedWindows"))
            for controller = (controller-for-window window)
            when controller return controller)
      (first *controllers*)))

(define-delegate-method ("gbSelfTestSnapshot:" :void) ((argument objc:objc-object-pointer))
  ;; What the window shows, for a script to check that a page arrived.
  (let ((controller (front-controller)))
    (when controller
      (note "self test: front window number ~d"
            (objc:invoke (controller-window controller) "windowNumber"))))
  (let ((controller (front-controller)))
    (when controller
      (let ((page (controller-page controller)))
        (note "self test: showing ~a: ~a (~d links)"
              (current-url controller)
              (if page (page-display-title page) "nothing")
              (if page (length (page-links page)) 0)))))
  (let ((path (uiop:getenv "GOPHER_BROWSER_SNAPSHOT")))
    (when (and path (plusp (length path)) *controllers*)
      (note "self test: snapshot ~:[failed~;written~] to ~a"
            (snapshot-window (front-controller) path) path))))

(defun start-application ()
  "Everything MAIN does before the event loop: AppKit, the menus, the delegate."
  (ensure-frameworks)
  (reset-text-styles)
  (forget-shared-state)
  (setf *main-thread-target* nil
        *controllers* '()
        *cascade-point* nil)
  (ensure-main-thread-target)
  (objc.runloop:shared-application)
  (setf *application-delegate* (make-instance 'application-delegate))
  (objc:invoke (objc.runloop:shared-application) "setDelegate:"
               (objc:objc-object-pointer *application-delegate*))
  (install-menu))

(defun main ()
  "The application's entry point.  Does not return."
  (setf *log* *error-output*)
  (handler-case
      (progn
        (start-application)
        (let ((url (uiop:getenv "GOPHER_BROWSER_URL")))
          (if (and url (plusp (length url)))
              (open-window url)
              ;; The windows and tabs open at the last quit, or the home page.
              (when (zerop (restore-session (saved-session)))
                (open-window))))
        (objc:invoke (objc.runloop:shared-application) "activateIgnoringOtherApps:" t)
        (note "ready")
        (let ((front (front-controller)))
          (when front
            (note "front window number ~d"
                  (objc:invoke (controller-window front) "windowNumber"))))
        (let ((seconds (self-test-seconds)))
          (when seconds
            (note "self test: quitting in ~d seconds" seconds)
            (objc:invoke (objc:objc-object-pointer *application-delegate*)
                         "performSelector:withObject:afterDelay:"
                         (objc:coerce-to-selector "gbSelfTestSnapshot:")
                         (cffi:null-pointer) (float (max 0.5 (- seconds 1)) 1d0))
            (objc:invoke (objc.runloop:shared-application)
                         "performSelector:withObject:afterDelay:"
                         (objc:coerce-to-selector "terminate:")
                         (cffi:null-pointer) (float seconds 1d0)))))
    (error (condition)
      (note "could not start: ~a" condition)
      (sb-ext:exit :code 1 :abort t)))
  (objc.runloop:run-cocoa-application))
