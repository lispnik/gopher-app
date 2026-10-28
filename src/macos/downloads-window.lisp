;;;; downloads-window.lisp -- Window › Downloads: what is being saved, and what was.

(in-package #:gopher-browser)

(objc:define-objc-class downloads-controller ()
  ((window :initform nil :accessor downloads-window)
   (table :initform nil :accessor downloads-table))
  (:objc-class-name "GopherBrowserDownloadsController"))

(defvar *downloads-controller* nil)

(defvar *show-downloads-window* t
  "Whether starting a download brings the Downloads window forward.  The tests
turn it off, since they never show a window.")

(defmacro define-downloads-method ((selector result-type &key on-error)
                                   (&rest arguments) &body body)
  `(objc:define-objc-method (,selector ,result-type)
       ((self downloads-controller) ,@arguments)
     (declare (ignorable self ,@(mapcar #'first arguments)))
     (handler-case (progn ,@body)
       (error (condition)
         (note "~a: ~a" ,selector condition)
         ,on-error))))

(defun refresh-downloads-window ()
  (when *downloads-controller*
    (objc:invoke (downloads-table *downloads-controller*) "reloadData")))

(define-downloads-method ("numberOfRowsInTableView:" :long :on-error 0)
    ((table objc:objc-object-pointer))
  (length *downloads*))

(define-downloads-method ("tableView:objectValueForTableColumn:row:" objc:objc-object-pointer
                          :on-error (cffi:null-pointer))
    ((table objc:objc-object-pointer) (column objc:objc-object-pointer) (row :long))
  (let ((download (nth row *downloads*)))
    (if download
        (objc:string-to-ns-string
         (let ((key (column-key column)))
           (cond ((string= key "name") (download-name download))
                 ((string= key "status") (download-status-text download))
                 (t (download-url download))))
         t)
        (cffi:null-pointer))))

(defun selected-download ()
  (let ((row (selected-row (downloads-table *downloads-controller*))))
    (and row (nth row *downloads*))))

(define-downloads-method ("stopDownload:" :void) ((sender objc:objc-object-pointer))
  (let ((download (selected-download)))
    (when download (stop-download download))))

(define-downloads-method ("showDownloadInFinder:" :void) ((sender objc:objc-object-pointer))
  (let ((download (selected-download)))
    (when (and download (eq :done (download-state download)))
      (objc:invoke (objc:invoke "NSWorkspace" "sharedWorkspace")
                   "activateFileViewerSelectingURLs:"
                   (vector (objc:invoke "NSURL" "fileURLWithPath:" (download-path download)))))))

(define-downloads-method ("clearDownloads:" :void) ((sender objc:objc-object-pointer))
  (clear-finished-downloads)
  (refresh-downloads-window))

(defun make-downloads-controller ()
  (let* ((controller (make-instance 'downloads-controller))
         (target (objc:objc-object-pointer controller))
         (width 620d0) (height 320d0)
         (window (make-panel-window "Downloads" width height target))
         (content (objc:invoke window "contentView")))
    (setf (downloads-window controller) window
          (downloads-table controller)
          (add-table content (vector 14d0 56d0 (- width 28d0) (- height 70d0))
                     '(("name" "Name" 180 nil) ("status" "Status" 160 nil)
                       ("url" "From" 240 nil))
                     target :double-action "showDownloadInFinder:"))
    (add-push-button content "Stop" "stopDownload:"
                     (vector 10d0 12d0 90d0 32d0) +flexible-right+ target)
    (add-push-button content "Show in Finder" "showDownloadInFinder:"
                     (vector 104d0 12d0 140d0 32d0) +flexible-right+ target)
    (add-push-button content "Clear" "clearDownloads:"
                     (vector (- width 104d0) 12d0 90d0 32d0) +flexible-left+ target)
    controller))

(defun ensure-downloads-controller ()
  (or *downloads-controller*
      (setf *downloads-controller* (make-downloads-controller))))

(defun show-downloads-window (&key (force nil))
  "Bring the Downloads window forward, if downloads should, or FORCE."
  (ensure-downloads-controller)
  (refresh-downloads-window)
  (when (or force *show-downloads-window*)
    (objc:invoke (downloads-window *downloads-controller*) "orderFront:" (cffi:null-pointer))))
