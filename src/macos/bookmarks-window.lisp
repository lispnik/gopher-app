;;;; bookmarks-window.lisp -- Bookmarks › Edit Bookmarks: rename, readdress,
;;;; reorder and delete.
;;;;
;;;; One window, made the first time it is asked for.  Its table shows the
;;;; bookmarks file as it is at each redraw, and every edit writes the file and
;;;; rebuilds the Bookmarks menu.

(in-package #:gopher-browser)

(objc:define-objc-class bookmarks-controller ()
  ((window :initform nil :accessor bookmarks-window)
   (table :initform nil :accessor bookmarks-table)
   (rows :initform '() :accessor bookmarks-rows))
  (:objc-class-name "GopherBrowserBookmarksController"))

(defvar *bookmarks-controller* nil)

(defmacro define-bookmarks-method ((selector result-type &key on-error)
                                   (&rest arguments) &body body)
  `(objc:define-objc-method (,selector ,result-type)
       ((self bookmarks-controller) ,@arguments)
     (declare (ignorable self ,@(mapcar #'first arguments)))
     (handler-case (progn ,@body)
       (error (condition)
         (note "~a: ~a" ,selector condition)
         ,on-error))))

(defun refresh-bookmarks-window (&optional select)
  "Show the bookmarks as they are now, and select row SELECT."
  (let ((controller *bookmarks-controller*))
    (when controller
      (setf (bookmarks-rows controller) (bookmarks))
      (objc:invoke (bookmarks-table controller) "reloadData")
      (when (and select (< -1 select (length (bookmarks-rows controller))))
        (select-table-row (bookmarks-table controller) select))))
  (rebuild-bookmarks-menu))

(define-bookmarks-method ("numberOfRowsInTableView:" :long :on-error 0)
    ((table objc:objc-object-pointer))
  (length (bookmarks-rows self)))

(define-bookmarks-method ("tableView:objectValueForTableColumn:row:" objc:objc-object-pointer
                          :on-error (cffi:null-pointer))
    ((table objc:objc-object-pointer) (column objc:objc-object-pointer) (row :long))
  (let ((bookmark (nth row (bookmarks-rows self))))
    (if bookmark
        (objc:string-to-ns-string (if (string= "url" (column-key column))
                                      (bookmark-url bookmark)
                                      (bookmark-title bookmark))
                                  t)
        (cffi:null-pointer))))

(define-bookmarks-method ("tableView:setObjectValue:forTableColumn:row:" :void)
    ((table objc:objc-object-pointer) (value objc:objc-object-pointer)
     (column objc:objc-object-pointer) (row :long))
  (let ((text (ns-string value)))
    (when text
      (if (string= "url" (column-key column))
          (update-bookmark row :url text)
          (update-bookmark row :title text))
      ;; A refused edit shows the old value again.
      (refresh-bookmarks-window row))))

(defun bookmarks-selection ()
  (selected-row (bookmarks-table *bookmarks-controller*)))

(define-bookmarks-method ("moveBookmarkUp:" :void) ((sender objc:objc-object-pointer))
  (let ((row (bookmarks-selection)))
    (when row (refresh-bookmarks-window (move-bookmark row -1)))))

(define-bookmarks-method ("moveBookmarkDown:" :void) ((sender objc:objc-object-pointer))
  (let ((row (bookmarks-selection)))
    (when row (refresh-bookmarks-window (move-bookmark row 1)))))

(define-bookmarks-method ("deleteBookmark:" :void) ((sender objc:objc-object-pointer))
  (let ((row (bookmarks-selection)))
    (when row
      (delete-bookmark row)
      (refresh-bookmarks-window (min row (1- (length (bookmarks))))))))

;;; Double-click, or Open: in the frontmost browser window, or a new one.
(define-bookmarks-method ("openSelectedBookmark:" :void) ((sender objc:objc-object-pointer))
  (let* ((row (bookmarks-selection))
         (bookmark (and row (nth row (bookmarks-rows self)))))
    (when bookmark
      (let ((browser (find-if-not #'controller-closed-p *controllers*)))
        (if browser
            (progn (navigate browser (bookmark-url bookmark))
                   (objc:invoke (controller-window browser) "makeKeyAndOrderFront:"
                                (cffi:null-pointer)))
            (open-window (bookmark-url bookmark)))))))

(defun make-bookmarks-controller ()
  (let* ((controller (make-instance 'bookmarks-controller))
         (target (objc:objc-object-pointer controller))
         (width 600d0) (height 420d0)
         (window (make-panel-window "Bookmarks" width height target))
         (content (objc:invoke window "contentView"))
         (left +flexible-right+)
         (right +flexible-left+))
    (setf (bookmarks-window controller) window
          (bookmarks-table controller)
          (add-table content (vector 14d0 56d0 (- width 28d0) (- height 70d0))
                     '(("title" "Title" 220 t) ("url" "Address" 340 t))
                     target :double-action "openSelectedBookmark:"))
    (add-push-button content "Open" "openSelectedBookmark:"
                     (vector 10d0 12d0 90d0 32d0) left target)
    (add-push-button content "Move Up" "moveBookmarkUp:"
                     (vector 104d0 12d0 100d0 32d0) left target)
    (add-push-button content "Move Down" "moveBookmarkDown:"
                     (vector 208d0 12d0 110d0 32d0) left target)
    (add-push-button content "Delete" "deleteBookmark:"
                     (vector (- width 104d0) 12d0 90d0 32d0) right target)
    (setf (bookmarks-rows controller) (bookmarks))
    controller))

(defun ensure-bookmarks-controller ()
  (or *bookmarks-controller*
      (setf *bookmarks-controller* (make-bookmarks-controller))))

(defun show-bookmarks-window ()
  (ensure-bookmarks-controller)
  (refresh-bookmarks-window)
  (objc:invoke (bookmarks-window *bookmarks-controller*) "makeKeyAndOrderFront:"
               (cffi:null-pointer)))
