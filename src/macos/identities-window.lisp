;;;; identities-window.lisp -- Identities: the client certificates the browser
;;;; made, what they are called, where each is shown, and deleting them.

(in-package #:gopher-browser)

(objc:define-objc-class identities-controller ()
  ((window :initform nil :accessor identities-window)
   (table :initform nil :accessor identities-table)
   (rows :initform '() :accessor identities-rows))
  (:objc-class-name "GopherBrowserIdentitiesController"))

(defvar *identities-controller* nil)

(defmacro define-identities-method ((selector result-type &key on-error)
                                    (&rest arguments) &body body)
  `(objc:define-objc-method (,selector ,result-type)
       ((self identities-controller) ,@arguments)
     (declare (ignorable self ,@(mapcar #'first arguments)))
     (handler-case (progn ,@body)
       (error (condition)
         (note "~a: ~a" ,selector condition)
         ,on-error))))

(defun refresh-identities-window (&optional select)
  (let ((controller *identities-controller*))
    (when controller
      (setf (identities-rows controller) (identities))
      (objc:invoke (identities-table controller) "reloadData")
      (when (and select (< -1 select (length (identities-rows controller))))
        (select-table-row (identities-table controller) select)))))

(defun identity-cell (identity key)
  (cond ((string= key "name") (identity-name identity))
        ((string= key "host") (identity-host identity))
        ((string= key "path") (identity-path identity))
        ((string= key "fingerprint")
         (let ((info (identity-certificate-info identity)))
           (if info (g:certificate-fingerprint info) "unreadable")))
        ((string= key "expires")
         (let* ((info (identity-certificate-info identity))
                (to (and info (g:certificate-not-after info))))
           (if to (universal-date to) "")))
        (t "")))

(define-identities-method ("numberOfRowsInTableView:" :long :on-error 0)
    ((table objc:objc-object-pointer))
  (length (identities-rows self)))

(define-identities-method ("tableView:objectValueForTableColumn:row:" objc:objc-object-pointer
                           :on-error (cffi:null-pointer))
    ((table objc:objc-object-pointer) (column objc:objc-object-pointer) (row :long))
  (let ((identity (nth row (identities-rows self))))
    (if identity
        (objc:string-to-ns-string (identity-cell identity (column-key column)) t)
        (cffi:null-pointer))))

;;; The name and the path are edited in place.
(define-identities-method ("tableView:setObjectValue:forTableColumn:row:" :void)
    ((table objc:objc-object-pointer) (value objc:objc-object-pointer)
     (column objc:objc-object-pointer) (row :long))
  (let ((text (ns-string value)))
    (when text
      (if (string= "path" (column-key column))
          (update-identity row :path text)
          (update-identity row :name text))
      (refresh-identities-window row))))

(define-identities-method ("newIdentity:" :void) ((sender objc:objc-object-pointer))
  (let ((values (run-form "New Identity"
                          (list (make-field :text "Host to show it to" :placeholder "example.org")
                                (make-field :text "For pages under this path" :value "/")
                                (make-field :text "Name it" :placeholder "Defaults to the host"))
                          :kind :new-identity)))
    (when values
      (destructuring-bind (host path name) values
        (let ((host (string-trim " " host)))
          (if (zerop (length host))
              (show-info "No identity was made." "An identity is for a host: name one.")
              (progn (create-identity host :path path :name name)
                     (refresh-identities-window (1- (length (identities)))))))))))

(define-identities-method ("deleteIdentity:" :void) ((sender objc:objc-object-pointer))
  (let* ((row (selected-row (identities-table self)))
         (identity (and row (nth row (identities-rows self)))))
    (when (and identity
               (confirm (format nil "Delete the identity “~a”?" (identity-name identity))
                        (format nil "~a will no longer know you as this identity, and it cannot be made again: a new one is a stranger to the server."
                                (identity-host identity))
                        "Delete"))
      (delete-identity row)
      (refresh-identities-window))))

(defun make-identities-controller ()
  (let* ((controller (make-instance 'identities-controller))
         (target (objc:objc-object-pointer controller))
         (width 780d0) (height 360d0)
         (window (make-panel-window "Identities" width height target))
         (content (objc:invoke window "contentView")))
    (setf (identities-window controller) window
          (identities-table controller)
          (add-table content (vector 14d0 56d0 (- width 28d0) (- height 70d0))
                     '(("name" "Name" 150 t) ("host" "Host" 150 nil) ("path" "For Path" 110 t)
                       ("fingerprint" "Fingerprint" 240 nil) ("expires" "Expires" 90 nil))
                     target))
    (add-push-button content "New…" "newIdentity:" (vector 10d0 12d0 90d0 32d0)
                     +flexible-right+ target)
    (add-push-button content "Delete…" "deleteIdentity:"
                     (vector (- width 114d0) 12d0 100d0 32d0) +flexible-left+ target)
    controller))

(defun ensure-identities-controller ()
  (or *identities-controller*
      (setf *identities-controller* (make-identities-controller))))

(defun show-identities-window ()
  (ensure-identities-controller)
  (refresh-identities-window)
  (objc:invoke (identities-window *identities-controller*) "makeKeyAndOrderFront:"
               (cffi:null-pointer)))
