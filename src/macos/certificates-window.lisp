;;;; certificates-window.lisp -- Trusted Certificates: the hosts trusted on
;;;; first use, and forgetting them.
;;;;
;;;; Forgetting a host means its next certificate is trusted as new, rather
;;;; than asked about as changed.  That is the way to accept a renewal made
;;;; elsewhere, and to stop trusting a certificate trusted by mistake.

(in-package #:gopher-browser)

(objc:define-objc-class certificates-controller ()
  ((window :initform nil :accessor certificates-window)
   (table :initform nil :accessor certificates-table)
   (rows :initform '() :accessor certificates-rows))
  (:objc-class-name "GopherBrowserCertificatesController"))

(defvar *certificates-controller* nil)

(defmacro define-certificates-method ((selector result-type &key on-error)
                                      (&rest arguments) &body body)
  `(objc:define-objc-method (,selector ,result-type)
       ((self certificates-controller) ,@arguments)
     (declare (ignorable self ,@(mapcar #'first arguments)))
     (handler-case (progn ,@body)
       (error (condition)
         (note "~a: ~a" ,selector condition)
         ,on-error))))

(defun known-host-rows ()
  "The trusted certificates, by host and port."
  (sort (copy-list (g:list-known-hosts (shared-known-hosts)))
        (lambda (a b)
          (or (string-lessp (g:known-host-host a) (g:known-host-host b))
              (and (string-equal (g:known-host-host a) (g:known-host-host b))
                   (< (g:known-host-port a) (g:known-host-port b)))))))

(defun refresh-certificates-window ()
  (let ((controller *certificates-controller*))
    (when controller
      (setf (certificates-rows controller) (known-host-rows))
      (objc:invoke (certificates-table controller) "reloadData"))))

(defun known-host-cell (entry key)
  (cond ((string= key "host") (g:known-host-host entry))
        ((string= key "port") (princ-to-string (g:known-host-port entry)))
        ((string= key "fingerprint") (g:known-host-fingerprint entry))
        ((string= key "seen")
         (let ((time (g:known-host-first-seen entry)))
           (if (integerp time) (universal-date time) "")))
        ((string= key "expires")
         (let ((time (g:known-host-expires entry)))
           (if (integerp time) (universal-date time) "")))
        (t "")))

(define-certificates-method ("numberOfRowsInTableView:" :long :on-error 0)
    ((table objc:objc-object-pointer))
  (length (certificates-rows self)))

(define-certificates-method ("tableView:objectValueForTableColumn:row:" objc:objc-object-pointer
                             :on-error (cffi:null-pointer))
    ((table objc:objc-object-pointer) (column objc:objc-object-pointer) (row :long))
  (let ((entry (nth row (certificates-rows self))))
    (if entry
        (objc:string-to-ns-string (known-host-cell entry (column-key column)) t)
        (cffi:null-pointer))))

(define-certificates-method ("forgetCertificate:" :void) ((sender objc:objc-object-pointer))
  (let* ((row (selected-row (certificates-table self)))
         (entry (and row (nth row (certificates-rows self)))))
    (when (and entry
               (confirm (format nil "Forget the certificate of ~a?" (g:known-host-host entry))
                        "The next certificate it shows will be trusted as new, without asking. Do this when you know the server's certificate has changed, or to stop trusting this one."
                        "Forget"))
      (g:forget-known-host (shared-known-hosts) (g:known-host-host entry)
                           (g:known-host-port entry))
      (refresh-certificates-window))))

(defun make-certificates-controller ()
  (let* ((controller (make-instance 'certificates-controller))
         (target (objc:objc-object-pointer controller))
         (width 760d0) (height 380d0)
         (window (make-panel-window "Trusted Certificates" width height target))
         (content (objc:invoke window "contentView")))
    (setf (certificates-window controller) window
          (certificates-table controller)
          (add-table content (vector 14d0 56d0 (- width 28d0) (- height 70d0))
                     '(("host" "Host" 170 nil) ("port" "Port" 50 nil)
                       ("fingerprint" "Fingerprint" 300 nil)
                       ("seen" "Trusted Since" 90 nil) ("expires" "Expires" 90 nil))
                     target))
    (add-push-button content "Forget…" "forgetCertificate:"
                     (vector 10d0 12d0 100d0 32d0) +flexible-right+ target)
    controller))

(defun ensure-certificates-controller ()
  (or *certificates-controller*
      (setf *certificates-controller* (make-certificates-controller))))

(defun show-certificates-window ()
  (ensure-certificates-controller)
  (refresh-certificates-window)
  (objc:invoke (certificates-window *certificates-controller*) "makeKeyAndOrderFront:"
               (cffi:null-pointer)))
