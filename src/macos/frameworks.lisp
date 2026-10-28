;;;; frameworks.lisp -- AppKit: loading it, its constants, and the log.
;;;;
;;;; Nothing foreign is made when this is loaded.  The application is a saved
;;;; image, and a pointer made in the build would be garbage in the run.

(in-package #:gopher-browser)

(defparameter +cocoa-framework+
  "/System/Library/Frameworks/Cocoa.framework/Versions/A/Cocoa")

(defun ensure-frameworks ()
  "Bring up the Objective-C runtime with AppKit in it.  Every entry point that
touches AppKit calls this first; in the bundle it is also what rebuilds the
classes defined here."
  (objc:ensure-objc-initialized :modules (list +cocoa-framework+)))

;;; Constants ---------------------------------------------------------------------

(defconstant +ns-window-style-mask+ 15
  "Titled, closable, miniaturizable, resizable.")
(defconstant +ns-backing-store-buffered+ 2)

;;; Autoresizing: which of a view's margins and dimensions give when its
;;; superview changes size.  AppKit's y runs upwards, so "min y" is the bottom.
(defconstant +flexible-left+ 1)
(defconstant +flexible-width+ 2)
(defconstant +flexible-right+ 4)
(defconstant +flexible-bottom+ 8)
(defconstant +flexible-height+ 16)
(defconstant +flexible-top+ 32)

(defconstant +ns-modal-response-ok+ 1)
(defconstant +ns-alert-first-button-return+ 1000)
(defconstant +ns-event-modifier-flag-command+ (ash 1 20))
(defconstant +ns-font-weight-bold+ 0.4d0)
(defconstant +ns-font-weight-semibold+ 0.3d0)
(defconstant +ns-italic-font-mask+ 1)
(defconstant +ns-progress-indicator-style-spinning+ 1)
(defconstant +ns-text-finder-action-show-find-interface+ 1)

(defparameter +run-loop-modes+
  #("NSDefaultRunLoopMode" "NSEventTrackingRunLoopMode" "NSModalPanelRunLoopMode")
  "The modes a perform on the main thread is delivered in: the usual one, and
the ones that run while a window is resized or a panel is up.")

(defun appkit-string-constant (name)
  "The NSString an exported AppKit constant points at, such as
NSFontAttributeName.  These are pointers TO an NSString, so the symbol's
address is dereferenced once.  Read, never written in: the values are opaque."
  (let ((symbol (cffi:foreign-symbol-pointer name)))
    (unless symbol
      (error "No such AppKit constant: ~a" name))
    (cffi:mem-ref symbol :pointer)))

;;; Small things --------------------------------------------------------------------

(defun null-object-p (object)
  "Whether OBJECT is no Objective-C object at all."
  (or (null object) (cffi:null-pointer-p object)))

(defun ns-string (object)
  "The Lisp string an NSString holds, or NIL."
  (if (null-object-p object)
      nil
      (objc:invoke-into 'string object "description")))

(defvar *log* nil
  "Where NOTE writes: the application's log, once MAIN has said so.")

(defun note (control &rest arguments)
  "Write a line to the log."
  (when *log*
    (ignore-errors
     (multiple-value-bind (second minute hour) (get-decoded-time)
       (format *log* "~&~2,'0d:~2,'0d:~2,'0d ~?~%" hour minute second control arguments))
     (finish-output *log*))))
