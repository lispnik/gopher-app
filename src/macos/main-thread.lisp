;;;; main-thread.lisp -- getting from a fetch thread to the main one.
;;;;
;;;; Fetches run on Lisp threads of their own, which may not touch AppKit.
;;;; What they have to say is put on a queue, and the main thread is asked to
;;;; empty it.  They never wait for that.

(in-package #:gopher-browser)

(defvar *main-thread-queue* '())
(defvar *main-thread-lock* (sb-thread:make-mutex :name "gopher-browser main-thread queue"))
(defvar *main-thread-target* nil
  "The Objective-C object that answers gbDrainQueue:, once there is one.")
(defvar *drain-scheduled* nil
  "Whether the main thread has been asked and has not yet answered, so that a
busy fetch asks once rather than once for each batch of lines.")

(defun drain-main-thread-queue ()
  "Run everything queued, on the main thread.  Answers how many there were."
  (let ((thunks (sb-thread:with-mutex (*main-thread-lock*)
                  (setf *drain-scheduled* nil)
                  (prog1 (nreverse *main-thread-queue*)
                    (setf *main-thread-queue* '())))))
    (dolist (thunk thunks)
      (handler-case (funcall thunk)
        (error (condition) (note "main thread: ~a" condition))))
    (length thunks)))

(objc:define-objc-class queue-target ()
  ()
  (:objc-class-name "GopherBrowserQueueTarget"))

(objc:define-objc-method ("gbDrainQueue:" :void)
    ((self queue-target) (argument objc:objc-object-pointer))
  (declare (ignorable self argument))
  (handler-case (drain-main-thread-queue)
    (error (condition) (note "gbDrainQueue: ~a" condition))))

(defvar *queue-target* nil
  "Held here so the target is not collected while AppKit holds its pointer.")

(defun ensure-main-thread-target ()
  "Make the object fetch threads ask the main thread through, once."
  (unless *main-thread-target*
    (setf *queue-target* (make-instance 'queue-target)
          *main-thread-target* (objc:objc-object-pointer *queue-target*)))
  *main-thread-target*)

(defun post-to-main-thread (function)
  "Have FUNCTION called on the main thread, soon, and return without waiting.
Called on the main thread, it is simply called."
  (cond
    ((objc.runloop:main-thread-p)
     (funcall function))
    (t
     (let ((ask (sb-thread:with-mutex (*main-thread-lock*)
                  (push function *main-thread-queue*)
                  (and *main-thread-target*
                       (not *drain-scheduled*)
                       (setf *drain-scheduled* t)))))
       (when ask
         ;; A pool of this thread's own: there is none here unless we make one.
         (objc:with-autorelease-pool ()
           (objc:invoke *main-thread-target*
                        "performSelectorOnMainThread:withObject:waitUntilDone:modes:"
                        (objc:coerce-to-selector "gbDrainQueue:")
                        (cffi:null-pointer) nil +run-loop-modes+))))))
  (values))
