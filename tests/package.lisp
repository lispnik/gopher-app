;;;; package.lisp -- the test package, the root suite, and what tests share.

(defpackage #:gopher-browser/tests
  (:use #:common-lisp #:fiveam)
  (:local-nicknames (#:gb #:gopher-browser) (#:g #:gopher))
  (:export #:all-tests #:run-tests))

(in-package #:gopher-browser/tests)

(def-suite all-tests :description "Everything.")

(defun run-tests ()
  "Run every test and answer whether they all passed."
  (let ((results (run 'all-tests)))
    (explain! results)
    (results-status results)))

;;; A data directory of the test's own --------------------------------------------------

(defun call-with-data-directory (function)
  "Call FUNCTION with the browser's data directory a new, empty temporary one,
so that nothing a test does touches the real bookmarks or trusted hosts."
  (let* ((directory (sb-posix:mkdtemp "/tmp/gopher-browser-test-XXXXXX"))
         (saved (sb-posix:getenv "GOPHER_BROWSER_HOME")))
    (sb-posix:setenv "GOPHER_BROWSER_HOME" directory 1)
    (gb::forget-shared-state)
    ;; Set, not bound: about pages are made on fetch threads.  Tests start
    ;; with no bookmarks unless they say otherwise.
    (setf gb::*suggest-bookmarks* nil)
    (unwind-protect (funcall function directory)
      (if saved
          (sb-posix:setenv "GOPHER_BROWSER_HOME" saved 1)
          (sb-posix:unsetenv "GOPHER_BROWSER_HOME"))
      (gb::forget-shared-state)
      (setf gb::*suggest-bookmarks* t)
      (uiop:delete-directory-tree
       (uiop:ensure-directory-pathname directory)
       :validate (lambda (path) (search "gopher-browser-test-" (namestring path)))
       :if-does-not-exist :ignore))))

(defmacro with-data-directory ((&optional (directory (gensym "DIRECTORY"))) &body body)
  `(call-with-data-directory (lambda (,directory) (declare (ignorable ,directory)) ,@body)))
