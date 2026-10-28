;;;; downloads.lisp -- the downloads list: what is being saved, and what was.
;;;;
;;;; A DOWNLOAD is the record the Downloads window shows.  The fetch itself is
;;;; SESSION-DOWNLOAD on a thread of its own; the record is changed on the main
;;;; thread only, from what that thread reports.

(in-package #:gopher-browser)

(defstruct (download (:constructor %make-download))
  (id 0 :type integer)
  (url "" :type string)
  (path "" :type string)
  (state :running :type keyword)        ; :running :done :failed :stopped
  (bytes 0 :type integer)
  (total nil)
  (message nil)
  (handle nil))

(defvar *downloads* '()
  "Every download this run, newest first.  Main thread only.")
(defvar *download-counter* 0)

(defun make-download (url path)
  "A running download of URL to PATH, put at the top of the list."
  (let ((download (%make-download :id (incf *download-counter*) :url url :path path
                                  :handle (g:make-request-handle))))
    (push download *downloads*)
    download))

(defun download-name (download)
  "The file's name, without its folder."
  (let* ((path (download-path download))
         (slash (position #\/ path :from-end t)))
    (if slash (subseq path (1+ slash)) path)))

(defun size-text (bytes)
  (cond ((< bytes 1024) (format nil "~d bytes" bytes))
        ((< bytes (* 1024 1024)) (format nil "~,1f KB" (/ bytes 1024)))
        (t (format nil "~,1f MB" (/ bytes 1024 1024)))))

(defun download-status-text (download)
  "What the Downloads window says of DOWNLOAD."
  (ecase (download-state download)
    (:running (format nil "~a~@[ of ~a~]" (size-text (download-bytes download))
                      (and (download-total download) (size-text (download-total download)))))
    (:done (format nil "Done, ~a" (size-text (download-bytes download))))
    (:stopped "Stopped")
    (:failed (format nil "Failed: ~a" (or (download-message download) "unknown error")))))

(defun download-finished (download outcome)
  "Record OUTCOME, the end of DOWNLOAD's fetch."
  (case (outcome-kind outcome)
    (:saved (setf (download-state download) :done
                  (download-bytes download) (parse-integer (outcome-message outcome))))
    (:cancelled (setf (download-state download) :stopped))
    (t (setf (download-state download) :failed
             (download-message download) (outcome-message outcome))))
  (setf (download-handle download) nil)
  download)

(defun stop-download (download)
  (let ((handle (download-handle download)))
    (when (and handle (eq :running (download-state download)))
      (g:cancel-request handle)
      t)))

(defun clear-finished-downloads ()
  (setf *downloads* (remove :running *downloads* :key #'download-state :test-not #'eq)))
