;;;; identities.lisp -- Gemini client certificates: which to show to whom.
;;;;
;;;; An IDENTITY is a certificate and key the browser made, with a name to
;;;; know it by and a SCOPE: a host, and a path on it.  A request shows the
;;;; identity whose scope fits its URL most closely -- the longest path that
;;;; is a prefix of the URL's -- and no identity to any other host.
;;;;
;;;; They are listed in identities.sexp; the PEM files are in identities/.
;;;; Identities from before the list existed (identities/HOST-cert.pem) are
;;;; adopted the first time it is read, scoped to their whole host.

(in-package #:gopher-browser)

(defstruct (client-identity (:conc-name identity-) (:constructor %make-identity))
  (name "" :type string)
  (host "" :type string)
  (path "/" :type string)
  (certificate "" :type string)         ; native path of the PEM certificate
  (key "" :type string)                 ; native path of the PEM key
  (created 0 :type integer))

(defvar *identities* :unloaded
  "The identities, as last read or written.")
(defvar *identities-lock* (bt:make-lock "gopher-browser identities"))

(defun identity-plist (identity)
  (list :name (identity-name identity) :host (identity-host identity)
        :path (identity-path identity) :certificate (identity-certificate identity)
        :key (identity-key identity) :created (identity-created identity)))

(defun plist-identity (plist)
  (destructuring-bind (&key name host path certificate key (created 0) &allow-other-keys) plist
    (and (stringp host) (stringp certificate) (stringp key)
         (%make-identity :name (if (stringp name) name host) :host host
                         :path (if (stringp path) path "/")
                         :certificate certificate :key key
                         :created (if (integerp created) created 0)))))

(defun legacy-identities ()
  "Identities made before identities.sexp: identities/HOST-cert.pem and its key."
  (let ((directory (data-file "identities/")))
    (loop for file in (and (probe-file directory)
                           (uiop:directory-files directory "*-cert.pem"))
          for name = (pathname-name file)
          for host = (subseq name 0 (- (length name) (length "-cert")))
          for key = (merge-pathnames (format nil "~a-key.pem" host) directory)
          when (probe-file key)
            collect (%make-identity :name host :host host :path "/"
                                    :certificate (namestring file) :key (namestring key)
                                    :created (or (ignore-errors (file-write-date file)) 0)))))

(defun save-identities (identities)
  (write-data-file "identities.sexp" (mapcar #'identity-plist identities))
  (setf *identities* identities))

(defun identities ()
  "Every identity, oldest first."
  (bt:with-lock-held (*identities-lock*)
    (when (eq *identities* :unloaded)
      (let ((form (read-data-file "identities.sexp")))
        (if (probe-file (data-file "identities.sexp"))
            (setf *identities* (remove nil (mapcar #'plist-identity (if (listp form) form '()))))
            ;; First read: adopt what there is, and write the list.
            (save-identities (legacy-identities)))))
    *identities*))

(defun forget-loaded-identities ()
  (bt:with-lock-held (*identities-lock*)
    (setf *identities* :unloaded)))

(defun normalise-scope-path (path)
  "PATH as a scope: starting with a slash, and \"/\" if empty."
  (let ((path (string-trim " " (or path ""))))
    (cond ((zerop (length path)) "/")
          ((char= #\/ (char path 0)) path)
          (t (concatenate 'string "/" path)))))

(defun identity-fits-p (identity host path)
  "Whether IDENTITY is for PATH on HOST: its path is PATH, or a whole-segment
beginning of it -- /club is for /club and /club/room, not /clubhouse."
  (and (string-equal host (identity-host identity))
       (let ((scope (identity-path identity)))
         (and (<= (length scope) (length path))
              (string= scope path :end2 (length scope))
              (or (= (length scope) (length path))
                  (char= #\/ (char scope (1- (length scope))))
                  (char= #\/ (char path (length scope))))))))

(defun identity-for-url (url)
  "The identity to show for URL, or NIL: of those for its host whose path is a
prefix of URL's, the one with the longest path."
  (let* ((parsed (ignore-errors (g:parse-url url)))
         (host (and parsed (g:url-host parsed)))
         (path (normalise-scope-path (and parsed (g:url-path parsed)))))
    (when host
      (let ((best nil))
        (dolist (identity (identities) best)
          (when (and (identity-fits-p identity host path)
                     (or (null best)
                         (> (length (identity-path identity)) (length (identity-path best)))))
            (setf best identity)))))))

(defun create-identity (host &key (path "/") name)
  "Make a new identity for HOST, shown for PATH and below.  Answers it."
  ;; Read the list first: its first reading adopts the identity files it
  ;; finds, and must not find this one before it is listed.
  (identities)
  (let* ((stamp (format nil "~d-~36r" (get-universal-time) (random (expt 36 4))))
         (certificate (data-file (format nil "identities/~a-cert.pem" stamp)))
         (key (data-file (format nil "identities/~a-key.pem" stamp)))
         (name (if (plusp (length (string-trim " " (or name "")))) (string-trim " " name) host)))
    (ensure-directories-exist certificate)
    (g:make-identity certificate key :common-name name)
    (let ((identity (%make-identity :name name :host (string-downcase host)
                                    :path (normalise-scope-path path)
                                    :certificate (namestring certificate)
                                    :key (namestring key)
                                    :created (get-universal-time))))
      (save-identities (append (identities) (list identity)))
      identity)))

(defun update-identity (index &key name path)
  "Rename the identity at INDEX, or change the path it is shown for.  An empty
name is refused.  Answers true if it changed."
  (let* ((identities (identities))
         (identity (nth index identities)))
    (when (and identity (or (null name) (plusp (length (string-trim " " name)))))
      (when name (setf (identity-name identity) (string-trim " " name)))
      (when path (setf (identity-path identity) (normalise-scope-path path)))
      (save-identities identities)
      t)))

(defun delete-identity (index)
  "Forget the identity at INDEX and delete its files.  It cannot be had back:
a server that knew it will not know a new one."
  (let* ((identities (identities))
         (identity (nth index identities)))
    (when identity
      (ignore-errors (delete-file (identity-certificate identity)))
      (ignore-errors (delete-file (identity-key identity)))
      (save-identities (remove identity identities))
      t)))

(defun identity-certificate-info (identity)
  "The CERTIFICATE-INFO of IDENTITY's certificate, or NIL if it will not read."
  (ignore-errors (g:certificate-info-from-file (identity-certificate identity))))
