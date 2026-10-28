;;;; navigation.lisp -- from a URL to an OUTCOME: what the window should do.
;;;;
;;;; SESSION-FETCH is synchronous and runs on a worker thread.  It never
;;;; signals: everything the library can say, a server's "no" or a socket's
;;;; failure, comes back as an OUTCOME, whose KIND is one of
;;;;
;;;;   :page                  show OUTCOME-PAGE
;;;;   :download              not text and not an image: offer to save the body
;;;;   :too-large             too big to hold: offer to save it with SESSION-DOWNLOAD
;;;;   :saved                 SESSION-DOWNLOAD wrote OUTCOME-TARGET
;;;;   :input                 ask OUTCOME-PROMPT, then fetch the URL again with it
;;;;   :redirect              go to OUTCOME-TARGET (another small-web scheme)
;;;;   :external              OUTCOME-TARGET belongs to another application
;;;;   :certificate-changed   ask whether to trust the host's new certificate
;;;;   :certificate-required  the host wants a client certificate (Gemini 6x)
;;;;   :error                 OUTCOME-MESSAGE says what went wrong
;;;;   :cancelled             Stop was pressed
;;;;
;;;; The window does the asking, the drawing and the saving; this decides.

(in-package #:gopher-browser)

(defstruct outcome
  (kind :error :type keyword)
  (url "" :type string)
  (page nil)
  (response nil)
  (prompt nil)
  (sensitive-p nil)
  (target nil)
  (message nil)
  (host nil)
  (suggested-name nil)
  (questions '()))

;;; The session ------------------------------------------------------------------------

(defclass session ()
  ((history :initform (g:make-history) :reader session-history)
   (cache :initform (g:make-response-cache :max-entries 100 :max-age 900)
          :reader session-cache)
   (known-hosts :initarg :known-hosts :reader session-known-hosts))
  (:documentation "One window's browsing: its history, and what it shares
with every other window (the cache and the trusted certificates)."))

(defvar *shared-cache* nil)
(defvar *shared-known-hosts* nil)
(defvar *shared-lock* (bt:make-lock "gopher-browser shared state"))

(defun shared-known-hosts ()
  "The trusted certificates, in known_hosts in the data directory, shared by
every window."
  (bt:with-lock-held (*shared-lock*)
    (or *shared-known-hosts*
        (setf *shared-known-hosts* (g:make-file-known-hosts (data-file "known_hosts"))))))

(defun forget-shared-state ()
  "Drop the shared cache and certificate store, so the next session reads them
again (from a data directory that may have changed, in the tests)."
  (bt:with-lock-held (*shared-lock*)
    (setf *shared-cache* nil
          *shared-known-hosts* nil))
  (forget-loaded-history)
  (forget-loaded-identities))

(defun clear-shared-cache ()
  "Forget every cached page, in every window."
  (let ((cache (bt:with-lock-held (*shared-lock*) *shared-cache*)))
    (when cache (g:cache-clear cache))))

(defun make-session ()
  (let ((session (make-instance 'session :known-hosts (shared-known-hosts))))
    (bt:with-lock-held (*shared-lock*)
      (if *shared-cache*
          (setf (slot-value session 'cache) *shared-cache*)
          (setf *shared-cache* (session-cache session))))
    session))

;;; Identities: one client certificate per host, made when a host asks. -----------------

(defun session-identity (session url)
  "The identity shown for URL (see IDENTITY-FOR-URL), as (values
certificate-path key-path identity), or NIL.  A bare host stands for its
root."
  (declare (ignore session))
  (when url
    (let ((identity (identity-for-url (if (search "://" url) url (format nil "gemini://~a/" url)))))
      (when (and identity (probe-file (identity-certificate identity))
                 (probe-file (identity-key identity)))
        (values (identity-certificate identity) (identity-key identity) identity)))))

(defun session-create-identity (session host &key (path "/") name)
  "Make an identity for HOST, shown from now on for PATH on it and below."
  (declare (ignore session))
  (let ((identity (create-identity host :path path :name name)))
    (values (identity-certificate identity) (identity-key identity) identity)))

;;; Responses to outcomes ---------------------------------------------------------------

(defun status-words (status)
  "A status keyword as words: :NOT-FOUND as \"Not found\"."
  (let ((words (substitute #\Space #\- (string-downcase (symbol-name status)))))
    (if (plusp (length words))
        (concatenate 'string (string (char-upcase (char words 0))) (subseq words 1))
        words)))

(defun image-mime-type-p (mime)
  (and mime (alexandria:starts-with-subseq "image/" (string-downcase mime))))

(defun suggested-file-name (url mime)
  "A file name for saving URL's body: the last part of its path, or a name
made from the MIME type."
  (let* ((path (handler-case (or (g:url-path (g:parse-url url)) "")
                 (error () "")))
         (name (subseq path (1+ (or (position #\/ path :from-end t) -1)))))
    (cond ((plusp (length name)) (g:percent-decode name))
          ((and mime (search "/" mime))
           (format nil "download.~a"
                   (let ((subtype (subseq mime (1+ (position #\/ mime)))))
                     (subseq subtype 0 (position #\; subtype)))))
          (t "download"))))

(defun response-outcome (response)
  "What to do with RESPONSE."
  (let ((url (g:url-string (g:response-url response))))
    (flet ((outcome (kind &rest arguments)
             (apply #'make-outcome :kind kind :url url :response response arguments)))
      (case (g:response-category response)
        (:success
         (let ((document (g:response-document response))
               (mime (g:response-mime-type response)))
           (cond (document
                  (outcome :page :page (document-page document url)))
                 ((image-mime-type-p mime)
                  (outcome :page :page (make-page url :title (url-title url)
                                                      :image (g:response-body response)
                                                      :mime-type mime)))
                 (t
                  (outcome :download :suggested-name (suggested-file-name url mime))))))
        (:input
         (outcome :input :prompt (let ((meta (g:response-meta response)))
                                   (if (plusp (length meta)) meta "Search for:"))
                         :sensitive-p (g:response-sensitive-input-p response)))
        (:redirect
         (let ((target (ignore-errors (g:response-redirect-url response))))
           (cond ((null target)
                  (outcome :error :message (format nil "A redirect to an address that could not be read: ~a"
                                                    (g:response-meta response))))
                 ((member (g:url-scheme target) (g:supported-schemes) :test #'string-equal)
                  (outcome :redirect :target (g:url-string target)))
                 (t
                  (outcome :external :target (g:url-string target))))))
        (:client-certificate
         (outcome :certificate-required
                  :host (g:url-host (g:response-url response))
                  :message (g:response-meta response)))
        (t
         (outcome :error
                  :message (format nil "~a~@[ (~d)~]~:[~;: ~a~]"
                                   (status-words (g:response-status response))
                                   (g:response-status-code response)
                                   (plusp (length (g:response-meta response)))
                                   (g:response-meta response))))))))

;;; Fetching ----------------------------------------------------------------------------

(defun url-scheme-of (string)
  "The scheme STRING starts with, lower-cased, or NIL.  \"host:70\" has none:
what follows a scheme's colon is never a digit."
  (let ((colon (position #\: string)))
    (and colon (plusp colon)
         (alpha-char-p (char string 0))
         (every (lambda (c) (or (alphanumericp c) (find c "+.-"))) (subseq string 0 colon))
         (not (and (< (1+ colon) (length string)) (digit-char-p (char string (1+ colon)))))
         (string-downcase (subseq string 0 colon)))))

(defun external-url-p (url)
  "Whether URL belongs to some other application: it names a scheme this
browser does not speak."
  (let ((scheme (url-scheme-of url)))
    (and scheme
         (not (string= scheme "about"))
         (not (member scheme (g:supported-schemes) :test #'string-equal)))))

(defun host-of (url)
  (ignore-errors (g:url-host (g:parse-url url))))

(defun call-with-outcome-conditions (url function)
  "Call FUNCTION, answering an OUTCOME for anything the library signals."
  (handler-case (funcall function)
    (g:certificate-mismatch (condition)
      (make-outcome :kind :certificate-changed :url url
                    :host (g:mismatch-host condition)
                    :message (format nil "The certificate of ~a has changed since it was first trusted.~%~%Before: ~a~%Now: ~a"
                                     (g:mismatch-host condition)
                                     (g:mismatch-expected condition)
                                     (let ((info (g:mismatch-certificate condition)))
                                       (if (g:certificate-info-p info)
                                           (g:certificate-fingerprint info)
                                           info)))))
    (g:request-cancelled ()
      (make-outcome :kind :cancelled :url url :message "Stopped."))
    (g:response-too-large ()
      ;; Too big to hold, so it is a file to save rather than a page to show.
      (make-outcome :kind :too-large :url url
                    :suggested-name (suggested-file-name url nil)
                    :message "This is too large to show, but it can be saved."))
    (g:small-web-error (condition)
      (make-outcome :kind :error :url url :message (princ-to-string condition)))
    (error (condition)
      (make-outcome :kind :error :url url
                    :message (format nil "Something went wrong: ~a" condition)))))

(defmacro with-outcome-conditions ((url) &body body)
  `(call-with-outcome-conditions ,url (lambda () ,@body)))

(defun accepting-certificates (accept function)
  "Call FUNCTION, trusting a changed certificate if ACCEPT."
  (handler-bind ((g:certificate-mismatch
                   (lambda (condition)
                     (declare (ignore condition))
                     (when accept
                       (invoke-restart 'g:accept-new-certificate)))))
    (funcall function)))

(defun url-with-search (url query)
  "URL with QUERY in it, as an address that asks the same question again:
a Gopher search in the URL's search field (%09), anything else as its ?query.
So Back, Reload, the history and bookmarks repeat the search rather than
asking for the words again."
  (handler-case
      (let ((parsed (g:parse-url url)))
        (g:url-string
         (if (member (g:url-scheme parsed) '("gopher" "gophers") :test #'string-equal)
             (g:make-gopher-url (g:url-host parsed)
                                :port (g:url-port parsed)
                                :type (or (g:url-gopher-type parsed) #\7)
                                :selector (or (g:url-gopher-selector parsed) "")
                                :search query
                                :scheme (g:url-scheme parsed))
             (g:url-with-query parsed query))))
    (error () url)))

(defun network-options (session url)
  "The FETCH options every request to URL shares: the proxy, TLS on Gopher,
the trusted certificates, and the host's identity if it has one."
  (multiple-value-bind (certificate key) (session-identity session url)
    (list :proxy (proxy)
          :tls (and (gopher-tls-p) :try)
          :known-hosts (session-known-hosts session)
          :client-certificate certificate
          :client-key key)))

(defparameter +view-source-prefix+ "view-source:")

(defun view-source-url-p (url)
  (let ((n (length +view-source-prefix+)))
    (and (> (length url) n) (string-equal +view-source-prefix+ url :end2 n))))

(defun gopher-plus-info-url-p (url)
  "Whether URL asks a Gopher+ server for an item's attributes: its Gopher+
field is \"!\"."
  (let ((parsed (ignore-errors (g:parse-url url))))
    (and parsed
         (member (g:url-scheme parsed) '("gopher" "gophers") :test #'string-equal)
         (equal "!" (ignore-errors (g:url-gopher-plus parsed))))))

(defun gopher-plus-info-url (url)
  "The address of the Gopher+ attributes of the item at URL."
  (let ((parsed (g:parse-url url)))
    (g:url-string (g:make-gopher-url (g:url-host parsed)
                                     :port (g:url-port parsed)
                                     :type (or (g:url-gopher-type parsed) #\1)
                                     :selector (or (g:url-gopher-selector parsed) "")
                                     :gopher-plus "!"
                                     :scheme (g:url-scheme parsed)))))

(defun gopher-plus-view-url (url view)
  "The address of VIEW, a GOPHER-PLUS-VIEW, of the item at URL."
  (let ((parsed (g:parse-url url)))
    (g:url-string (g:make-gopher-url (g:url-host parsed)
                                     :port (g:url-port parsed)
                                     :type (or (g:url-gopher-type parsed) #\1)
                                     :selector (or (g:url-gopher-selector parsed) "")
                                     :gopher-plus (format nil "+~a~@[ ~a~]"
                                                          (g:gopher-plus-view-mime-type view)
                                                          (g:gopher-plus-view-language view))
                                     :scheme (g:url-scheme parsed)))))

(defun question-words (kind)
  (case kind
    (:ask "Ask") (:ask-password "Password") (:ask-long "Several lines") (:ask-file "File")
    (:select "Select") (:choose "Choose") (:choose-file "Choose a file") (:note "Note")
    (t (string-capitalize (symbol-name kind)))))

(defun attributes-gemtext (records url)
  "The Gopher+ attribute RECORDS of the item at URL, as gemtext to read."
  (with-output-to-string (out)
    (if (null records)
        (format out "# No information~%~%The server sent no Gopher+ attributes for this item.~%")
        (dolist (record records)
          (let ((info (g:gopher-plus-record-info record)))
            (format out "# ~a~%" (if info (g:line-text info) "Item information"))
            (when (and info (g:line-url info))
              (format out "=> ~a Open the item~%" (g:line-url info))))
          (let ((abstract (g:gopher-plus-record-abstract record)))
            (when abstract
              (format out "~%## Abstract~%~a~%" abstract)))
          (let ((views (g:gopher-plus-record-views record)))
            (when views
              (format out "~%## Views~%")
              (dolist (view views)
                (format out "=> ~a ~a~@[ (~a)~]~@[ ~a~]~%"
                        (gopher-plus-view-url url view)
                        (g:gopher-plus-view-mime-type view)
                        (g:gopher-plus-view-language view)
                        (g:gopher-plus-view-size view)))))
          (let ((admin (g:gopher-plus-record-admin record)))
            (when admin
              (format out "~%## Administration~%")
              (loop for (name . value) in admin
                    do (format out "* ~a: ~a~%" name value))))
          (let ((questions (g:gopher-plus-record-questions record)))
            (when questions
              (format out "~%## Form~%")
              (dolist (q questions)
                (format out "* ~a: ~a~%" (question-words (g:gopher-plus-question-kind q))
                        (g:gopher-plus-question-prompt q)))))
          (let ((others (remove-if (lambda (block)
                                     (member (car block) '("INFO" "ABSTRACT" "VIEWS" "ADMIN" "ASK")
                                             :test #'string-equal))
                                   (g:gopher-plus-record-blocks record))))
            (dolist (block others)
              (format out "~%## ~a~%```~%~{~a~%~}```~%" (string-capitalize (car block))
                      (cdr block))))))))

(defun source-page (outcome)
  "A page of what the server sent for OUTCOME, as it was sent."
  (let* ((response (outcome-response outcome))
         (text (or (and response (g:response-text response)) ""))
         (url (outcome-url outcome))
         (page (make-page url :title (format nil "Source of ~a" (url-title url)))))
    (page-add-lines page (g:document-lines (g:parse-plain-text text)))
    page))

(defun answered-at (url query response)
  "The outcome of RESPONSE, a reply to QUERY asked of URL.  A page shown for
it is at URL-WITH-SEARCH, so that its address asks the question again; any
other outcome keeps the URL asked."
  (let ((outcome (response-outcome response)))
    (when (and url query (eq :page (outcome-kind outcome))
               ;; A redirect elsewhere made a page of its own address.
               (null (g:response-redirects response)))
      (let ((at (url-with-search url query)))
        (setf (outcome-url outcome) at
              (page-url (outcome-page outcome)) at)))
    outcome))

(defun session-fetch (session url &key query handle on-lines on-progress
                                       accept-certificate reload (timeout (request-timeout))
                                       (max-size g:*default-max-size*)
                                       form answers)
  "Fetch URL and answer an OUTCOME.  Never signals.

QUERY is input the server asked for.  HANDLE is a REQUEST-HANDLE that Stop
cancels.  ON-LINES and ON-PROGRESS are the library's streaming callbacks, and
run on this thread.  ACCEPT-CERTIFICATE trusts a changed certificate instead of
answering :CERTIFICATE-CHANGED.  RELOAD skips the cache.  A body larger than
MAX-SIZE answers :TOO-LARGE: SESSION-DOWNLOAD is the way to fetch it.

FORM fetches the Gopher+ form of the item at URL instead of the item, and
answers :FORM with its questions; ANSWERS submits answers to it, one for each
question that is not a note.

A view-source: URL answers the page the URL after it sends, as text.  A Gopher
URL whose Gopher+ field is ! answers the item's attributes, drawn as a page."
  (let ((url (string-trim " " url)))
    (with-outcome-conditions (url)
      (cond
        ((view-source-url-p url)
         (let* ((inner (subseq url (length +view-source-prefix+)))
                (outcome (session-fetch session inner :handle handle :timeout timeout
                                                      :accept-certificate accept-certificate
                                                      :reload reload)))
           (if (and (eq :page (outcome-kind outcome)) (outcome-response outcome)
                    (not (page-image (outcome-page outcome))))
               (let ((page (source-page outcome)))
                 (setf (page-url page) url)
                 (make-outcome :kind :page :url url :page page
                               :response (outcome-response outcome)))
               (if (eq :page (outcome-kind outcome))
                   (make-outcome :kind :error :url url
                                 :message "This page has no source to show.")
                   outcome))))
        ((external-url-p url)
         (make-outcome :kind :external :url url :target url))
        ((about-url-p url)
         (let ((page (about-page url query)))
           (if page
               (make-outcome :kind :page :url (page-url page) :page page)
               (make-outcome :kind :error :url url
                             :message (format nil "There is no page called ~a." url)))))
        ((or form (gopher-plus-info-url-p url))
         (accepting-certificates
          accept-certificate
          (lambda ()
            (multiple-value-bind (records response)
                (apply #'g:gopher-attributes url :handle handle :timeout timeout
                       (network-options session url))
              (cond ((not (g:response-success-p response))
                     (response-outcome response))
                    (form
                     (let ((questions (loop for record in records
                                            append (g:gopher-plus-record-questions record))))
                       (if questions
                           (make-outcome :kind :form :url url :response response
                                         :questions questions)
                           ;; No form after all: just the item.
                           (session-fetch session url :handle handle :timeout timeout
                                                      :on-lines on-lines
                                                      :on-progress on-progress))))
                    (t
                     (make-outcome :kind :page :url url :response response
                                   :page (let ((page (document-page
                                                      (g:parse-gemtext (attributes-gemtext records url)
                                                                       (g:parse-url url))
                                                      url)))
                                           page))))))))
        (answers
         (let* ((reply (if (form-reply-p answers) answers (make-form-reply :answers answers)))
                (file (form-reply-file reply)))
           (accepting-certificates
            accept-certificate
            (lambda ()
              (response-outcome
               (apply #'g:gopher-plus-submit url (form-reply-answers reply)
                      :handle handle :timeout timeout :on-lines on-lines :on-progress on-progress
                      :max-size max-size
                      (append (and file (list :gopher-plus-file
                                              (sb-ext:parse-native-namestring file)))
                              (network-options session url))))))))
        (t
         (when reload
           (ignore-errors (g:cache-forget (session-cache session) url)))
         (accepting-certificates
          accept-certificate
          (lambda ()
            (answered-at
             (and query url)
             query
             (apply #'g:fetch url :query query :handle handle
                    :on-lines on-lines :on-progress on-progress
                    :timeout timeout :max-size max-size
                    :cache (and (not query) (session-cache session))
                    (network-options session url))))))))))

(defun session-upload (session url data &key (mime-type "text/gemini") token handle
                                             accept-certificate
                                             (timeout (max 60 (request-timeout))))
  "Upload DATA (a string, octets or a pathname) to URL with Titan (a titan://
or gemini:// URL), and answer an OUTCOME for what the server says, which is
usually the page the upload made."
  (let ((url (string-trim " " url)))
    (with-outcome-conditions (url)
      (accepting-certificates
       accept-certificate
       (lambda ()
         (response-outcome
          (apply #'g:upload url data :mime-type mime-type
                 :token (and token (plusp (length token)) token)
                 :handle handle :timeout timeout
                 (network-options session url))))))))

(defun session-download (session url path &key handle on-progress accept-certificate
                                               (timeout (request-timeout)))
  "Fetch URL straight into the file PATH (a native namestring), never holding
the body in memory and with no limit on its size.  Answers an OUTCOME:
:SAVED (with the byte count in OUTCOME-MESSAGE) or why not.

The body is written beside PATH and renamed onto it only when the whole of it
has arrived, so a failed or stopped download leaves nothing behind -- and an
existing file at PATH stays as it was."
  (let* ((url (string-trim " " url))
         (partial-name (format nil "~a.download" path))
         (partial (sb-ext:parse-native-namestring partial-name))
         (done nil))
    (unwind-protect
         (with-outcome-conditions (url)
           (let ((response
                   (with-open-file (sink partial :direction :output
                                                 :element-type '(unsigned-byte 8)
                                                 :if-exists :supersede)
                     (accepting-certificates
                      accept-certificate
                      (lambda ()
                        (apply #'g:fetch url :handle handle :on-progress on-progress
                                             :timeout timeout :sink sink
                               (network-options session url)))))))
             (cond ((g:response-success-p response)
                    ;; rename(2), not RENAME-FILE: that would give a target
                    ;; with no extension this one's ".download".
                    (sb-posix:rename partial-name path)
                    (setf done t)
                    (make-outcome :kind :saved :url url :response response
                                  :target path
                                  :message (format nil "~d" (g:response-bytes-read response))))
                   (t
                    ;; Not a file after all: a "no", a question, a redirect.
                    (let ((outcome (response-outcome response)))
                      (if (eq :error (outcome-kind outcome))
                          outcome
                          (make-outcome :kind :error :url url
                                        :message (format nil "There was no file to save here (~a)."
                                                         (status-words (g:response-status response))))))))))
      (unless done
        (ignore-errors (delete-file partial))))))

;;; Gopher+ form replies -----------------------------------------------------------------
;;;
;;; What a form's fields hold becomes what is sent.  Per the Gopher+ spec each
;;; question but a Note is one line; an AskL is a count and its lines; a
;;; ChooseF file is sent whole after the answers, its line just its name (not
;;; the path, which is nobody's business); and an AskF names a local file to
;;; keep the reply in, its line likewise just the name.

(defstruct form-reply
  (answers '())
  (file nil)          ; native path of a ChooseF file to send
  (save-to nil))      ; native path to keep the reply in (AskF)

(defun form-reply (questions values)
  "The FORM-REPLY for QUESTIONS answered with VALUES, one per question."
  (let ((reply (make-form-reply)))
    (setf (form-reply-answers reply)
          (loop for question in questions
                for value in values
                for kind = (g:gopher-plus-question-kind question)
                unless (eq kind :note)
                  collect (case kind
                            (:ask-long
                             (uiop:split-string (string-right-trim '(#\Newline) (or value ""))
                                                :separator '(#\Newline)))
                            (:choose-file
                             (let ((path (string-trim " " (or value ""))))
                               (when (and (plusp (length path)) (null (form-reply-file reply)))
                                 (setf (form-reply-file reply) path))
                               (subseq path (1+ (or (position #\/ path :from-end t) -1)))))
                            (:ask-file
                             (let ((path (string-trim " " (or value ""))))
                               (when (and (plusp (length path)) (null (form-reply-save-to reply)))
                                 (setf (form-reply-save-to reply) path))
                               (subseq path (1+ (or (position #\/ path :from-end t) -1)))))
                            (t (or value "")))))
    reply))

;;; Page information ----------------------------------------------------------------------

(defun trust-words (trust)
  (case trust
    (:new "Trusted on first use: this is the first time this certificate was seen here.")
    (:trusted "The same certificate as was trusted before.")
    (:expired-replaced "A new certificate, trusted because the old one had expired.")
    (:changed "Changed since it was first trusted, and trusted again when you said so.")
    (:unverified "Not checked.")
    (t (format nil "~(~a~)" trust))))

(defun universal-date (time)
  (multiple-value-bind (s m h day month year) (decode-universal-time time 0)
    (declare (ignore s m h))
    (format nil "~d-~2,'0d-~2,'0d" year month day)))

(defun page-info-text (session outcome)
  "What Page Info says about OUTCOME, the page on show: where it came from,
what it is, and how the connection was secured."
  (let* ((response (and outcome (outcome-response outcome)))
         (url (if outcome (outcome-url outcome) ""))
         (certificate (and response (g:response-certificate response))))
    (with-output-to-string (out)
      (format out "Address: ~a~%" url)
      (cond
        ((null response)
         (format out "~%A page the browser wrote itself.~%"))
        (t
         (format out "Protocol: ~a~%" (string-capitalize (symbol-name (g:response-protocol response))))
         (format out "Status: ~a~@[ (~d)~]~%" (status-words (g:response-status response))
                 (g:response-status-code response))
         (format out "Type: ~a~%" (or (g:response-mime-type response) "unknown"))
         (format out "Size: ~a~%" (size-text (g:response-bytes-read response)))
         (format out "~%Connection: ~:[not encrypted~;encrypted (TLS)~]~%" certificate)
         (when (proxy)
           (format out "Through the proxy ~a~%" (proxy)))
         (when certificate
           (format out "Certificate: ~a~%" (g:certificate-fingerprint certificate))
           (let ((names (g:certificate-common-names certificate)))
             (when names (format out "Issued to: ~{~a~^, ~}~%" names)))
           (let ((from (g:certificate-not-before certificate))
                 (to (g:certificate-not-after certificate)))
             (when (and from to)
               (format out "Valid: ~a to ~a~:[~; (expired)~]~%" (universal-date from)
                       (universal-date to) (g:certificate-expired-p certificate))))
           (format out "Trust: ~a~%" (trust-words (g:response-trust response))))
         (multiple-value-bind (certificate key identity) (session-identity session url)
           (declare (ignore certificate key))
           (when identity
             (format out "~%Your identity “~a” (for ~a~a) was shown to the server.~%"
                     (identity-name identity) (identity-host identity)
                     (identity-path identity)))))))))

;;; History -----------------------------------------------------------------------------
;;;
;;; Back and Forward look before they step: the history moves only once the
;;; page they asked for has arrived, so a failed fetch leaves it where it was.

(defun session-record (session url)
  "Note that URL is now on show: it becomes the current history entry."
  (g:history-visit (session-history session) url))

(defun history-neighbour (session offset)
  "The URL OFFSET entries from the current one, or NIL."
  (multiple-value-bind (entries position) (g:history-list (session-history session))
    (let ((index (+ position offset)))
      (and (<= 0 index) (< index (length entries)) (nth index entries)))))

(defun session-back-url (session)
  "Where Back would go, without going."
  (history-neighbour session -1))

(defun session-forward-url (session)
  "Where Forward would go, without going."
  (history-neighbour session 1))

(defun session-back (session)
  "Step back.  Answers the URL to show, or NIL."
  (g:history-back (session-history session)))

(defun session-forward (session)
  "Step forward.  Answers the URL to show, or NIL."
  (g:history-forward (session-history session)))
