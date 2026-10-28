;;;; navigation-tests.lisp -- from a URL to an outcome, against real servers.

(in-package #:gopher-browser/tests)

(def-suite navigation :in all-tests :description "Fetching and deciding what to do.")
(in-suite navigation)

(defun fetch (url &rest keys)
  (apply #'gb:session-fetch (gb:make-session) url :timeout 5 keys))

(test a-gopher-menu-is-a-page
  (with-data-directory ()
    (with-gopher-server (port)
      (let ((outcome (fetch (gopher-url port))))
        (is (eq :page (gb:outcome-kind outcome)))
        (is (search "Welcome to the fixture" (gb:page-text (gb:outcome-page outcome))))
        (is (= 6 (length (gb:page-links (gb:outcome-page outcome)))))))))

(test a-gopher-text-file-is-a-preformatted-page
  (with-data-directory ()
    (with-gopher-server (port)
      (let* ((outcome (fetch (gopher-url port "/0/hello.txt")))
             (page (gb:outcome-page outcome)))
        (is (eq :page (gb:outcome-kind outcome)))
        (is (string= (format nil "Hello, Gopher.~%Second line.~%") (gb:page-text page)))
        (is (every (lambda (run) (eq :pre (gb:run-style run))) (gb:page-runs page)))))))

(test a-search-asks-and-then-searches
  (with-data-directory ()
    (with-gopher-server (port)
      (let ((asked (fetch (gopher-url port "/7/search"))))
        (is (eq :input (gb:outcome-kind asked)))
        (is (not (gb:outcome-sensitive-p asked)))
        (let ((answered (fetch (gb:outcome-url asked) :query "lisp")))
          (is (eq :page (gb:outcome-kind answered)))
          (is (search "You searched for lisp" (gb:page-text (gb:outcome-page answered)))))))))

(test an-image-is-a-page-with-a-picture
  (with-data-directory ()
    (with-gopher-server (port)
      (let ((outcome (fetch (gopher-url port "/I/pic.png"))))
        (is (eq :page (gb:outcome-kind outcome)))
        (is (equalp +png+ (gb:page-image (gb:outcome-page outcome))))))))

(test a-binary-is-a-download
  (with-data-directory ()
    (with-gopher-server (port)
      (let ((outcome (fetch (gopher-url port "/9/file.bin"))))
        (is (eq :download (gb:outcome-kind outcome)))
        (is (string= "file.bin" (gb:outcome-suggested-name outcome)))
        (is (= 256 (length (g:response-body (gb:outcome-response outcome)))))))))

(test a-web-link-goes-to-another-application
  (with-data-directory ()
    (with-gopher-server (port)
      ;; The library turns a Gopher URL: item into the link it names.
      (let* ((menu (gb:outcome-page (fetch (gopher-url port))))
             (web (find "The web" (gb:page-links menu) :key #'gb:link-label :test #'string=))
             (outcome (fetch (gb:link-url web))))
        (is (string= "https://example.com/" (gb:link-url web)))
        (is (eq :external (gb:outcome-kind outcome)))
        (is (string= "https://example.com/" (gb:outcome-target outcome)))))
    (is (eq :external (gb:outcome-kind (fetch "mailto:someone@example.org"))))
    ;; A host and port is not a scheme.
    (is (not (eq :external (gb:outcome-kind (fetch "127.0.0.1:1")))))))

(test a-refused-connection-is-an-error-outcome
  (with-data-directory ()
    (let* ((port (with-gopher-server (port) port))
           (outcome (fetch (gopher-url port))))
      (is (eq :error (gb:outcome-kind outcome)))
      (is (plusp (length (gb:outcome-message outcome)))))))

(test stop-cancels-a-fetch
  (with-data-directory ()
    (with-gopher-server (port)
      (let* ((handle (g:make-request-handle))
             (result nil)
             (thread (sb-thread:make-thread
                      (lambda () (setf result (fetch (gopher-url port "/1/stall") :handle handle))))))
        (sleep 0.3)
        (g:cancel-request handle)
        (sb-thread:join-thread thread)
        (is (eq :cancelled (gb:outcome-kind result)))))))

(test gemini-pages-and-trust-on-first-use
  (with-data-directory ()
    (with-gemini-server (port)
      (let ((outcome (fetch (gemini-url port))))
        (is (eq :page (gb:outcome-kind outcome)))
        (is (string= "Welcome" (gb:page-title (gb:outcome-page outcome))))
        (is (eq :new (g:response-trust (gb:outcome-response outcome)))))
      ;; The certificate was written to known_hosts in the data directory.
      (is (probe-file (gb::data-file "known_hosts")))
      (let ((again (fetch (gemini-url port "/two"))))
        (is (eq :trusted (g:response-trust (gb:outcome-response again))))))))

(test a-changed-certificate-is-asked-about
  (with-data-directory ()
    (with-gemini-server (port)
      (g:store-known-host (gb::shared-known-hosts)
                          (g:make-known-host :host "127.0.0.1" :port port
                                             :fingerprint "sha256:00"))
      (let ((outcome (fetch (gemini-url port))))
        (is (eq :certificate-changed (gb:outcome-kind outcome)))
        (is (string= "127.0.0.1" (gb:outcome-host outcome)))
        (is (search "sha256:00" (gb:outcome-message outcome))))
      (let ((accepted (fetch (gemini-url port) :accept-certificate t)))
        (is (eq :page (gb:outcome-kind accepted))))
      ;; Accepted means remembered.
      (is (eq :page (gb:outcome-kind (fetch (gemini-url port "/two"))))))))

(test gemini-input-and-sensitive-input
  (with-data-directory ()
    (with-gemini-server (port)
      (let ((asked (fetch (gemini-url port "/input"))))
        (is (eq :input (gb:outcome-kind asked)))
        (is (string= "What is your name?" (gb:outcome-prompt asked)))
        (is (search "Hello, Ann Smith."
                    (gb:page-text (gb:outcome-page (fetch (gemini-url port "/input")
                                                          :query "Ann Smith"))))))
      (is (gb:outcome-sensitive-p (fetch (gemini-url port "/secret")))))))

(test redirects-to-other-schemes
  (with-data-directory ()
    (with-gopher-server (gopher-port)
      (with-gemini-server (port)
        (progn
          ;; Set, not bound: the server reads it on a thread of its own.
          (setf *gopher-port-for-redirect* gopher-port)
          (let ((outcome (fetch (gemini-url port "/to-gopher"))))
            (is (eq :redirect (gb:outcome-kind outcome)))
            (is (string= (gopher-url gopher-port "/0/hello.txt") (gb:outcome-target outcome))))
          (let ((outcome (fetch (gemini-url port "/elsewhere"))))
            (is (eq :external (gb:outcome-kind outcome)))))))))

(test a-server-error-is-an-error-outcome-with-its-words
  (with-data-directory ()
    (with-gemini-server (port)
      (let ((outcome (fetch (gemini-url port "/nowhere"))))
        (is (eq :error (gb:outcome-kind outcome)))
        (is (search "Not here" (gb:outcome-message outcome)))
        (is (search "(51)" (gb:outcome-message outcome)))))))

(test an-identity-is-made-when-a-host-asks
  (with-data-directory ()
    (with-gemini-server (port)
      (let ((session (gb:make-session)))
        (let ((asked (gb:session-fetch session (gemini-url port "/identity") :timeout 5)))
          (is (eq :certificate-required (gb:outcome-kind asked)))
          (is (string= "127.0.0.1" (gb:outcome-host asked)))
          (is (null (gb:session-identity session "127.0.0.1"))))
        (gb:session-create-identity session "127.0.0.1")
        (is (gb:session-identity session "127.0.0.1"))
        (let ((answered (gb:session-fetch session (gemini-url port "/identity") :timeout 5)))
          (is (eq :page (gb:outcome-kind answered)))
          (is (search "with a certificate" (gb:page-text (gb:outcome-page answered)))))))))

(test about-pages-need-no-network
  (with-data-directory ()
    (let ((outcome (fetch "about:start")))
      (is (eq :page (gb:outcome-kind outcome)))
      (is (string= "about:start" (gb:page-url (gb:outcome-page outcome)))))
    (is (eq :error (gb:outcome-kind (fetch "about:nonsense"))))))

(test streaming-lines-are-the-pages-lines
  (with-data-directory ()
    (with-gopher-server (port)
      (let* ((batches '())
             (outcome (fetch (gopher-url port "/1/slow")
                             :on-lines (lambda (lines) (push (coerce lines 'list) batches))))
             (streamed (gb::make-page (gopher-url port "/1/slow"))))
        (dolist (batch (reverse batches))
          (gb:page-add-lines streamed batch))
        ;; The pause on the server splits the menu in two at least.
        (is (<= 2 (length batches)))
        (is (string= (gb:page-text (gb:outcome-page outcome)) (gb:page-text streamed)))))))

(test history-records-and-steps
  (with-data-directory ()
    (history-steps)))

(defun history-steps ()
  (let ((session (gb:make-session)))
    (gb:session-record session "gopher://a/")
    (gb:session-record session "gopher://b/")
    (gb:session-record session "gopher://c/")
    (is (string= "gopher://b/" (gb:session-back session)))
    (is (string= "gopher://a/" (gb:session-back session)))
    (is (null (gb:session-back session)))
    (is (string= "gopher://b/" (gb:session-forward session)))
    ;; Somewhere new from the middle drops what was ahead.
    (gb:session-record session "gopher://d/")
    (is (null (gb:session-forward session)))))

;;; Downloads -----------------------------------------------------------------------------

(test a-download-streams-into-its-file
  (with-data-directory (directory)
    (with-gopher-server (port)
      (let* ((path (format nil "~a/file" directory))
             (outcome (gb:session-download (gb:make-session) (gopher-url port "/9/file.bin")
                                           path :timeout 5)))
        (is (eq :saved (gb:outcome-kind outcome)))
        (is (string= "256" (gb:outcome-message outcome)))
        ;; Not held in memory.
        (is (null (g:response-body (gb:outcome-response outcome))))
        (is (equalp (coerce (loop for i below 256 collect i) '(vector (unsigned-byte 8)))
                    (with-open-file (in path :element-type '(unsigned-byte 8))
                      (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
                        (read-sequence octets in)
                        octets))))
        ;; The name has no extension, and the partial file's did not leak into it.
        (is (null (probe-file (format nil "~a.download" path))))))))

(test a-failed-download-leaves-nothing-behind
  (with-data-directory (directory)
    (with-gemini-server (port)
      (let* ((path (format nil "~a/missing.zip" directory))
             (outcome (gb:session-download (gb:make-session) (gemini-url port "/nowhere")
                                           path :timeout 5)))
        (is (eq :error (gb:outcome-kind outcome)))
        (is (search "Not here" (gb:outcome-message outcome)))
        (is (null (probe-file path)))
        (is (null (probe-file (format nil "~a.download" path))))))))

(test a-stopped-download-leaves-the-old-file-alone
  (with-data-directory (directory)
    (with-gopher-server (port)
      (let* ((path (format nil "~a/kept.txt" directory))
             (handle (g:make-request-handle))
             (result nil))
        (with-open-file (out path :direction :output) (write-string "old" out))
        (let ((thread (sb-thread:make-thread
                       (lambda ()
                         (setf result (gb:session-download (gb:make-session)
                                                           (gopher-url port "/9/stall")
                                                           path :handle handle :timeout 5))))))
          (sleep 0.3)
          (g:cancel-request handle)
          (sb-thread:join-thread thread))
        (is (eq :cancelled (gb:outcome-kind result)))
        (is (string= "old" (uiop:read-file-string path)))
        (is (null (probe-file (format nil "~a.download" path))))))))

(test too-large-to-show-is-offered-for-saving
  (with-data-directory ()
    (with-gopher-server (port)
      (let ((outcome (fetch (gopher-url port "/9/file.bin") :max-size 100)))
        (is (eq :too-large (gb:outcome-kind outcome)))
        (is (string= "file.bin" (gb:outcome-suggested-name outcome)))))))

(test gopher-files-are-known-before-they-are-fetched
  (with-data-directory ()
    (with-gopher-server (port)
      (let ((links (gb:page-links (gb:outcome-page (fetch (gopher-url port))))))
        (flet ((download-p (label)
                 (gb:download-link-p (find label links :key #'gb:link-label :test #'string=))))
          (is (download-p "A binary"))
          (is (not (download-p "A picture")))
          (is (not (download-p "A text file")))
          (is (not (download-p "A menu"))))))))

(test looking-back-does-not-step-back
  (with-data-directory ()
    (let ((session (gb:make-session)))
      (gb:session-record session "gopher://a/")
      (gb:session-record session "gopher://b/")
      (is (string= "gopher://a/" (gb:session-back-url session)))
      (is (string= "gopher://a/" (gb:session-back-url session)))
      (is (null (gb:session-forward-url session)))
      (is (string= "gopher://b/" (g:history-current (gb:session-history session)))))))

(test a-search-is-at-an-address-that-searches-again
  (with-data-directory ()
    (with-gopher-server (port)
      (let ((answered (fetch (gopher-url port "/7/search") :query "common lisp")))
        (is (string= (gopher-url port "/7/search%09common%20lisp") (gb:outcome-url answered)))
        (is (string= (gb:outcome-url answered) (gb:page-url (gb:outcome-page answered))))
        ;; And fetching that address searches, with no question.
        (let ((again (fetch (gb:outcome-url answered))))
          (is (eq :page (gb:outcome-kind again)))
          (is (search "You searched for common lisp" (gb:page-text (gb:outcome-page again)))))))
    (with-gemini-server (port)
      (let ((answered (fetch (gemini-url port "/input") :query "Ann Smith")))
        (is (string= (gemini-url port "/input?Ann%20Smith") (gb:outcome-url answered)))
        (is (eq :page (gb:outcome-kind (fetch (gb:outcome-url answered)))))))))

(test an-identity-is-shown-only-where-it-is-for
  (with-data-directory ()
    (with-gemini-server (port)
      (let ((session (gb:make-session)))
        (gb::create-identity "127.0.0.1" :path "/elsewhere" :name "Not here")
        (is (eq :certificate-required
                (gb:outcome-kind (gb:session-fetch session (gemini-url port "/identity") :timeout 5))))
        (gb::create-identity "127.0.0.1" :path "/identity" :name "Here")
        (let ((outcome (gb:session-fetch session (gemini-url port "/identity") :timeout 5)))
          (is (eq :page (gb:outcome-kind outcome)))
          (is (search "“Here”" (gb::page-info-text session outcome))))))))
