;;;; servers.lisp -- a Gopher server and a Gemini server to browse in the tests.
;;;;
;;;; Real sockets on 127.0.0.1 and ephemeral ports, so the browser's whole path
;;;; -- the library's transport, TOFU, client certificates -- is exercised and
;;;; the suite needs no network.  The Gemini server's certificate is made by
;;;; the library's own MAKE-IDENTITY, once, into a temporary directory.

(in-package #:gopher-browser/tests)

(defstruct (test-server (:conc-name fixture-) (:constructor make-fixture)) socket thread port (stop nil))

(defun read-request-line (stream)
  "One line of octets from STREAM, as a string without its CR LF."
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          until (or (null byte) (= byte 10))
          do (vector-push-extend byte buffer))
    (when (and (plusp (length buffer)) (= 13 (aref buffer (1- (length buffer)))))
      (decf (fill-pointer buffer)))
    (babel:octets-to-string (coerce buffer '(simple-array (unsigned-byte 8) (*)))
                            :encoding :utf-8)))

(defun send (stream &rest things)
  "Write THINGS, strings as UTF-8 and octet vectors as they are."
  (dolist (thing things)
    (write-sequence (if (stringp thing) (babel:string-to-octets thing :encoding :utf-8) thing)
                    stream))
  (force-output stream))

(defun crlf (&rest lines)
  (format nil "~{~a~c~c~}" (loop for line in lines append (list line #\Return #\Linefeed))))

;;; TLS --------------------------------------------------------------------------------

(defvar *certificate-directory* nil)

(defun server-certificate ()
  "The fixture server's certificate and key paths, made on first use."
  (unless *certificate-directory*
    (let ((directory (sb-posix:mkdtemp "/tmp/gopher-browser-certs-XXXXXX")))
      (g:make-identity (format nil "~a/cert.pem" directory) (format nil "~a/key.pem" directory)
                       :common-name "localhost")
      (setf *certificate-directory* directory)))
  (values (format nil "~a/cert.pem" *certificate-directory*)
          (format nil "~a/key.pem" *certificate-directory*)))

(cffi:defcallback accept-any-certificate :int ((ok :int) (context :pointer))
  (declare (ignore ok context))
  1)

(defvar *client-certificate-context* nil)

(defun client-certificate-context ()
  "A context that asks for a client certificate and takes whatever it is
given, as Gemini servers do: it is an identity, not a credential."
  (or *client-certificate-context*
      (setf *client-certificate-context*
            (let ((context (cl+ssl:make-context :verify-mode cl+ssl:+ssl-verify-peer+)))
              (cffi:foreign-funcall "SSL_CTX_set_verify" :pointer context
                                    :int cl+ssl:+ssl-verify-peer+
                                    :pointer (cffi:callback accept-any-certificate) :void)
              context))))

;;; Serving ---------------------------------------------------------------------------

(defun serve-connection (client handler tls)
  (let ((stream nil))
    (unwind-protect
         (handler-case
             (let ((raw (usocket:socket-stream client)))
               (setf stream
                     (if tls
                         (multiple-value-bind (certificate key) (server-certificate)
                           (cl+ssl:with-global-context ((client-certificate-context))
                             (cl+ssl:make-ssl-server-stream raw :certificate certificate
                                                                :key key)))
                         raw))
               (funcall handler stream)
               (ignore-errors (force-output stream)))
           (error () nil))
      (when (and tls stream) (ignore-errors (close stream)))
      (ignore-errors (usocket:socket-close client)))))

(defun start-fixture (handler &key tls)
  "Serve HANDLER, a function of one binary stream, on an ephemeral port."
  (let* ((listen (usocket:socket-listen "127.0.0.1" 0 :reuse-address t :backlog 16
                                                      :element-type '(unsigned-byte 8)))
         (fixture (make-fixture :socket listen :port (usocket:get-local-port listen))))
    (setf (fixture-thread fixture)
          (sb-thread:make-thread
           (lambda ()
             (loop until (fixture-stop fixture)
                   do (when (usocket:wait-for-input listen :timeout 0.1 :ready-only t)
                        (let ((client (ignore-errors (usocket:socket-accept listen))))
                          (when client
                            (sb-thread:make-thread
                             (lambda () (serve-connection client handler tls))
                             :name "fixture connection")))))
             (ignore-errors (usocket:socket-close listen)))
           :name "fixture server"))
    fixture))

(defun stop-fixture (fixture)
  (setf (fixture-stop fixture) t)
  (ignore-errors (sb-thread:join-thread (fixture-thread fixture))))

(defmacro with-test-server ((var handler &rest keys) &body body)
  `(let ((,var (start-fixture ,handler ,@keys)))
     (unwind-protect (progn ,@body)
       (stop-fixture ,var))))

;;; Gopher -----------------------------------------------------------------------------

(defparameter +png+
  (coerce #(137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 1 0 0 0 1 8 6 0 0 0 31
            21 196 137 0 0 0 13 73 68 65 84 120 156 99 248 207 192 240 31 0 5 0 1 255 137
            153 61 29 0 0 0 0 73 69 78 68 174 66 96 130)
          '(vector (unsigned-byte 8)))
  "A one-pixel PNG.")

(defun gopher-item (type label selector &optional (host "127.0.0.1") (port 70))
  (format nil "~c~a~c~a~c~a~c~d" type label #\Tab selector #\Tab host #\Tab port))

(defun gopher-handler (port)
  "A Gopher server's handler, whose menu links point back at PORT."
  (lambda (stream)
    (let* ((request (read-request-line stream))
           (tab (position #\Tab request))
           (selector (subseq request 0 tab))
           (search (and tab (subseq request (1+ tab))))
           (fields (uiop:split-string request :separator (string #\Tab))))
      (flet ((item (type label selector) (gopher-item type label selector "127.0.0.1" port)))
        (cond
          ((member selector '("" "/") :test #'string=)
           (send stream (crlf (gopher-item #\i "Welcome to the fixture" "" "error.host" 1)
                              (item #\0 "A text file" "/hello.txt")
                              (item #\1 "A menu" "/sub")
                              (item #\7 "Search" "/search")
                              (item #\I "A picture" "/pic.png")
                              (item #\9 "A binary" "/file.bin")
                              (gopher-item #\h "The web" "URL:https://example.com/" "127.0.0.1" port)
                              (gopher-item #\3 "Something is wrong" "" "error.host" 1)
                              ".")))
          ;; Gopher+: a menu with an item with attributes and views, and a form.
          ((string= selector "/plus")
           (send stream (crlf (format nil "~a~c+" (item #\0 "About this server" "/about") #\Tab)
                              (format nil "~a~c?" (item #\0 "Survey" "/survey") #\Tab)
                              ".")))
          ((and (string= selector "/about") (equal (second fields) "!"))
           (send stream (crlf "+-1"
                              (format nil "+INFO: 0About this server~c/about~c127.0.0.1~c~d~c+"
                                      #\Tab #\Tab #\Tab port #\Tab)
                              "+ABSTRACT:"
                              " A fixture, for the tests."
                              "+ADMIN:"
                              " Admin: Fixture Keeper <keeper@example.org>"
                              " Mod-Date: <20261028120000>"
                              "+VIEWS:"
                              " text/plain: <1k>"
                              " text/plain Fr_FR: <1k>"
                              ".")))
          ((and (string= selector "/about") (equal (second fields) "+text/plain"))
           (send stream (crlf "+-1" "The plain view." ".")))
          ((and (string= selector "/survey") (equal (second fields) "!"))
           (send stream (crlf "+-1"
                              (format nil "+INFO: 0Survey~c/survey~c127.0.0.1~c~d~c?"
                                      #\Tab #\Tab #\Tab port #\Tab)
                              "+ASK:"
                              (format nil " Ask: Your name?~cAnonymous" #\Tab)
                              (format nil " Choose: Colour?~cred~cgreen" #\Tab #\Tab)
                              " AskL: Comments?"
                              " Note: Thanks!"
                              ".")))
          ;; A form whose data block is counted: answers, then a file.
          ((and (string= selector "/filesurvey") (equal (third fields) "1"))
           (let* ((header (read-request-line stream))
                  (count (parse-integer header :start 1))
                  (octets (make-array count :element-type '(unsigned-byte 8))))
             (read-sequence octets stream)
             (let ((text (babel:octets-to-string octets :encoding :utf-8)))
               (send stream (crlf "+-1"
                                  (format nil "You said: ~{~a~^|~}"
                                          (remove "" (uiop:split-string
                                                      (remove #\Return text)
                                                      :separator (string #\Newline))
                                                  :test #'string=))
                                  ".")))))
          ((and (string= selector "/survey") (equal (third fields) "1"))
           (let ((answers (loop for line = (read-request-line stream)
                                until (or (string= line ".") (string= line ""))
                                collect line)))
             (send stream (crlf "+-1" (format nil "You said: ~{~a~^|~}" answers) "."))))
          ((string= selector "/hello.txt")
           (send stream (crlf "Hello, Gopher." "Second line." ".")))
          ((string= selector "/sub")
           (send stream (crlf (gopher-item #\i "The submenu" "" "error.host" 1)
                              (item #\1 "Home" "/")
                              ".")))
          ((string= selector "/search")
           (send stream (crlf (gopher-item #\i (format nil "You searched for ~a" search)
                                           "" "error.host" 1)
                              ".")))
          ((string= selector "/pic.png")
           (send stream +png+))
          ((string= selector "/file.bin")
           (send stream (coerce (loop for i below 256 collect i) '(vector (unsigned-byte 8)))))
          ((string= selector "/slow")
           ;; A menu that takes its time: lines, a pause, more lines.
           (send stream (crlf (gopher-item #\i "First, quickly" "" "error.host" 1)))
           (sleep 1.0)
           (send stream (crlf (gopher-item #\i "Then, slowly" "" "error.host" 1) ".")))
          ((string= selector "/stall")
           (sleep 20))
          (t
           (send stream (crlf (gopher-item #\3 "Not found" "" "error.host" 1) "."))))))))

(defmacro with-gopher-server ((var) &body body)
  "Run BODY with VAR bound to the port of a running Gopher fixture."
  (let ((fixture (gensym "FIXTURE")) (port (gensym "PORT")))
    `(let* ((,port nil)
            (,fixture (start-fixture (lambda (stream) (funcall (gopher-handler ,port) stream)))))
       (setf ,port (fixture-port ,fixture))
       (unwind-protect (let ((,var ,port)) ,@body)
         (stop-fixture ,fixture)))))

(defun gopher-url (port &optional (path "/"))
  (format nil "gopher://127.0.0.1:~d~a" port path))

;;; Gemini ------------------------------------------------------------------------------

(defvar *last-upload* nil
  "What the Gemini fixture was last sent with Titan: (path mime token . octets).")

(defun titan-upload (stream url)
  "Take a Titan upload for URL from STREAM, and answer it as a Titan server does."
  (multiple-value-bind (parameters path) (g:url-titan-parameters url)
    (let* ((size (parse-integer (or (cdr (assoc "size" parameters :test #'string=)) "0")))
           (octets (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      (let ((token (cdr (assoc "token" parameters :test #'string=))))
        (cond ((and (search "/locked" path) (not (equal token "secret")))
               (send stream (crlf "59 A token is needed here")))
              (t
               (setf *last-upload* (list* path (cdr (assoc "mime" parameters :test #'string=))
                                          token octets))
               (send stream (crlf (format nil "30 gemini://127.0.0.1:~d/uploaded"
                                          (g:url-port url))))))))))

(defvar *gopher-port-for-redirect* nil
  "Where the Gemini fixture's /to-gopher redirect goes.")

(defun gemini-handler (stream)
  (let* ((request (read-request-line stream))
         (url (g:parse-url request))
         (path (or (g:url-path url) "/"))
         (query (g:url-query url)))
    (flet ((gemtext (&rest lines)
             (send stream (crlf "20 text/gemini") (format nil "~{~a~%~}" lines))))
      (cond
        ((string-equal "titan" (g:url-scheme url))
         (titan-upload stream url))
        ((string= path "/uploaded")
         (gemtext "# Uploaded"
                  (if *last-upload*
                      (babel:octets-to-string (cdddr *last-upload*) :encoding :utf-8)
                      "Nothing yet.")))
        ((member path '("" "/") :test #'string=)
         (gemtext "# Welcome" "Some text." "=> /two The second page" "* an item" "> a quote"))
        ((string= path "/two")
         (gemtext "# Two" "The second page."))
        ((string= path "/input")
         (if query
             (gemtext "# Hello" (format nil "Hello, ~a." (g:percent-decode query)))
             (send stream (crlf "10 What is your name?"))))
        ((string= path "/secret")
         (if query
             (gemtext "# Secret" "Accepted.")
             (send stream (crlf "11 Password"))))
        ((string= path "/elsewhere")
         (send stream (crlf "31 https://example.com/")))
        ((string= path "/to-gopher")
         (send stream (crlf (format nil "30 gopher://127.0.0.1:~d/0/hello.txt"
                                    *gopher-port-for-redirect*))))
        ((string= path "/identity")
         (if (let ((x509 (cl+ssl::ssl-stream-x509-certificate stream)))
               (and x509 (not (cffi:null-pointer-p x509))))
             (gemtext "# Hello, you with a certificate")
             (send stream (crlf "60 Who are you?"))))
        ((string= path "/file.zip")
         (send stream (crlf "20 application/zip") (coerce #(80 75 5 6 0 0) '(vector (unsigned-byte 8)))))
        (t
         (send stream (crlf "51 Not here")))))))

(defmacro with-gemini-server ((var) &body body)
  "Run BODY with VAR bound to the port of a running Gemini fixture."
  (let ((fixture (gensym "FIXTURE")))
    `(let ((,fixture (start-fixture #'gemini-handler :tls t)))
       (unwind-protect (let ((,var (fixture-port ,fixture))) ,@body)
         (stop-fixture ,fixture)))))

(defun gemini-url (port &optional (path "/"))
  (format nil "gemini://127.0.0.1:~d~a" port path))

;;; Gopher over TLS -------------------------------------------------------------------------

(defmacro with-tls-gopher-server ((var) &body body)
  "Run BODY with VAR bound to the port of a Gopher fixture that speaks only TLS."
  (let ((fixture (gensym "FIXTURE")) (port (gensym "PORT")))
    `(let* ((,port nil)
            (,fixture (start-fixture (lambda (stream) (funcall (gopher-handler ,port) stream))
                                     :tls t)))
       (setf ,port (fixture-port ,fixture))
       (unwind-protect (let ((,var ,port)) ,@body)
         (stop-fixture ,fixture)))))
