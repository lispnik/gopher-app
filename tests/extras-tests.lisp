;;;; extras-tests.lisp -- Gopher+, Titan, the proxy, TLS on Gopher, Page Info
;;;; and View Source, without a window.

(in-package #:gopher-browser/tests)

(def-suite extras :in all-tests :description "What the library can do, offered by the browser.")
(in-suite extras)

;;; Gopher+ --------------------------------------------------------------------------------

(test gopher-plus-items-are-marked-in-menus
  (with-data-directory ()
    (with-gopher-server (port)
      (let* ((page (gb:outcome-page (fetch (gopher-url port "/1/plus"))))
             (links (gb:page-links page)))
        (is (search "TXT+ About this server" (gb:page-text page)))
        (is (search "TXT? Survey" (gb:page-text page)))
        (is (eq :plus (gb::link-plus (aref links 0))))
        (is (eq :ask (gb::link-plus (aref links 1))))))))

(test item-information-is-a-page-of-its-attributes
  (with-data-directory ()
    (with-gopher-server (port)
      (let* ((info-url (gb::gopher-plus-info-url (gopher-url port "/0/about")))
             (outcome (fetch info-url))
             (page (gb:outcome-page outcome))
             (text (gb:page-text page)))
        (is (gb::gopher-plus-info-url-p info-url))
        (is (eq :page (gb:outcome-kind outcome)))
        (is (string= "About this server" (gb:page-title page)))
        (is (search "A fixture, for the tests." text))
        (is (search "Admin: Fixture Keeper" text))
        (is (search "text/plain (Fr_FR) <1k>" text))
        ;; A view is a link, and following it fetches that view.
        (let* ((view (find "text/plain <1k>" (gb:page-links page)
                           :key #'gb:link-label :test #'string=))
               (shown (fetch (gb:link-url view))))
          (is (eq :page (gb:outcome-kind shown)))
          (is (search "The plain view." (gb:page-text (gb:outcome-page shown)))))))))

(test a-gopher-plus-form-is-asked-then-submitted
  (with-data-directory ()
    (with-gopher-server (port)
      (let* ((url (gopher-url port "/0/survey"))
             (asked (fetch url :form t)))
        (is (eq :form (gb:outcome-kind asked)))
        (is (equal '(:ask :choose :ask-long :note)
                   (mapcar #'g:gopher-plus-question-kind (gb::outcome-questions asked))))
        (let ((answered (fetch url :answers (list "Ada" "green" (list "one" "two")))))
          (is (eq :page (gb:outcome-kind answered)))
          (is (search "You said: +-1|Ada|green|2|one|two"
                      (gb:page-text (gb:outcome-page answered)))))))))

;;; Titan -----------------------------------------------------------------------------------

(test a-titan-upload-lands-on-the-page-it-made
  (with-data-directory ()
    (with-gemini-server (port)
      (setf *last-upload* nil)
      (let ((outcome (gb::session-upload (gb:make-session) (gemini-url port "/notes.gmi")
                                         (format nil "# Notes~%Written by a test.~%")
                                         :mime-type "text/gemini" :timeout 5)))
        (is (eq :page (gb:outcome-kind outcome)) "the library follows titan -> gemini")
        (is (search "Written by a test." (gb:page-text (gb:outcome-page outcome))))
        (is (string= "/notes.gmi" (first *last-upload*)))
        (is (string= "text/gemini" (second *last-upload*)))))))

(test a-titan-token-is-sent
  (with-data-directory ()
    (with-gemini-server (port)
      (let ((session (gb:make-session)))
        (is (eq :error (gb:outcome-kind
                        (gb::session-upload session (gemini-url port "/locked/a.txt") "x"
                                            :mime-type "text/plain" :timeout 5))))
        (is (eq :page (gb:outcome-kind
                       (gb::session-upload session (gemini-url port "/locked/a.txt") "x"
                                           :mime-type "text/plain" :token "secret"
                                           :timeout 5))))
        (is (equal "secret" (third *last-upload*)))))))

(test titan-addresses-for-pages
  (is (string= "titan://example.org/notes.gmi"
               (gb::titan-url-for "gemini://example.org/notes.gmi?x#y")))
  (is (null (gb::titan-url-for "gopher://example.org/")))
  (is (null (gb::titan-url-for nil))))

;;; The proxy --------------------------------------------------------------------------------

(test the-proxy-carries-every-request
  (with-data-directory ()
    (with-gopher-server (port)
      (is (eq :page (gb:outcome-kind (fetch (gopher-url port)))))
      ;; A proxy that is not there: the request fails, so it went to the proxy.
      ;; (A page not fetched before, or the cache would answer.)
      (gb::set-proxy (format nil "socks5://127.0.0.1:~d" (with-gopher-server (dead) dead)))
      (let ((outcome (fetch (gopher-url port "/1/sub"))))
        (is (eq :error (gb:outcome-kind outcome))))
      (gb::set-proxy "")
      (is (null (gb::proxy)))
      (is (eq :page (gb:outcome-kind (fetch (gopher-url port))))))))

(test only-socks5-proxies
  (with-data-directory ()
    (signals error (gb::set-proxy "http://127.0.0.1:8080"))
    (is (null (gb::proxy)))
    (gb::set-proxy " socks5://user:pw@127.0.0.1:9050 ")
    (is (string= "socks5://user:pw@127.0.0.1:9050" (gb::proxy)))))

;;; TLS on Gopher ----------------------------------------------------------------------------

(test gopher-tries-tls-first-and-falls-back
  (with-data-directory ()
    (g:forget-gopher-tls-status)
    (is (gb::gopher-tls-p) "on unless turned off")
    (with-tls-gopher-server (port)
      (let ((outcome (fetch (gopher-url port))))
        (is (eq :page (gb:outcome-kind outcome)))
        (is (g:response-certificate (gb:outcome-response outcome)) "it was TLS")))
    (with-gopher-server (port)
      (let ((outcome (fetch (gopher-url port))))
        (is (eq :page (gb:outcome-kind outcome)))
        (is (null (g:response-certificate (gb:outcome-response outcome))) "plain, as offered")))
    (gb::set-gopher-tls nil)
    (is (not (gb::gopher-tls-p)))
    (with-tls-gopher-server (port)
      ;; Plain Gopher to a server that speaks only TLS gets nowhere.
      (is (not (eq :page (gb:outcome-kind (fetch (gopher-url port)))))))))

;;; Page Info and View Source ----------------------------------------------------------------

(test page-info-says-how-a-page-came
  (with-data-directory ()
    (with-gemini-server (port)
      (let* ((session (gb:make-session))
             (outcome (gb:session-fetch session (gemini-url port) :timeout 5))
             (text (gb::page-info-text session outcome)))
        (is (search (format nil "Address: ~a" (gemini-url port)) text))
        (is (search "Protocol: Gemini" text))
        (is (search "Status: Success (20)" text))
        (is (search "Type: text/gemini" text))
        (is (search "encrypted (TLS)" text))
        (is (search "Certificate: sha256:" text))
        (is (search "Issued to: localhost" text))
        (is (search "Trusted on first use" text))
        (is (not (search "identity" text)))))
    (with-gopher-server (port)
      (let* ((session (gb:make-session))
             (text (gb::page-info-text session (gb:session-fetch session (gopher-url port)
                                                                :timeout 5))))
        (is (search "Connection: not encrypted" text))
        (is (not (search "Certificate:" text)))))
    (is (search "wrote itself" (gb::page-info-text (gb:make-session) (fetch "about:start"))))))

(test view-source-shows-what-was-sent
  (with-data-directory ()
    (with-gemini-server (port)
      (let* ((url (concatenate 'string "view-source:" (gemini-url port)))
             (outcome (fetch url))
             (page (gb:outcome-page outcome)))
        (is (eq :page (gb:outcome-kind outcome)))
        (is (string= url (gb:page-url page)))
        (is (search "=> /two The second page" (gb:page-text page)))
        (is (zerop (length (gb:page-links page))) "source has no links")))
    (is (eq :error (gb:outcome-kind (fetch "view-source:about:start"))))))

;;; Gopher+ forms with files -------------------------------------------------------------------

(test a-chosen-file-is-sent-with-the-answers
  (with-data-directory (directory)
    (with-gopher-server (port)
      (let ((file (format nil "~a/attached.txt" directory)))
        (with-open-file (out file :direction :output)
          (format out "attached words~%"))
        (let ((outcome (fetch (gopher-url port "/0/filesurvey")
                              :answers (gb::make-form-reply :answers (list "Ada" "attached.txt")
                                                            :file file))))
          (is (eq :page (gb:outcome-kind outcome)))
          (is (search "Ada|attached.txt|attached words"
                      (gb:page-text (gb:outcome-page outcome)))))))))

(test changing-the-proxy-forgets-the-cache
  (with-data-directory ()
    (with-gopher-server (port)
      (let ((session (gb:make-session)))
        (gb:session-fetch session (gopher-url port) :timeout 5)
        (is (= 1 (g:cache-count (gb::session-cache session))))
        (gb::clear-shared-cache)
        (is (= 0 (g:cache-count (gb::session-cache session))))))))
