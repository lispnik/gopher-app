;;;; page-tests.lisp -- documents to runs: what each kind of line looks like.

(in-package #:gopher-browser/tests)

(def-suite pages :in all-tests :description "Documents as styled runs.")
(in-suite pages)

(defun gemtext-page (text &optional (url "gemini://example.org/"))
  (gb:document-page (g:parse-gemtext text (g:parse-url url)) url))

(defun menu-page (&rest lines)
  (let ((url "gopher://example.org/"))
    (gb:document-page (g:parse-gopher-menu (apply #'crlf (append lines '(".")))
                                           (g:parse-url url))
                      url)))

(defun styles (page)
  (map 'list #'gb:run-style (gb:page-runs page)))

(test gemtext-lines-get-their-styles
  (let ((page (gemtext-page (format nil "# One~%## Two~%### Three~%Prose.~%* item~%> quote~%```~%pre~%```~%"))))
    (is (equal '(:heading-1 :heading-2 :heading-3 :text :list :quote :pre) (styles page)))
    (is (string= "One" (gb:page-title page)))
    (is (search "•  item" (gb:page-text page)))))

(test gemtext-links-are-indexed-and-resolved
  (let* ((page (gemtext-page (format nil "=> /a First~%=> gopher://h/ Second~%=> /bare~%")))
         (links (gb:page-links page))
         (link-runs (coerce (remove nil (gb:page-runs page) :key #'gb:run-link) 'list)))
    (is (= 3 (length links)))
    (is (equal '(0 1 2) (map 'list #'gb:run-link link-runs)))
    (is (string= "gemini://example.org/a" (gb:link-url (aref links 0))))
    (is (string= "gopher://h/" (gb:link-url (aref links 1))))
    (is (string= "First" (gb:run-text (first link-runs))))
    ;; No label: the link as written stands in.
    (is (string= "/bare" (gb:run-text (third link-runs))))
    (is (every (lambda (link) (eq :follow (gb:link-kind link))) links))))

(test menus-are-monospaced-with-tags-that-line-up
  (let* ((page (menu-page (gopher-item #\i "Hello" "" "error.host" 1)
                          (gopher-item #\1 "Menu" "/m" "example.org" 70)
                          (gopher-item #\0 "Text" "/t" "example.org" 70)
                          (gopher-item #\7 "Find" "/f" "example.org" 70)
                          (gopher-item #\3 "Broken" "" "error.host" 1)))
         (text (gb:page-text page))
         (lines (uiop:split-string (string-right-trim '(#\Newline) text) :separator '(#\Newline))))
    (is (equal '("     Hello" "DIR  Menu" "TXT  Text" "ASK  Find" "     Broken") lines))
    (is (equal '(:pre :tag :menu-link :pre :tag :menu-link :pre :tag :menu-link :pre :error)
               (styles page)))
    (is (string= "gopher://example.org/1/m"
                 (gb:link-url (aref (gb:page-links page) 0))))))

(test spartan-prompts-are-input-links
  (let* ((page (gemtext-page (format nil "=: /search Search the site~%")
                             "spartan://example.org/"))
         (link (aref (gb:page-links page) 0)))
    (is (eq :input (gb:link-kind link)))
    (is (string= "Search the site" (gb:link-label link)))
    (is (eq :tag (first (styles page))))))

(test streaming-adds-to-a-page-a-batch-at-a-time
  (let* ((document (g:parse-gemtext (format nil "# T~%=> /a A~%text~%=> /b B~%")
                                    (g:parse-url "gemini://h/")))
         (lines (coerce (g:document-lines document) 'list))
         (page (gb::make-page "gemini://h/"))
         (first-runs (gb:page-add-lines page (subseq lines 0 2)))
         (rest-runs (gb:page-add-lines page (subseq lines 2))))
    (is (= 3 (length first-runs)))
    (is (= 3 (length rest-runs)))
    ;; The links are numbered across batches, as the whole document numbers them.
    (is (= 1 (gb:run-link (second rest-runs))))
    (is (string= (gb:page-text (gb:document-page document "gemini://h/"))
                 (gb:page-text page)))
    (is (= 4 (gb::page-line-count page)))))

(test message-pages-say-what-they-are-given
  (let ((page (gb:message-page "gemini://h/" "Could not load"
                               "Plain."
                               '(:error "It broke.")
                               '(:link "gemini://h/" "Try again"))))
    (is (equal '(:heading-1 :text :error :link :text) (styles page)))
    (is (string= "gemini://h/" (gb:link-url (aref (gb:page-links page) 0))))
    (is (string= "Could not load" (gb:page-title page)))))

(test titles-fall-back-to-the-address
  (is (string= "example.org/1/sub" (gb::url-title "gopher://example.org/1/sub")))
  (is (string= "example.org" (gb::url-title "gemini://example.org/"))))

(test file-names-for-saving
  (is (string= "file.zip" (gb::suggested-file-name "gemini://h/a/file.zip" "application/zip")))
  (is (string= "my file.txt" (gb::suggested-file-name "gopher://h/0/my%20file.txt" nil)))
  (is (string= "download.zip" (gb::suggested-file-name "gemini://h/" "application/zip; x=y")))
  (is (string= "download" (gb::suggested-file-name "gemini://h/" nil))))
