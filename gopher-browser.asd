;;;; gopher-browser.asd -- a small-web browser for macOS, its logic, and its tests.
;;;;
;;;; The bundle is gopher-browser-app.asd.  It is a file of its own because
;;;; :DEFSYSTEM-DEPENDS-ON is resolved when a .asd is read, and loading the
;;;; browser should not need asdf-macos-app.

(defsystem "gopher-browser/core"
  :description "What the browser does, without AppKit: pages, navigation,
bookmarks and settings.  Testable anywhere."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("gopher" "alexandria" "bordeaux-threads" (:require :sb-posix))
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "settings")
                             (:file "bookmarks")
                             (:file "page")
                             (:file "history")
                             (:file "identities")
                             (:file "about")
                             (:file "navigation")
                             (:file "address")
                             (:file "swipe")
                             (:file "downloads"))))
  :in-order-to ((test-op (test-op "gopher-browser/tests"))))

(defsystem "gopher-browser"
  :description "A Gopher, Gemini, Spartan, Nex, Guppy and Finger browser in a
Cocoa window."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("gopher-browser/core" "objc" "cffi" "babel")
  :components ((:module "src/macos"
                :pathname "src/macos/"
                :serial t
                :components ((:file "frameworks")
                             (:file "main-thread")
                             (:file "text")
                             (:file "dialogs")
                             (:file "table")
                             (:file "window")
                             (:file "bookmarks-window")
                             (:file "downloads-window")
                             (:file "certificates-window")
                             (:file "identities-window")
                             (:file "app"))))
  :in-order-to ((test-op (test-op "gopher-browser/tests"))))

(defsystem "gopher-browser/tests"
  :description "The FiveAM suite."
  :depends-on ("gopher-browser" "fiveam" "usocket" "cl+ssl" "babel")
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "servers")
                             (:file "page-tests")
                             (:file "settings-tests")
                             (:file "navigation-tests")
                             (:file "browsing-tests")
                             (:file "extras-tests")
                             (:file "ui-tests"))))
  ;; FIVEAM:RUN! prints failures but returns NIL, and ASDF discards what a
  ;; TEST-OP returns, so the failure has to be an error.
  :perform (test-op (o c)
             (declare (ignore o c))
             (unless (uiop:symbol-call :gopher-browser/tests :run-tests)
               (error "The test suite failed."))))
