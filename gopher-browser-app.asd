;;;; gopher-browser-app.asd -- "Gopher.app", built with asdf-macos-app.
;;;;
;;;;   make app        (or: sbcl --eval '(asdf:make "gopher-browser-app")' --quit)

(defsystem "gopher-browser-app"
  :defsystem-depends-on ("asdf-macos-app")
  :class :macos-app-system
  :build-operation "macos-app-op"
  :entry-point "gopher-browser:main"
  :description "A small-web browser, as an application."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("gopher-browser")
  :bundle-identifier "org.lispnik.gopher-browser"
  :bundle-name "Gopher"
  :bundle-executable "gopher-browser"
  ;; Drawn by tools/icon.lisp; asdf-macos-app makes the .icns from the PNG.
  :bundle-icon "res/icon.png"
  :bundle-principal-class "NSApplication"
  :bundle-category "public.app-category.productivity"
  :bundle-copyright "MIT"
  :bundle-log t
  ;; Links to these from other applications open here.
  :bundle-url-schemes ("gopher" "gophers" "gemini" "spartan" "nex" "guppy" "finger")
  ;; Merged against this file rather than the current directory, which is what
  ;; a bare "build/" would resolve against.
  :bundle-output-directory
  #.(merge-pathnames "build/"
                     (uiop:pathname-directory-pathname
                      (or *load-truename* *default-pathname-defaults*)))
  ;; Ad hoc unless told otherwise; an empty variable counts as unset.
  :code-signing-identity #.(let ((identity (uiop:getenv "MACOS_SIGNING_IDENTITY")))
                             (if (and identity (plusp (length identity)))
                                 identity
                                 "-")))
