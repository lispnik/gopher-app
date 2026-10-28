;;;; dialogs.lisp -- asking: for input, for trust, and where to save.
;;;;
;;;; Each runs modally and answers when the person has.  They are called on the
;;;; main thread, from a method AppKit called, never from a fetch thread.

(in-package #:gopher-browser)

(defvar *dialog-answerer* nil
  "When set, a function that answers dialogs instead of a person: called with
the dialog's kind (:TEXT, :CONFIRM or :SAVE) and its message.  The tests set
it, since no test can click.")

(defun ask-for-text (message &key detail sensitive (button "OK"))
  "Ask for a line of text.  Answers the string, or NIL for Cancel.  SENSITIVE
hides what is typed, as for a password."
  (if *dialog-answerer*
      (funcall *dialog-answerer* :text message)
      (let ((alert (objc:invoke (objc:invoke "NSAlert" "alloc") "init"))
            (field (objc:invoke (objc:invoke (if sensitive "NSSecureTextField" "NSTextField")
                                             "alloc")
                                "initWithFrame:" #(0d0 0d0 320d0 24d0))))
        (unwind-protect
             (progn
               (objc:invoke alert "setMessageText:" message)
               (when detail (objc:invoke alert "setInformativeText:" detail))
               (objc:invoke alert "addButtonWithTitle:" button)
               (objc:invoke alert "addButtonWithTitle:" "Cancel")
               (objc:invoke alert "setAccessoryView:" field)
               (objc:invoke alert "layout")
               (objc:invoke (objc:invoke alert "window") "setInitialFirstResponder:" field)
               (when (= +ns-alert-first-button-return+ (objc:invoke alert "runModal"))
                 (or (objc:invoke-into 'string field "stringValue") "")))
          (objc:release field)
          (objc:release alert)))))

(defun confirm (message detail button)
  "Ask with an alert.  True if BUTTON, rather than Cancel, was pressed."
  (if *dialog-answerer*
      (funcall *dialog-answerer* :confirm message)
      (let ((alert (objc:invoke (objc:invoke "NSAlert" "alloc") "init")))
        (unwind-protect
             (progn
               (objc:invoke alert "setMessageText:" message)
               (objc:invoke alert "setInformativeText:" detail)
               (objc:invoke alert "addButtonWithTitle:" button)
               (objc:invoke alert "addButtonWithTitle:" "Cancel")
               (= +ns-alert-first-button-return+ (objc:invoke alert "runModal")))
          (objc:release alert)))))

(defun choose-save-path (suggested-name &optional (message "Save"))
  "Ask where to save, offering SUGGESTED-NAME.  Answers a native path, or NIL."
  (if *dialog-answerer*
      (funcall *dialog-answerer* :save suggested-name)
      (let ((panel (objc:invoke "NSSavePanel" "savePanel")))
        (objc:invoke panel "setNameFieldStringValue:" suggested-name)
        (objc:invoke panel "setMessage:" message)
        (when (= +ns-modal-response-ok+ (objc:invoke panel "runModal"))
          (let ((url (objc:invoke panel "URL")))
            (and (not (null-object-p url))
                 (objc:invoke-into 'string url "path")))))))

(defun open-externally (url)
  "Hand URL to whatever application handles its scheme.  True if one took it."
  (let ((ns-url (objc:invoke "NSURL" "URLWithString:" url)))
    (and (not (null-object-p ns-url))
         (objc:invoke-bool (objc:invoke "NSWorkspace" "sharedWorkspace") "openURL:" ns-url))))

(defun write-octets (path octets)
  (with-open-file (out (sb-ext:parse-native-namestring path)
                       :direction :output :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
    (write-sequence octets out))
  path)

;;; Forms: a modal window of labelled fields -------------------------------------------------
;;;
;;; Gopher+ forms, the Titan upload and Settings are all "fill these in, then
;;; OK or Cancel".  RUN-FORM builds the window from a list of FIELDs, runs it
;;; modally, and answers the values, or NIL for Cancel.

(defstruct (field (:constructor make-field (kind label &key ((:value initial)) choices placeholder)))
  "One row of a form.  KIND is :TEXT, :PASSWORD, :LINES (a multi-line text),
:CHOICE (a pop-up of CHOICES), :CHECK (a checkbox, LABEL beside it), :NOTE (a
label alone, with no value), :FILE (an existing file's path, with a Choose
button) or :SAVE-FILE (a new file's path, with a Choose button)."
  kind label (initial nil) choices placeholder
  (control nil))

(objc:define-objc-class form-target ()
  ((fields :initform #() :accessor form-fields
           :documentation "The form's fields: a Choose button's tag is its field's index."))
  (:objc-class-name "GopherBrowserFormTarget"))

(objc:define-objc-method ("formOK:" :void) ((self form-target) (sender objc:objc-object-pointer))
  (declare (ignorable self sender))
  (objc:invoke (objc.runloop:shared-application :activation-policy nil) "stopModalWithCode:" 1))

(objc:define-objc-method ("formCancel:" :void) ((self form-target) (sender objc:objc-object-pointer))
  (declare (ignorable self sender))
  (objc:invoke (objc.runloop:shared-application :activation-policy nil) "stopModalWithCode:" 0))

(objc:define-objc-method ("formChooseFile:" :void) ((self form-target) (sender objc:objc-object-pointer))
  (handler-case
      (let* ((field (aref (form-fields self) (objc:invoke sender "tag")))
             (save (eq (field-kind field) :save-file))
             (panel (if save
                        (objc:invoke "NSSavePanel" "savePanel")
                        (objc:invoke "NSOpenPanel" "openPanel"))))
        (unless save
          (objc:invoke panel "setCanChooseFiles:" t)
          (objc:invoke panel "setCanChooseDirectories:" nil))
        (when (= +ns-modal-response-ok+ (objc:invoke panel "runModal"))
          (let ((url (objc:invoke panel "URL")))
            (unless (null-object-p url)
              (objc:invoke (field-control field) "setStringValue:"
                           (objc:invoke-into 'string url "path"))))))
    (error (condition) (note "formChooseFile: ~a" condition))))

(defparameter +form-width+ 480d0)

(defun field-height (field)
  (ecase (field-kind field)
    ((:text :password :choice :file :save-file) 50d0)
    (:lines 130d0)
    (:check 28d0)
    (:note 34d0)))

(defun field-value (field)
  "What FIELD's control holds now."
  (let ((control (field-control field)))
    (ecase (field-kind field)
      ((:text :password :file :save-file) (or (objc:invoke-into 'string control "stringValue") ""))
      (:lines (or (objc:invoke-into 'string control "string") ""))
      (:choice (or (objc:invoke-into 'string control "titleOfSelectedItem") ""))
      (:check (= 1 (objc:invoke control "state")))
      (:note nil))))

(defun build-form-window (title fields target)
  "The form's window, its fields' controls made and recorded in FIELDS."
  (let* ((width +form-width+)
         (height (+ 70d0 (reduce #'+ fields :key #'field-height)))
         (window (objc:invoke (objc:invoke "NSWindow" "alloc")
                              "initWithContentRect:styleMask:backing:defer:"
                              (vector 0d0 0d0 width height) 1 ; titled
                              +ns-backing-store-buffered+ t))
         (content (objc:invoke window "contentView"))
         (y (- height 14d0)))
    (objc:invoke window "setReleasedWhenClosed:" nil)
    (objc:invoke window "setTitle:" title)
    (flet ((label (text frame &optional wrap)
             (let ((label (objc:invoke "NSTextField"
                                       (if wrap "wrappingLabelWithString:" "labelWithString:")
                                       text)))
               (objc:invoke label "setFrame:" frame)
               (objc:invoke content "addSubview:" label)
               label))
           (control (class frame)
             (let ((control (objc:invoke (objc:invoke class "alloc") "initWithFrame:" frame)))
               (objc:invoke content "addSubview:" control)
               (objc:release control)
               control)))
      (dolist (field fields)
        (decf y (field-height field))
        (let ((kind (field-kind field)) (inner (- width 40d0)))
          (unless (member kind '(:check))
            (label (field-label field)
                   (vector 20d0 (+ y (field-height field) -20d0) inner 18d0)
                   (eq kind :note)))
          (setf (field-control field)
                (ecase kind
                  ((:text :password)
                   (let ((c (control (if (eq kind :password) "NSSecureTextField" "NSTextField")
                                     (vector 20d0 (+ y 4d0) inner 24d0))))
                     (when (field-initial field) (objc:invoke c "setStringValue:" (field-initial field)))
                     (when (field-placeholder field) (objc:invoke c "setPlaceholderString:" (field-placeholder field)))
                     c))
                  ((:file :save-file)
                   (let ((c (control "NSTextField" (vector 20d0 (+ y 4d0) (- inner 110d0) 24d0)))
                         (button (objc:invoke "NSButton" "buttonWithTitle:target:action:"
                                              "Choose…" target
                                              (objc:coerce-to-selector "formChooseFile:"))))
                     (objc:invoke button "setFrame:" (vector (- width 126d0) y 106d0 32d0))
                     (objc:invoke button "setTag:" (position field fields))
                     (objc:invoke content "addSubview:" button)
                     (when (field-placeholder field) (objc:invoke c "setPlaceholderString:" (field-placeholder field)))
                     (when (field-initial field) (objc:invoke c "setStringValue:" (field-initial field)))
                     c))
                  (:lines
                   (let ((scroll (objc:invoke "NSTextView" "scrollableTextView")))
                     (objc:invoke scroll "setFrame:" (vector 20d0 (+ y 4d0) inner 100d0))
                     (objc:invoke scroll "setBorderType:" 2)
                     (objc:invoke content "addSubview:" scroll)
                     (let ((text (objc:invoke scroll "documentView")))
                       (objc:invoke text "setRichText:" nil)
                       (objc:invoke text "setFont:" (mono-font 12d0))
                       (objc:invoke text "setAutomaticQuoteSubstitutionEnabled:" nil)
                       (objc:invoke text "setAutomaticDashSubstitutionEnabled:" nil)
                       (when (field-initial field)
                         (objc:invoke text "setString:" (field-initial field)))
                       text)))
                  (:choice
                   (let ((c (control "NSPopUpButton" (vector 18d0 (+ y 2d0) (+ inner 4d0) 26d0))))
                     (dolist (choice (field-choices field))
                       (objc:invoke c "addItemWithTitle:" choice))
                     (when (field-initial field)
                       (objc:invoke c "selectItemWithTitle:" (field-initial field)))
                     c))
                  (:check
                   (let ((c (objc:invoke "NSButton" "checkboxWithTitle:target:action:"
                                         (field-label field) (cffi:null-pointer) (cffi:null-pointer))))
                     (objc:invoke c "setFrame:" (vector 18d0 (+ y 4d0) inner 20d0))
                     (objc:invoke c "setState:" (if (field-initial field) 1 0))
                     (objc:invoke content "addSubview:" c)
                     c))
                  (:note nil)))))
      (let ((ok (objc:invoke "NSButton" "buttonWithTitle:target:action:" "OK" target
                             (objc:coerce-to-selector "formOK:")))
            (cancel (objc:invoke "NSButton" "buttonWithTitle:target:action:" "Cancel" target
                                 (objc:coerce-to-selector "formCancel:"))))
        (objc:invoke ok "setFrame:" (vector (- width 112d0) 14d0 96d0 32d0))
        (objc:invoke ok "setKeyEquivalent:" (string #\Return))
        (objc:invoke cancel "setFrame:" (vector (- width 212d0) 14d0 96d0 32d0))
        (objc:invoke cancel "setKeyEquivalent:" (string (code-char 27)))
        (objc:invoke content "addSubview:" ok)
        (objc:invoke content "addSubview:" cancel)))
    (objc:invoke window "center")
    window))


(defun run-form (title fields &key (kind :form) (ok-title "OK"))
  "Show FIELDS in a modal window titled TITLE.  Answers the list of their
values in order (NIL for a :NOTE), or NIL if it was cancelled.

With *DIALOG-ANSWERER* set, it is asked instead, with KIND and FIELDS."
  (declare (ignorable ok-title))
  (if *dialog-answerer*
      (funcall *dialog-answerer* kind fields)
      (let* ((target (make-instance 'form-target))
             (pointer (progn (setf (form-fields target) (coerce fields 'vector))
                             (objc:objc-object-pointer target)))
             (window (build-form-window title fields pointer)))
        (unwind-protect
             (when (= 1 (objc:invoke (objc.runloop:shared-application) "runModalForWindow:" window))
               (mapcar #'field-value fields))
          (objc:invoke window "orderOut:" (cffi:null-pointer))
          (objc:release window)))))

;;; What the browser asks with forms -------------------------------------------------------

(defun question-field (question)
  "The form row for a Gopher+ question."
  (let ((choices (g:gopher-plus-question-choices question))
        (prompt (g:gopher-plus-question-prompt question)))
    (ecase (g:gopher-plus-question-kind question)
      (:ask (make-field :text prompt :value (first choices)))
      (:ask-password (make-field :password prompt))
      (:ask-long (make-field :lines prompt))
      ;; AskF: where to keep what the server sends back.  ChooseF: a file to send.
      (:ask-file (make-field :save-file prompt :value (first choices)
                             :placeholder "Where to save the reply"))
      (:choose-file (make-field :file prompt :placeholder "A file on this Mac to send"))
      ((:select :choose) (make-field :choice prompt :choices choices :value (first choices)))
      (:note (make-field :note prompt)))))

(defun file-name-of (path)
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path (1+ slash)) path)))

(defun ask-gopher-plus-form (title questions)
  "Ask QUESTIONS, a Gopher+ +ASK form.  Answers a FORM-REPLY, or NIL if it
was cancelled."
  (let ((values (run-form title (mapcar #'question-field questions) :kind :gopher-plus-form)))
    (when values
      (form-reply questions values))))

(defun show-info (title text)
  "Show TEXT in an alert titled TITLE."
  (if *dialog-answerer*
      (funcall *dialog-answerer* :info text)
      (let ((alert (objc:invoke (objc:invoke "NSAlert" "alloc") "init")))
        (unwind-protect
             (progn
               (objc:invoke alert "setMessageText:" title)
               (objc:invoke alert "setInformativeText:" text)
               (objc:invoke alert "addButtonWithTitle:" "OK")
               (objc:invoke alert "runModal"))
          (objc:release alert)))))
