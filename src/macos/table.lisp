;;;; table.lisp -- what the Bookmarks and Downloads windows are built from.

(in-package #:gopher-browser)

(defun make-panel-window (title width height delegate)
  "A titled, resizable window that Lisp owns (it is not freed on closing)."
  (let ((window (objc:invoke (objc:invoke "NSWindow" "alloc")
                             "initWithContentRect:styleMask:backing:defer:"
                             (vector 0d0 0d0 width height)
                             +ns-window-style-mask+ +ns-backing-store-buffered+ t)))
    (objc:invoke window "setReleasedWhenClosed:" nil)
    (objc:invoke window "setTitle:" title)
    (objc:invoke window "setContentMinSize:" #(360d0 200d0))
    (objc:invoke window "setDelegate:" delegate)
    (objc:invoke window "center")
    window))

(defun add-table (content frame columns target &key (double-action nil))
  "A table in a scroll view filling FRAME of CONTENT.  COLUMNS are
(identifier title width editable).  Answers the table."
  (let ((scroll (objc:invoke (objc:invoke "NSScrollView" "alloc") "initWithFrame:" frame))
        (table (objc:invoke (objc:invoke "NSTableView" "alloc") "initWithFrame:"
                            (vector 0d0 0d0 (aref frame 2) (aref frame 3)))))
    (objc:invoke table "setStyle:" 1)       ; NSTableViewStyleFullWidth
    (loop for (key title width editable) in columns
          do (let ((column (objc:invoke (objc:invoke "NSTableColumn" "alloc")
                                        "initWithIdentifier:" key)))
               (objc:invoke column "setTitle:" title)
               (objc:invoke column "setWidth:" (float width 1d0))
               (objc:invoke column "setEditable:" editable)
               (objc:invoke column "setResizingMask:" 3)
               (objc:invoke table "addTableColumn:" column)
               (objc:release column)))
    (objc:invoke table "setUsesAlternatingRowBackgroundColors:" t)
    (objc:invoke table "setColumnAutoresizingStyle:" 1)
    (objc:invoke table "setDataSource:" target)
    (objc:invoke table "setDelegate:" target)
    (when double-action
      (objc:invoke table "setTarget:" target)
      (objc:invoke table "setDoubleAction:" (objc:coerce-to-selector double-action)))
    (objc:invoke scroll "setHasVerticalScroller:" t)
    (objc:invoke scroll "setAutohidesScrollers:" t)
    (objc:invoke scroll "setBorderType:" 2)
    (objc:invoke scroll "setAutoresizingMask:" (logior +flexible-width+ +flexible-height+))
    (objc:invoke scroll "setDocumentView:" table)
    (objc:invoke content "addSubview:" scroll)
    (objc:release table)
    (objc:release scroll)
    table))

(defun add-push-button (content title action frame mask target)
  (let ((button (objc:invoke "NSButton" "buttonWithTitle:target:action:"
                             title target (objc:coerce-to-selector action))))
    (objc:invoke button "setFrame:" frame)
    (objc:invoke button "setAutoresizingMask:" mask)
    (objc:invoke content "addSubview:" button)
    button))

(defun column-key (column)
  (if (null-object-p column)
      ""
      (or (objc:invoke-into 'string column "identifier") "")))

(defun selected-row (table)
  "The selected row, or NIL."
  (let ((row (objc:invoke table "selectedRow")))
    (and (>= row 0) row)))

(defun select-table-row (table row)
  (objc:invoke table "selectRowIndexes:byExtendingSelection:"
               (objc:invoke "NSIndexSet" "indexSetWithIndex:" row) nil)
  (objc:invoke table "scrollRowToVisible:" row))
