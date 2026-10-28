;;;; text.lisp -- runs to attributed strings: fonts, colours, links, images.
;;;;
;;;; Each style's attributes are one NSDictionary, made the first time it is
;;;; asked for and kept.  They are made at run time only: a dictionary made in
;;;; the build would be a dangling pointer in the saved image, so MAIN calls
;;;; RESET-TEXT-STYLES before anything is drawn.

(in-package #:gopher-browser)

(defvar *style-attributes* (make-hash-table)
  "Style keyword -> a retained NSDictionary of its attributes.")
(defvar *attribute-keys* (make-hash-table :test #'equal)
  "AppKit attribute-name constant -> its NSString.")

(defparameter +body-size+ 15d0)
(defparameter +mono-size+ 13d0)

(defvar *text-scale* 1d0
  "The scale the cached styles were made at: TEXT-SCALE, read when they were
reset.")

(defun scaled (size)
  (* size *text-scale*))

(defun reset-text-styles ()
  "Forget every dictionary made so far.  For a fresh image, whose old pointers
mean nothing."
  (setf *style-attributes* (make-hash-table)
        *attribute-keys* (make-hash-table :test #'equal)
        *text-scale* (text-scale)))

(defun attribute-key (name)
  (or (gethash name *attribute-keys*)
      (setf (gethash name *attribute-keys*) (appkit-string-constant name))))

(defun system-font (size &optional (weight 0d0))
  (objc:invoke "NSFont" "systemFontOfSize:weight:" size weight))

(defun mono-font (size &optional (weight 0d0))
  (objc:invoke "NSFont" "monospacedSystemFontOfSize:weight:" size weight))

(defun italic (font)
  (objc:invoke (objc:invoke "NSFontManager" "sharedFontManager")
               "convertFont:toHaveTrait:" font +ns-italic-font-mask+))

(defun style-font (style)
  (case style
    ((:pre :tag :menu-link :number) (mono-font (scaled +mono-size+)))
    (:heading-1 (system-font (scaled 26d0) +ns-font-weight-bold+))
    (:heading-2 (system-font (scaled 21d0) +ns-font-weight-semibold+))
    (:heading-3 (system-font (scaled 17d0) +ns-font-weight-semibold+))
    (:quote (italic (system-font (scaled +body-size+))))
    (:notice (system-font (scaled 13d0)))
    (t (system-font (scaled +body-size+)))))

(defun style-color (style)
  (objc:invoke "NSColor"
               (case style
                 ((:tag :quote :notice :number) "secondaryLabelColor")
                 (:error "systemRedColor")
                 (t "labelColor"))))

(defun style-paragraph (style)
  "An NSParagraphStyle for STYLE: space between paragraphs of prose, none
between lines of a menu or of preformatted text, and indents for quotes and
list items."
  (let ((paragraph (objc:invoke (objc:invoke "NSMutableParagraphStyle" "alloc") "init")))
    (case style
      (:tag
       ;; A menu line starts with its tag, so this is the menu line's style: a
       ;; label too long for the window wraps under itself, not under the tag.
       (objc:invoke paragraph "setHeadIndent:"
                    (* +tag-width+ (aref (objc:invoke (mono-font (scaled +mono-size+))
                                                      "maximumAdvancement")
                                         0))))
      ((:pre :menu-link :error) nil)
      ((:heading-1 :heading-2 :heading-3)
       (objc:invoke paragraph "setParagraphSpacingBefore:" 10d0)
       (objc:invoke paragraph "setParagraphSpacing:" 6d0))
      (:quote
       (objc:invoke paragraph "setFirstLineHeadIndent:" 18d0)
       (objc:invoke paragraph "setHeadIndent:" 18d0)
       (objc:invoke paragraph "setParagraphSpacing:" 6d0))
      (:list
       ;; The bullet hangs: wrapped lines start under the text, not the bullet.
       (objc:invoke paragraph "setFirstLineHeadIndent:" 6d0)
       (objc:invoke paragraph "setHeadIndent:" 24d0)
       (objc:invoke paragraph "setParagraphSpacing:" 3d0))
      (t
       (objc:invoke paragraph "setParagraphSpacing:" 6d0)))
    (objc:autorelease paragraph)))

(defun make-style-attributes (style)
  (let ((dictionary (objc:invoke "NSMutableDictionary" "dictionary")))
    (objc:invoke dictionary "setObject:forKey:" (style-font style)
                 (attribute-key "NSFontAttributeName"))
    (objc:invoke dictionary "setObject:forKey:" (style-color style)
                 (attribute-key "NSForegroundColorAttributeName"))
    (objc:invoke dictionary "setObject:forKey:" (style-paragraph style)
                 (attribute-key "NSParagraphStyleAttributeName"))
    (objc:retain dictionary)))

(defun style-attributes (style)
  (or (gethash style *style-attributes*)
      (setf (gethash style *style-attributes*) (make-style-attributes style))))

(defun link-value (index)
  "What a run's link attribute holds: the index of the link in the page, as a
string.  The text view hands it back when the link is clicked."
  (format nil "gopher-browser-link:~d" index))

(defun link-index (value)
  "The index LINK-VALUE put in VALUE, or NIL."
  (let ((string (ns-string value))
        (prefix "gopher-browser-link:"))
    (and string
         (> (length string) (length prefix))
         (string= prefix string :end2 (length prefix))
         (ignore-errors (parse-integer string :start (length prefix))))))

(defun run-attributes (run links)
  "The attributes of RUN: its style's, plus the link and its tooltip if it is
a link.  Autoreleased when it is new."
  (let ((base (style-attributes (run-style run)))
        (index (run-link run)))
    (if (null index)
        base
        (let ((dictionary (objc:autorelease (objc:invoke base "mutableCopy")))
              (link (aref links index)))
          (objc:invoke dictionary "setObject:forKey:" (link-value index)
                       (attribute-key "NSLinkAttributeName"))
          (objc:invoke dictionary "setObject:forKey:" (link-url link)
                       (attribute-key "NSToolTipAttributeName"))
          dictionary))))

(defun utf-16-length (string)
  "STRING's length as an NSString counts it: in UTF-16 units, so a character
beyond the Basic Multilingual Plane (an emoji) is two."
  (loop for c across string sum (if (> (char-code c) #xFFFF) 2 1)))

(defun runs-attributed-string (runs links &key (offset 0) (numbers (link-numbers-p)))
  "An NSMutableAttributedString of RUNS (a sequence), whose links index LINKS.
Autoreleased.

The second value says where each link went: a list of (INDEX START . LENGTH),
START counted in UTF-16 units from OFFSET, which is where the string will be
put.  With NUMBERS, each link is drawn after its number, [1] onwards, the
number not part of the link."
  (let ((result (objc:autorelease
                 (objc:invoke (objc:invoke "NSMutableAttributedString" "alloc") "init")))
        (position offset)
        (ranges '()))
    (flet ((add (text attributes)
             (let ((piece (objc:invoke (objc:invoke "NSAttributedString" "alloc")
                                       "initWithString:attributes:" text attributes)))
               (objc:invoke result "appendAttributedString:" piece)
               (objc:release piece))
             (incf position (utf-16-length text))))
      (map nil (lambda (run)
                 (let ((index (run-link run)))
                   (when index
                     (when (and numbers (not (assoc index ranges)))
                       (add (format nil "[~d] " (1+ index)) (style-attributes :number)))
                     (let ((existing (assoc index ranges))
                           (length (utf-16-length (run-text run))))
                       (if existing
                           (incf (cddr existing) length)
                           (push (list* index position length) ranges))))
                   (add (run-text run) (run-attributes run links))))
           runs))
    (values result (nreverse ranges))))

;;; Images ----------------------------------------------------------------------------

(defun octets-data (octets)
  "An NSData holding a copy of OCTETS.  Autoreleased."
  (let ((vector (coerce octets '(simple-array (unsigned-byte 8) (*)))))
    (cffi:with-pointer-to-vector-data (pointer vector)
      (objc:invoke "NSData" "dataWithBytes:length:" pointer (length vector)))))

(defun image-attributed-string (octets max-width)
  "An attributed string holding the image in OCTETS, no wider than MAX-WIDTH,
or NIL if AppKit cannot read it.  Autoreleased."
  (let ((image (objc:invoke (objc:invoke "NSImage" "alloc") "initWithData:"
                            (octets-data octets))))
    (unless (null-object-p image)
      (objc:autorelease image)
      (let* ((size (objc:invoke image "size"))
             (width (aref size 0))
             (height (aref size 1)))
        (when (and (plusp width) (> width max-width))
          (objc:invoke image "setSize:"
                       (vector max-width (* height (/ max-width width))))))
      (let ((attachment (objc:autorelease
                         (objc:invoke (objc:invoke "NSTextAttachment" "alloc") "init"))))
        (objc:invoke attachment "setImage:" image)
        (objc:invoke "NSAttributedString" "attributedStringWithAttachment:" attachment)))))

(defun page-attributed-string (page max-width)
  "Everything PAGE shows, as one attributed string.  Autoreleased.  The second
value is where its links are, as RUNS-ATTRIBUTED-STRING says."
  (let ((image (and (page-image page)
                    (image-attributed-string (page-image page) max-width))))
    (cond (image (values image '()))
          ((page-image page)
           (runs-attributed-string
            (list (make-run (format nil "This image could not be shown (~a, ~:d bytes).~%"
                                    (or (page-mime-type page) "unknown type")
                                    (length (page-image page)))
                            :notice))
            #()))
          (t (runs-attributed-string (page-runs page) (page-links page))))))
