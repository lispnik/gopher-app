;;;; tools/icon.lisp -- the application icon, drawn with the objc bindings.
;;;;
;;;; A build tool, not part of the application: `make icon' loads it, and it
;;;; never enters the saved image.  The artwork is this file; res/icon.png is
;;;; what it draws, checked in so that building the bundle needs no display.
;;;;
;;;; The picture: a Gopher menu on an earth-brown tile -- a card of rows, each
;;;; an item-type tag and a label, the way the browser draws one.

(defpackage #:gopher-browser-icon
  (:use #:common-lisp)
  (:export #:render-icon #:main))

(in-package #:gopher-browser-icon)

(defparameter +appkit+
  "/System/Library/Frameworks/AppKit.framework/Versions/C/AppKit")

(defun color (red green blue &optional (alpha 1))
  (objc:invoke "NSColor" "colorWithSRGBRed:green:blue:alpha:"
               (float red 1d0) (float green 1d0) (float blue 1d0) (float alpha 1d0)))

(defun rect (x y width height)
  (vector (float x 1d0) (float y 1d0) (float width 1d0) (float height 1d0)))

(defun rounded-rect (x y width height radius)
  (objc:invoke "NSBezierPath" "bezierPathWithRoundedRect:xRadius:yRadius:"
               (rect x y width height) (float radius 1d0) (float radius 1d0)))

(defun fill-gradient (path top bottom)
  "Fill PATH with a gradient from TOP down to BOTTOM."
  (let ((gradient (objc:invoke (objc:invoke "NSGradient" "alloc")
                               "initWithStartingColor:endingColor:" bottom top)))
    ;; Ninety degrees is upwards, so the starting colour is the bottom one.
    (objc:invoke gradient "drawInBezierPath:angle:" path 90d0)
    (objc:release gradient)))

(defun draw-icon (size)
  "Draw the icon into the current context, SIZE points square."
  (let ((unit (/ size 1024d0)))
    (flet ((u (value) (* unit value)))
      ;; The tile, with the margin the system's own icons leave round it.
      (fill-gradient (rounded-rect (u 100) (u 100) (u 824) (u 824) (u 185))
                     (color 0.78 0.55 0.30) (color 0.50 0.31 0.14))
      ;; The card: a menu page, slightly shadowed.
      (objc:invoke (color 0 0 0 0.18) "setFill")
      (objc:invoke (rounded-rect (u 214) (u 196) (u 596) (u 620) (u 48)) "fill")
      (fill-gradient (rounded-rect (u 214) (u 212) (u 596) (u 620) (u 48))
                     (color 1 1 1) (color 0.94 0.91 0.86))
      ;; Its rows: a tag and a label each, the labels of different lengths.
      (loop for row from 0 below 5
            for y = (- 712 (* row 108))
            for label in '(380 300 340 250 320)
            for tag-color in (list (color 0.85 0.45 0.15) (color 0.20 0.50 0.85)
                                   (color 0.85 0.45 0.15) (color 0.35 0.62 0.30)
                                   (color 0.20 0.50 0.85))
            do (objc:invoke tag-color "setFill")
               (objc:invoke (rounded-rect (u 278) (u y) (u 92) (u 56) (u 14)) "fill")
               (objc:invoke (color 0.36 0.30 0.25) "setFill")
               (objc:invoke (rounded-rect (u 398) (u (+ y 12)) (u label) (u 32) (u 16)) "fill")))))

(defun render-icon (path &optional (size 1024))
  "Write the icon to PATH as a PNG, SIZE pixels square."
  (objc:ensure-objc-initialized :modules (list +appkit+))
  (objc:with-autorelease-pool ()
    (let ((rep (objc:invoke (objc:invoke "NSBitmapImageRep" "alloc")
                            "initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:"
                            (cffi:null-pointer) size size 8 4 t nil
                            "NSCalibratedRGBColorSpace" 0 0)))
      ;; Pushed and popped: NSColor and NSBezierPath draw into whichever context
      ;; is current, and one left current would catch the next drawing.
      (objc:invoke "NSGraphicsContext" "saveGraphicsState")
      (unwind-protect
           (progn
             (objc:invoke "NSGraphicsContext" "setCurrentContext:"
                          (objc:invoke "NSGraphicsContext"
                                       "graphicsContextWithBitmapImageRep:" rep))
             (draw-icon size))
        (objc:invoke "NSGraphicsContext" "restoreGraphicsState"))
      (let ((data (objc:invoke rep "representationUsingType:properties:" 4
                               (objc:invoke "NSDictionary" "dictionary"))))
        (ensure-directories-exist path)
        (unless (objc:invoke-bool data "writeToFile:atomically:" (namestring path) t)
          (error "Could not write ~a." path)))
      (objc:release rep)
      path)))

(defun main ()
  (format t "~&wrote ~a~%" (render-icon (merge-pathnames "res/icon.png" (uiop:getcwd)))))
