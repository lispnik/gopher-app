;;;; swipe.lisp -- two fingers across the trackpad: Back or Forward.
;;;;
;;;; With "Swipe between pages" set to two fingers (the default), a swipe
;;;; arrives as a run of scroll events: a gesture that begins, changes, and
;;;; ends.  The text never scrolls sideways, so a gesture that is mostly
;;;; sideways and long enough is a swipe.  Like Safari: fingers moving right go
;;;; back, as if pulling the previous page in from the left.
;;;;
;;;; Which way the fingers moved depends on the scroll direction setting:
;;;; with natural scrolling (the event is "inverted from the device") the
;;;; deltas follow the fingers; without it they are the other way round.

(in-package #:gopher-browser)

(defconstant +phase-began+ 1)
(defconstant +phase-changed+ 4)
(defconstant +phase-ended+ 8)
(defconstant +phase-cancelled+ 16)

(defparameter +swipe-distance+ 120d0
  "How far, in points, the fingers must travel sideways to make a swipe.")

(defstruct swipe-tracker
  (active nil)
  (dx 0d0)
  (dy 0d0))

(defun track-scroll (tracker phase momentum-phase delta-x delta-y inverted)
  "Feed TRACKER one scroll event.  Answers :BACK or :FORWARD when the event
ends a swipe, otherwise NIL.  Momentum (the scroll coasting after the fingers
lift) and scrolls with no phase (a mouse wheel) are never swipes."
  (cond
    ((or (/= momentum-phase 0) (= phase 0)) nil)
    ((= phase +phase-began+)
     (setf (swipe-tracker-active tracker) t
           (swipe-tracker-dx tracker) (float delta-x 1d0)
           (swipe-tracker-dy tracker) (float delta-y 1d0))
     nil)
    ((not (swipe-tracker-active tracker)) nil)
    ((= phase +phase-changed+)
     (incf (swipe-tracker-dx tracker) delta-x)
     (incf (swipe-tracker-dy tracker) delta-y)
     nil)
    ((member phase (list +phase-ended+ +phase-cancelled+))
     (setf (swipe-tracker-active tracker) nil)
     (let* ((dx (swipe-tracker-dx tracker))
            (dy (swipe-tracker-dy tracker))
            (fingers (if inverted dx (- dx))))
       (when (and (= phase +phase-ended+)
                  (> (abs dx) +swipe-distance+)
                  (> (abs dx) (* 2 (abs dy))))
         (if (plusp fingers) :back :forward))))
    (t nil)))
