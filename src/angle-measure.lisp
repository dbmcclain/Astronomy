;; angle-measure.lisp
;;
;; DM/RAL  2024/05/20 06:04:16 UTC
;; ----------------------------------

(in-package #:com.ral.astro.angle.measure)

;; ----------------------------------
;;
;; Lisp is *not* strongly typed. Rather than defining our own wrapper
;; objects to encapsulate data of various angular measures, defining
;; an ad-hoc type system, we instead operate with a common angle
;; measure behind the scenes. All angle input, of various measure, are
;; immediately converted into the background angle measure. You can
;; ask for a copy of that measure expressed in any of the cooperating
;; units.
;;
;; This avoids having to override basic arithmetic operations like
;; addition, subtraction, and having to deal with arithmetic between
;; mixed units among the operands. Background angle measure is still a
;; REAL, and ordinary arithmetic can be applied to it. This is not as
;; safeguarding as a strongly typed system, but represents a good
;; compromise for us.
;;
;; ----------------------------------
;; Set Angle Mode: :DEG, :HRS, :RAD, :TURNS
;;
;; When computing on the sky, TURNS is a natural unit, but any unit
;; can assume the background role.
;;
;; The way this works is that all angle entry are referred back to
;; Turns. And TURNS decides what the native representation is, by
;; multiplying the incoming Turns measures by the scale factor stored
;; in *1Turn*.
;;
;; If *1Turn* = 2*Pi then you are running Radian measure as the
;; canonical measure. Else, if set to 360 then you are using Degrees
;; as the measure. etc. etc... You just have to state how many of your
;; chosen units represents 1 Turn.
;;
;; Similarly, on output, every item is converted to Turns before
;; scaling up for the display measure. And TO-TURNS uses that scale
;; factor to convert the internal measure to true Turns.
;;
;; The whole system is internally consistent and only needs changing
;; in just one place to switch to another unit of measure for the
;; canonical representation.
;;
;; By doing things this way, each unit of measure only needs to know
;; how to compute the equivalent number of Turns for itself. No
;; measure needs to know anything about all the other measures that
;; may be present in the system.
;;
;; You can literally set the value of *1Turn* to anything that you
;; like. But whenever its value is changed, you should recompile this
;; body of code to have all the DEFVARs and reader-macros set up
;; correct values for internal constants in the code.

(defvar *1turn*)

(defun set-ang-mode (mode)
  (setf *1turn*
        (ecase mode
          (:deg   360.)
          (:hrs    24.)
          (:rad   #.(* 2. pi))
          (:turns   1))
        ))

(unless (boundp '*1turn*)
  (set-ang-mode :turns)
  ;; (set-ang-mode :rad)
  )

;; ----------------------------------
;; Angle measure introductions

(defun turns (x)
  (* *1turn* x))

(defun deg (x)
  (turns (/ x 360.)))

(defun arcmin (x)
  (deg (/ x 60.)))

(defun arcsec (x)
  (arcmin (/ x 60.)))

(defun mas (x)
  (arcsec (/ x 1000.)))

(defun hrs (x)
  (turns (/ x 24.)))

(defun mins (x)
  (hrs (/ x 60.)))

(defun secs (x)
  (mins (/ x 60.)))

(defun rad (x)
  (turns (/ x 2. pi)))

(defun mrad (x)
  (rad (* 1d-3 x)))

(defun μrad (x)
  (mrad (* 1d-3 x)))

;; ----------------------------------
;; Conversion of the background measure into various units.
;; Turns, Degrees, Radians, Hours
;;
;; 1 turn = 360 deg = 2 Pi rad = 24 hrs

(defun to-turns (x)
  (/ x *1turn*))

(defun to-deg (x)
  (* (to-turns x) 360.))

(defun to-arcmin (x)
  (* 60. (to-deg x)))

(defun to-arcsec (x)
  (* 60. (to-arcmin x)))

(defun to-mas (x)
  (* 1000. (to-arcsec x)))

(defun to-hrs (x)
  (* (to-turns x) 24.))

(defun to-mins (x)
  (* (to-hrs x) 60.))

(defun to-secs (x)
  (* (to-mins x) 60.))

(defun to-rad (x)
  (* (to-turns x) 2. pi))

(defun to-mrad (x)
  (* (to-rad x) 1d3))

(defun to-μrad (x)
  (* (to-mrad x) 1d3))

#|
;; E.g.,
(to-arcsec (deg 1))  => 3600
(to-μrad (arcsec 1)) => 4.848
(to-deg (rad 1))     => 57.3
(to-rad (turns 1))   => 6.28
 |#

(defmacro to (ufn val)
  `(/ ,val (,ufn 1)))

#|
(to μrad (arcsec 1)) => 4.848
 |#

;; ---------------------------------
;; Override the trig functions to automatically provide conversions
;; between background measure and radians.

(defun sin (x)
  (cl:sin (to-rad x)))

(defun cos (x)
  (cl:cos (to-rad x)))

(defun tan (x)
  (cl:tan (to-rad x)))

(defun asin (x)
  (rad (cl:asin x)))

(defun acos (x)
  (rad (cl:acos x)))

(defun atan (y &optional x)
  (rad (cl:atan y x)))

(defun phase (x)
  (rad (cl:phase x)))

(defun cis (x)
  (cl:cis (to-rad x)))

#|
;; E.g.,
(sin (deg 30)) => 0.5
 |#

