;; angle-measure.lisp
;;
;; DM/RAL  2024/05/20 06:04:16 UTC
;; ----------------------------------

(in-package #:angle)

;; ----------------------------------
;;
;; Lisp is *not* strongly typed. Rather than defining our own wrapper
;; objects to encapsulate data of various angular measures, defining
;; an ad-hoc type system, we instead operate with a common angle
;; measure behind the scenes. All angle input, of various measure, are
;; immediately converted into the background angle measure. You can
;; ask for a copy of that measure expressed in any of the cooperative
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

(defun hrs (x)
  (turns (/ x 24.0)))

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

