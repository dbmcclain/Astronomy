;; angle-input.lisp
;;
;; DM/RAL  2024/05/20 06:05:10 UTC
;; ----------------------------------

(in-package #:com.ral.astro.angle.input)

;; ----------------------------------
;; Degrees, minutes, and seconds
;;   - or -
;; Hours, minutes, and seconds
;; ------------------------------------------
;; DMS & HMS list forms

(defgeneric sexi-in (d &optional m s)
  (:method ((d real) &optional (m 0) (s 0))
   (let ((sgn  (if (some #'minusp (list d m s))
                   -1
                 1)))
     (* sgn
        (/ (+ (abs s) (* 60. (+ (abs m) (* 60. (abs d)))))
           3600.))
     ))
  (:method ((d string) &optional m s)
   ;; intended for "±DD:MM:SS.ss"
   (declare (ignore m s))
   (/ (com.ral.useful-macros.reader-macros::convert-sexigisimal d)
      3600.)))

(defun dms (deg &optional (min 0) (sec 0))
  (deg (sexi-in deg min sec)))

(defun hms (hrs &optional (min 0) (sec 0))
  (hrs (sexi-in hrs min sec)))

#|
;; E.g.,
(to μrad (dms 0 0 1)) => 4.848
(to deg (hms 6 0 0)) => 90
(to μrad (dms "00:00:01")) => 4.848
(to deg (hms "06:00")) => 90
 |#
;; -------------------------------------------
;; D.MS and H.MS forms

(defun dot-conv-in (x)
  (multiple-value-bind (w f)
      (truncate (* 10000. (abs x)))
    (multiple-value-bind (q s)
        (truncate w 100.)
      (multiple-value-bind (d m)
          (truncate q 100.)
        (* (signum x)
           (/ (+ s f (* 60. (+ m (* 60. d))))
              3600.))
        ))))

(defun d.ms (x)
  (deg (dot-conv-in x)))

(defun h.ms (x)
  (hrs (dot-conv-in x)))

#|
;; E.g.,
(to μrad (d.ms 0.0001)) => 4.848
(to deg  (h.ms 6.0000)) => 90.0
 |#
