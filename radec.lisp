;; radec.lisp
;;
;; DM/RAL  2024/05/20 06:12:27 UTC
;; ----------------------------------

(in-package #:astro)

;; ----------------------------------

;; --------------------------------
;; Introducers for RA, Dec

(defun ra (hrs &optional (min 0) (sec 0))
  ;; convert RA to degs
  (hms hrs min sec))

(defun dec (deg &optional (min 0) (sec 0))
  ;; convert Dec to degs
  (dms deg min sec))

#|
(ra 15 59 30.1)
(dec 25 55 13)
|#
;; -------------------------------------------
;; Conversion of angle back to RA, DEC forms

(defun to-ra (x)
  (destructuring-bind (_ d m s)
      (to-hms (unipolar x))
    (declare (ignore _))
    `(ra ,d ,m ,s)))

(defun to-dec (x)
  (destructuring-bind (_ d m s)
      (to-dms (bipolar x))
    (declare (ignore _))
    `(dec ,d ,m ,s)
    ))

#|
(to-dec (deg 25.92028))
(to-ra  (deg 239.875417))
|#

(defun to-ra-h.ms (x)
  (to-h.ms (unipolar x)))

(defun to-dec-d.ms (x)
  (to-d.ms (bipolar x)))

