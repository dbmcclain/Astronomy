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

(defun sexi-in (d m s)
  (let ((sgn  (cond
               ((minusp d) -1)
               ((zerop d)
                (cond ((minusp m)
                       -1)
                      ((zerop m)
                       (cond ((minusp s) -1)
                             (t           1)))
                      (t  1)))
               (t  1))
              ))
  (* sgn
     (/ (+ (abs s) (* 60. (+ (abs m) (* 60. (abs d)))))
        3600.))
  ))

(defun dms (deg &optional (min 0) (sec 0))
  (deg (sexi-in deg min sec)))

(defun hms (hrs &optional (min 0) (sec 0))
  (hrs (sexi-in hrs min sec)))

#|
;; E.g.,
(to-μrad (dms 0 0 1)) => 4.848
(to-deg (hms 6 0 0)) => 90.0
 |#
;; -------------------------------------------
;; D.MS and H.MS forms

(defun dot-conv-in (x)
  (multiple-value-bind (d dfrac)
      (truncate x)
    (multiple-value-bind (m mfrac)
        (truncate (* 100. dfrac))
      (let ((s (* 100. mfrac)))
        (/ (+ s (* 60. (+ m (* 60. d)))) 3600.))
      )))

(defun d.ms (x)
  (deg (dot-conv-in x)))

(defun h.ms (x)
  (hrs (dot-conv-in x)))

#|
;; E.g.,
(to-μrad (d.ms 0.0001)) => 4.848
(to-deg  (h.ms 6.0000)) => 90.0
 |#
