;; azel.lisp
;;
;; DM/RAL  2024/05/20 06:20:16 UTC
;; ----------------------------------

(in-package #:astro)

;; ----------------------------------

;; -----------------------------------------------------
;; Az/El coordinates
;;
;; Az measured from North toward East.
;; 0deg Az = North, 90deg Az = East, 180deg Az = South, 270deg Az = West

(defun hadec-to-azel (ha dec &optional (lat *qth-lat*))
  (multiple-value-bind (az el)
      (rotx-ang (- ha (turns 0.25)) dec (- lat (turns 0.25)))
    (values (unipolar (- az (turns 0.25))) el)
    ))

(defun azel-to-hadec (az el &optional (lat *qth-lat*))
  (multiple-value-bind (ha dec)
      (rotx-ang (+ az (turns 0.25)) el (- (turns 0.25) lat))
    (values (bipolar (+ ha (turns 0.25))) dec)))

(defun radec-to-azel (ra dec &key (lat *qth-lat*) (epoch (current-epoch)))
  (hadec-to-azel (ra-to-ha ra epoch) dec lat))

(defun azel-to-radec (az el &key (lat *qth-lat*) (epoch (current-epoch)))
  (multiple-value-bind (ha dec)
      (azel-to-hadec az el lat)
    (values (ha-to-ra ha epoch) dec)))

#|
;; What RA,Dec is rising from the NE at El>40 deg?
(let ((az (deg 45))
      (el (deg 40)))
  (multiple-value-bind (ra dec)
      (azel-to-radec az el)
    (let* ((ha  (ra-to-ha ra))
           (ang (parallactic-angle ha dec)))
      (values (to-ra  ra)
              (to-dec dec)
              `(:HA ,(to-hms ha))
              `(:pa (deg ,(to-deg ang))))
      )))
|#
