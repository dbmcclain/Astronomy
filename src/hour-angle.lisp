;; hour-angle.lisp
;;
;; DM/RAL  2024/05/20 06:18:44 UTC
;; ----------------------------------

(in-package #:com.ral.astro.ha)

;; -------------------------------------------------
;; Hour Angle (HA) +W/-E of Meridian

(defun ra-to-ha (ra &key (lon *qth-lon*) (epoch (current-epoch)))
  ;; Hour Angle of object RA
  ;; +W,-E of meridian
  (bipolar (- (lmst :epoch epoch :lon lon) ra)))

(defun ha-to-ra (ha &key (lon *qth-lon*) (epoch (current-epoch)))
  ;; Given HA find corresponding RA
  (unipolar (- (lmst :epoch epoch :lon lon) ha)))

#|
;; What RA is on the Meridian now?
(to-hms (lmst))
(>hms (lmst))

;; What is rising now, about 4 hours E of Meridian?
(to-ra (ha-to-ra (hms -4)))
(>hms  (ha-to-ra (hms -4)))

;; What is HA of RA 22h 50m?
(to-hms (ra-to-ha (ra 22 50)))
(>ha    (ra-to-ha (ra 22 50)))
|#



;; -------------------------------------------------
;; Parallactic Angle (E-,W+)

(defun parallactic-angle (ha dec &key (lat *qth-lat*))
  (atan (sin ha)
        (- (* (tan lat) (cos dec))
           (* (sin dec) (cos ha))
           )))

#|
(to-deg (parallactic-angle (hms -4) (dec 22)))
(um:fdpl 1 (to-deg (parallactic-angle (hms -4) (dec 22))))
|#

