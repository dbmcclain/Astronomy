;; siderial-time.lisp
;;
;; DM/RAL  2024/05/20 06:16:57 UTC
;; ----------------------------------

(in-package #:astro)

;; ------------------------------------------------------------------------
;; Local Mean Siderial Time
;;
;; What precision do we really need?
;;
;; DPFP has 53 bits of precision.
;; 0.1 arcsec resolution as fraction of a turn requires 24 bits.
;; A century interval from JD2000 requires 39 bits for 0.1 arcsec resolution.
;; Whole days for a century (= 36525) requires 16 bits.
;;
;; If we want whole days for a century, multiplied by some factor, to still
;; represent 0.1 arcsec resolution, then we need 24 + 16 = 40 bits of precision
;; in the multiplicative factor.
;;
;; Hence, we shouldn't worry about precision underflow as long as we
;; have 53 bits of precision.

(defun lmst0 (epoch)
  ;; Mean Sidereal time at Greenwich, expressed in degrees.
  ;; Mean time excludes nutation.
  ;; Definitional Epoch here is 1 Jan 2000 at 12:00 UT
  (let* ((deljd     (- epoch *j2000*))         ;; days since Epoch
         (tcent     (/ deljd *days-per-century*))         ;; centuries since Epoch
         (a0        #.(to-turns (deg 280.460_618_37)))    ;; turns offset at Epoch
         (a1-excess #.(to-turns (deg 0.985_647_366_29)))  ;; excess turns per day less 1
         (a2        #.(to-turns (deg 0.000_387_933)))     ;; 2nd ord turns per century
         (a3        #.(to-turns (deg (/ 38_710_000)))))   ;; 3rd ord turns per century
    ;; Compute residual turns to date from Epoch.
    ;; deljd = NDays + FracOfDay           ;; NDays in Ints, 0 <= FracOfDay < 1
    ;; a1    = (1 + eps) Turns, eps = 0.985... deg/360 excess turns per day
    ;; deljd * a1 = NDays * 1 Turn         ;; = 0 mod 1
    ;;              + NDays * eps          ;; accum frac likely > 1
    ;;              + FracOfDay * 1 Turn   ;; 0 <= FracOfDay < 1
    ;;              + FracOfDay * eps      ;; < 1
    ;; Doing it this way preserves precision.
    (multiple-value-bind (ndays frac-of-day)
        (truncate deljd)
      (multiple-value-bind (_ accum-frac-from-ndays)
          (truncate (* ndays a1-excess))
        (declare (ignore _))
        ;; Compute turns at epoch
        ;; all terms are: 0 <= term < 1
        (turns
         (+ a0
            accum-frac-from-ndays
            frac-of-day
            (* frac-of-day a1-excess)
            (* tcent tcent (- a2 (* a3 tcent)))
            ))
        ))))

#|
(jdn 2024 05 16 :hh 01 :mm 40 :ss 21)
(to-ra (lmst0 (jdn 2024 05 16 :hh 01 :mm 40 :ss 21)))

(jdn 2000 01 01 :LCL-UT 0 :hh 12)
|#

(defun lmst (&key (lon *qth-lon*) (epoch (current-epoch)))
  ;; Local Mean Sidereal Time at longitude long, expressed in degs.
  ;; = RA on Meridian
  (unipolar (+ (lmst0 epoch) lon)))
   
#|
(jdn 2024 05 15 :hh 01 :mm 40 :ss 21)
(to-ra (lmst (jdn 2024 05 15 :hh 01 :mm 40 :ss 21)))
(to-ra (lmst))
|#
