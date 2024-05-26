;; siderial-time.lisp
;;
;; DM/RAL  2024/05/20 06:16:57 UTC
;; ----------------------------------

(in-package #:astro)

;; ----------------------------------------

(defun poly-eval (x coffs)
  ;; Horner's rule...
  (reduce (lambda (c acc)
            (+ c (* x acc)))
          coffs
          :from-end t
          :initial-value 0))

;; ------------------------------------------------------------------------
;;

(defun JD_UT1 (JD_TT)
  ;; Compute UT1 from TT (which itself came from UTC)
  (let ((ΔT  (+ +TAI-OFFSET+ *ΔAT* (- *DUT1*)))) ;; secs
    (- JD_TT (/ ΔT +sec/day+))
    ))

(defun JD_TT (JD_UTC)
  (+ JD_UTC (/ (+ +TAI-OFFSET+ *ΔAT*) +sec/day+)))

;; ----------------------------------------
;; Local Mean Siderial Time
;;
;; Validated to within 1ms against USNO, testing every year for 50
;; years, starting with 2000-05-24T00:00:00Z, and incrementing by 365 days.
;; Results (GMST - USNO): mean = -0.97ms, sigma = 0.028ms
;;
;; - DM/RAL 2024/05/25 12:53:56 UTC

(defun ERA (epochUTC)
  ;; Earth Rotation Angle
  (let* ((JD0  (+ 1/2 (floor (- epochUTC 1/2))))
         (H    (- epochUTC JD0))
         (Dut  (d2k JD0))
         (a0   #.(- 0.779_057_273_2640d0
                    1/2
                    (to-turns (arcsec 0.014506d0))))
         (a1  0.002_737_811_911_354_48d0))
    (unipolar
     (turns
      (+ a0
         (* Dut a1)
         (* H   a1)
         H
         )))
    ))

(defun GMST (epochUTC)
  ;; Greenwich mean siderial time
  (let* ((Tc   (c2k epochUTC))  ;; centuries from J2000
         (ERA  (ERA epochUTC))
         ;; precession of Equinox, arcsec/cent
         (prec (poly-eval Tc '(   0.014506d0
                               4612.156534d0
                                  1.3915817d0
                                 -0.00000044d0
                                 -0.000029956d0
                                 -0.0000000368d0))))
    (unipolar
     (+ ERA (arcsec prec)))
    ))

(defun LMST (&key (epoch (current-epoch))
                  (lon   *qth-lon*))
  (unipolar
   (+ (GMST epoch) lon)))

;; ----------------------------------------
