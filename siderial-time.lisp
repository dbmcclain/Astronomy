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

(defvar *ΔAT*    37)      ;; Leap seconds added to UTC to match TAI
(defvar *DUT1*   -0.0448) ;; = (UT1 - UTC) secs, was -44.8ms on 2023-06-14

(defconstant +TAI-OFFSET+  32.184) ;; secs, TT = TAI + +TAI-OFFSET+, never changes

(defun JD_UTC-to-UT1 (JD_UTC)
  (+ JD_UTC *DUT1*))

(defun JD_UTC-to-TT (JD_UTC)
  (+ JD_UTC (/ (+ +TAI-OFFSET+ *ΔAT*) +sec/day+)))

(defun JD_TT-to-UT1 (JD_TT)
  ;; Compute UT1 from TT (which itself came from UTC)
  (let ((ΔT  (+ +TAI-OFFSET+ *ΔAT* (- *DUT1*)))) ;; secs
    (- JD_TT (/ ΔT +sec/day+))
    ))

;; ----------------------------------------
;; Local Mean Siderial Time
;;
;; Validated to within 1ms against USNO, testing every year for 50
;; years, starting with 2000-05-24T00:00:00Z, and incrementing by 365 days.
;; Results (GMST - USNO): mean = -0.97ms, sigma = 0.028ms
;;
;; - DM/RAL 2024/05/25 12:53:56 UTC

(defun ERA (epochUT1)
  ;; Earth Rotation Angle
  (let* ((Dut  (d2k epochUT1)))
    (unipolar
     (turns
      (+ 0.779_057_273_2640d0
         (* Dut 0.002_737_811_911_354_48d0)
         (frac Dut)
         )))
    ))

(defun EO (epoch_TT)
  ;; Equation of Mean Equinox
  (let* ((Tc  (c2k epoch_TT))
         (prec (poly-eval Tc '(   0.014506d0
                               4612.156534d0
                                  1.3915817d0
                                 -0.00000044d0
                                 -0.000029956d0
                                 -0.0000000368d0))))
    (- (arcsec prec))))

(defun GMST (epochUT1)
  ;; Greenwich mean siderial time
  (let* ((ERA  (ERA epochUT1))
         (EO   (EO  epochUT1)))
    (unipolar
     (- ERA EO))
    ))

(defun LMST (&key (epoch (current-epoch))
                  (lon   *qth-lon*))
  (unipolar
   (+ (GMST epoch) lon)))

;; ----------------------------------------
