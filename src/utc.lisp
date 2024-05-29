;; UTC.lisp -- Time Conversion
;;
;; DM/RAL  2024/05/29 20:59:49 UTC
;; ------------------------------------------------------------------------

(in-package #:com.ral.astro.utc)

;; ------------------------------------------------------------------------

(defvar *ΔAT*    37)      ;; Leap seconds added to UTC to match TAI
(defvar *DUT1*   -0.0448) ;; = (UT1 - UTC) secs, was -44.8ms on 2023-06-14

(defconstant +TAI-OFFSET+  32.184) ;; secs, TT = TAI + +TAI-OFFSET+, never changes

(defun JD_UTC-to-UT1 (JD_UTC)
  (+ JD_UTC (secs *DUT1*)))

(defun JD_UT1-to-TT (JD_UT1)
  (let ((ΔT  (secs (+ +TAI-OFFSET+ *ΔAT*))))
    (+ JD_UT1 ΔT)
    ))

(defun JD_TT-to-UT1 (JD_TT)
  ;; Compute UT1 from TT (which itself came from UTC)
  (let ((ΔT  (secs (+ +TAI-OFFSET+ *ΔAT*))))
    (- JD_TT ΔT)
    ))

