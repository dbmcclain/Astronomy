;; precession-cio-aa.lisp -- Precession and Nutation based on CIO transforms from Astronomical Almanac
;; 
;; DM/RAL  2024/05/29 20:20:15 UTC
;; ---------------------------------------------------------------------

(in-package #:com.ral.astro.precession.cio-aa)

;; ---------------------------------------------------
;;
;; From Explanatory Supplement to American Almanac
;; Is this better than M_CIO? Ans: Approx same + 1/2 yr nutation (0.5 arcsec)
;;
(defun GCRS-XY-aa-prec (epoch)
  (let* ((Tc  (c2k epoch))
         (X   (arcsec (+ (* Tc 2004.19d0)
                         (* Tc Tc -0.43d0)
                         )))
         (Y   (arcsec (+ (* Tc -0.03d0)
                         (* Tc Tc -22.41d0)
                         ))))
    `(,(to-rad X)  ;; ≈ (Sin X)
      ,(to-rad Y)) ;; ≈ (Sin Y)
    ))
  
(defun GCRS-XY-aa-nut (epoch)
  (let* ((Tc  (c2k epoch))
         (L   (deg (+ 280.5d0 (* Tc 36_000.8))))  ;; 1 yr period, Ecliptic lon of Sun
         (Ω   (deg (+ 125.0d0 (* Tc -1934.1d0)))) ;; 18.6 yr period
         (ΔX  (arcsec (+ (* -6.84d0 (sin Ω))
                         (* -0.52d0 (sin (+ L L))) ;; 1/2 yr period
                         )))
         (ΔY  (arcsec (+ (* 9.21d0 (cos Ω))
                         (* 0.57d0 (cos (+ L L))) 
                         ))))
    `(,(to-rad ΔX)  ;; ≈ (Sin X)
      ,(to-rad ΔY)) ;; ≈ (Sin Y)
    ))
  
(defun CIP (epoch)
  (mapcar #'+
          (GCRS-XY-aa-prec epoch)
          (GCRS-XY-aa-nut epoch)))

;; ------------------------------------------------------

(defun prec-aa (ra dec &optional (to-epoch (current-epoch)) (from-epoch +j2000+))
  ;; CIRS-based precession
  ;; On entry, RA and Dec should refer to an EQX-based position.
  ;; precession + 18 yr nutation + 0.5 yr nutation
  (let ((*CIP-fn* #'CIP))
    (mvb (rac decc) ;; convert to CIO-based position
        (eqx-to-cio ra dec from-epoch)
      (mvb (ra2k dec2k)  ;; unwind precession+nutation to reach J2000.0
          (CIRS-to-GCRS rac decc from-epoch)
        (mvb (rap decp) ;; apply precession+nutation for to-epoch
            (GCRS-to-CIRS ra2k dec2k to-epoch)
          (cio-to-eqx rap decp to-epoch) ;; convert to EQX-base position
          )))))



