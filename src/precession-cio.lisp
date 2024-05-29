;; precession-cio.lisp -- Precession and Nutation based on CIO transforms
;; 
;; DM/RAL  2024/05/29 20:20:15 UTC
;; ---------------------------------------------------------------------

(in-package #:com.ral.astro.precession.cio)

;; ------------------------------------------
;; IAU 2006 GCRS/CIRS Precession
;; ICRS Procedures - International Celestial Reference System
;; Ref, P.T.Wallace and N.Capitaine: "IAU 2006 precession-nutation procedures"

(defun CIP (epoch)
  ;; Compute CIP of epoch.
  ;; Approx, assumes small angle s=0
  (let* ((D  (d2k epoch))
         (Ω  (rad (+ 2.182d0 (* D -9.242d-4))))
         (cs (cis Ω))
         (X  (+ (* D 2.6603d-7)   (* -33.2d-6 (imagpart cs))))
         (Y  (+ (* D D -8.14d-14) (*  44.6d-6 (realpart cs)))))
    `(,X ,Y)
    ))
  
;; ------------------------------------------------------------

(defun preca (ra dec &optional (to-epoch (current-epoch)) (from-epoch +j2000+))
  ;; CIRS-based precession
  ;; 18 yr nutation + precession
  ;; On entry, RA and Dec should refer to an EQX-based position.
  (let ((*CIP-fn*  #'CIP))
    (mvb (rac decc) ;; convert to CIO-based position
        (eqx-to-cio ra dec from-epoch)
      (mvb (ra2k dec2k)  ;; unwind precession+nutation to reach J2000.0
          (CIRS-to-GCRS rac decc from-epoch)
        (mvb (rap decp) ;; apply precession+nutation for to-epoch
            (GCRS-to-CIRS ra2k dec2k to-epoch)
          (cio-to-eqx rap decp to-epoch) ;; convert to EQX-base position
          )))))

;; ------------------------------------------------------------
#|
;; Check from Wallace and Capitaine
;; For TT = 2400000.5+53750.892855138888889(JD)  20060115T21:24:37.5Z
;;   MCIO ≈ (( +1.00000000000000000 +0.00000000000000000 −0.00058224012792061)
;;           ( +0.00000000000000000 +1.00000000000000000 −0.00004374943683668)
;;           ( +0.00058224012792061 +0.00004374943683668 +1.00000000000000000))
;;
(let* ((tt (+ 2400000.5d0 53750.892855138888889d0))
       (jd (jd_ut1-to-tt
            (jdn 2006 01 15
                 :time (hms 21 24 37.5)
                 :lcl-ut 0))))
  (list (- tt 2400000.5)
        (- jd 2400000.5)
        (to-secs (- jd tt))
        (to-hms (frac (+ 1/2 tt)))
        (to-hms (frac (+ 1/2 jd)))
        (CIP tt)
        (M_CIO  (CIP tt))))
=>
(53750.89285513898  ;; tt
 53750.892901435495 ;; JD_TT
 4.000018537044525  ;; JD-tt secs
 (HMS 21 25 42.684) ;; tt
 (HMS 21 25 46.684) ;; JD
 ((1 0 -5.822401279206334E-4)
  (0 1 -4.374943683668478E-5)
  (5.822401279206334E-4 4.374943683668478E-5 1)))
;; ---------------
(let* ((ut1 (+ 2400000.5d0 53750.892855138888889d0 (/ 0.3341 +sec/day+))))
  (list
   :era (to-deg (ERA ut1))
   :era (to-hms (ERA ut1))
   (-
   (to-mas (ERA ut1))
   (to-mas
    (unipolar
     (turns (+ 0.7790572732640
               (* 1.00273781191135448
                  (- ut1 2451545.0))))
     )))))
|#

(defun R_TIRS (epoch)
  ;; Simplified precession, good to 0.12 arcsec in 21st cy,
  ;; good to 0.85 arcsec over ±2 cy
  ;;
  ;;  v_TIRS = R(TT,UT) . v_GCRS
  ;;  v_CIRS = M_CIO(TT) . v_GCRS
  ;;
  ;; This function is R(TT,UT).
  ;;
  ;; A positive rotation, R3, subtracts the ERA(epoch) from the RA.
  ;;
  (let* ((CIP   (CIP epoch))
         (M_CIO (M_CIO CIP))
         (ERA   (ERA epoch))
         (R3    (R3 ERA)))
    (mat-mulm R3 M_CIO)))

(defun GCRS-to-TIRS (ra dec &optional (epoch (current-epoch)))
  ;; On entry, RA and Dec should refer to a CIO-based J2000.0
  ;; position.  Computes:
  ;;
  ;;      (RA,Dec) -> Prec(RA,Dec,epoch) - ERA(epoch) = -HA(epoch).
  ;;
  ;; This is the -HA of the position at Greenwich on epoch. Done
  ;; entirely without needing knowledge of GST, nor the Equinox.
  (let* ((v_GCRS  (to-xyz ra dec))
         (R_TIRS  (R_TIRS epoch))
         (v_TIRS  (mat-mulv R_TIRS v_GCRS)))
    (to-thphi v_TIRS)
    ))

(defun TIRS-to-GCRS (ra dec &optional (epoch (current-epoch)))
  ;; On entry, RA and Dec should refer to a CIO-based position. This
  ;; computes CIO-based RA, Dec at J2000.0, given the RA = -HA at
  ;; Greenwich on epoch.
  (let* ((v_TIRS  (to-xyz ra dec))
         (R_TIRS  (trn (R_TIRS epoch)))
         (v_GCRS  (mat-mulv R_TIRS v_TIRS)))
    (to-thphi v_GCRS)
    ))

