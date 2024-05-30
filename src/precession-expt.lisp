;; precession-expt.lisp - Experiments in Precession
;;
;; DM/RAL  2024/05/29 20:31:10 UTC
;; ------------------------------------------------------

(in-package #:com.ral.astro.precession.expt)

;; ------------------------------------------------------
;; Approx CIRS precession + nutation, with XY precession obtained from
;; more accurate long-term CIP model, and nutation obtained from AA
;; model.

(defun CIP-mn (epoch)
  (pequ (c2k epoch)))  ;; Long-term precession model
  
(defun CIP-ap (epoch)
  (mapcar #'+
          (CIP-mean epoch)
          (GCRS-XY-aa-Nut epoch))) ;; AA nutation

;; -------------------------------------------------

(defun aberration (epoch)
  (let* ((Tc  (c2k epoch))
         (L   (deg (+ 280.5d0 (* Tc 36_000.8))))  ;; 1 yr period, Ecliptic lon of Sun
         (cL  (cos L))
         (sL  (sin L)))
  (vscale #.(/ 173.24)
          `(,(*  0.0172 sL)
            ,(* -0.0158 cL)
            ,(* -0.0068 cL) ))
  ))

;; -----------------------------------------------
;; 3 basic scenarios -
;;    1. Precess mean J2000 position to mean position at some epoch.
;;    2. Precess mean J2000 position to apparent position at some epoch.
;;    3. Precess mean position at some epoch to mean J2000 position.
;;

(defun prec-CIRS-mn-to-GCRS-2k (vmn &optional (from-epoch (current-epoch)))
  ;; Precess mean position CIRS(x,y,z) from from-epoch to GCRS J2000.0
  (let* ((CIP    (CIP-mn from-epoch))
         (M_CIO  (M_CIO CIP)))
    (mat-mulv (trn M_CIO) vmn)))

(defun prec-GCRS-2k-to-CIRS-mn (v2k &optional (to-epoch (current-epoch)))
  ;; Precess mean J2000 GCRS(x,y,z) to mean CIRS(x,y,z) at to-epoch.
  (let* ((CIP   (CIP-mn to-epoch))
         (M_CIO (M_CIO CIP)))
    (mat-mulv M_CIO v2k)))

(defun prec-GCRS-2k-to-CIRS-ap (v2k &optional (to-epoch (current-epoch)))
  ;; Precess mean J2000 GCRS(x,y,z) to apparent CIRS(x,y,z) at to-epoch.
  (let* ((CIP   (CIP-ap to-epoch))
         (M_CIO (M_CIO CIP))
         (ab    (aberration to-epoch)))
    (vadd ab (mat-mulv M_CIO v2k))))

;; ----------------------------------------------
;; Presuming that J2000 catalogs are all GCRS coord (ie. not Equinox based).
;; Presuming that all other epochs of interest are Equinox based.
;; 

(defun EQX-to-CIRS-xyz (ra dec epoch)
  (to-xyz (+ ra (EO epoch)) dec))

(defun CIRS-xyz-to-EQX (vxyz epoch)
  (mvb (ra dec)
      (to-thphi vxyz)
    (values (to-ra (- ra (EO epoch)))
            (to-dec dec))))

;; -------------------------------------------------
;; Just like we do for angle measures, converting on entry to a common
;; measure, on entry RADEC converts to J2000 GCRS 3-vector.

(defun radec (ra dec &optional (epoch +j2000+))
  ;; RA & Dec assumed mean catalog position
  (let ((vxyz (eqx-to-cirs-xyz ra dec epoch)))
    (prec-CIRS-mn-to-GCRS-2k vxyz epoch)))

(defun to-radec (vxyz &optional (epoch (current-epoch)))
  ;; Precess to apparent position at epoch
  (let ((vp (prec-gcrs-2k-to-cirs-ap vxyz epoch)))
    (CIRS-xyz-to-EQX vp epoch)))

(defun to-mn-radec (vxyz &optional (epoch (current-epoch)))
  ;; Precess to apparent position at epoch
  (let ((vp (prec-gcrs-2k-to-cirs-mn vxyz epoch)))
    (CIRS-xyz-to-EQX vp epoch)))

#|
(radec (deg 0) (deg 0))
(to-mn-radec (radec (deg 0) (deg 0)) +j2000+)

(let* ((ra    (ra  02 31 49.0837)) ;; Polaris
       (dec   (dec 89 15 50.794))
       (ra    (ra 11 14 14.4052)) ;; θ Leo
       (dec   (dec 15 25 46.453)) 
       (epoch (+ (ymd 2024 05 30) 0.060257670))
       (eppch 246_0128.375)
       (v*    (radec ra dec)))
  (to-mn-radec v* epoch))
       
 |#
;; --------------------------------------------
(defun R3EO-e (epoch)
  ;; From M_CIO = R3(-EO) . M_class => R3(-EO) = M_CIO . Trn(M_class)
  ;;
  ;; Our PMAT routine computes M_class based on long-term models for CIP and Ecliptic Pole.
  ;;
  ;; R3(ang) applied to CIO vectors subtracts ang from CIO RA.
  ;; So we can subtract EO from RA, to convert from CIO α to EQX α, by applying:
  ;;
  ;;    R3(EO) = Trn(R3(-EO)) = Trn(M_CIO . Trn(M_class)) = M_class . Trn(M_CIO)
  ;;
  ;; Returns R3(EO) = ((  cos EO  sin EO  0)
  ;;                   ( -sin EO  cos EO  0)
  ;;                   (  0       0       1))
  ;;
  (let* ((M_class  (pmat epoch))
         ;; CIP is 3rd row of M_class
         (CIP      (third M_class))
         (M_CIO    (M_CIO CIP)))
    (mat-mulm M_class (trn M_CIO))
    ))

(defun EO-e (epoch)
  (let* ((R3EO  (R3EO-e epoch))
         (Row1  (first R3EO))
         (cEO   (first Row1))
         (sEO   (second Row1)))
    (atan sEO cEO)))

#|
(plt:fplot 'plt '(-50 50)
           (lambda (dyr)
             (let* ((epoch (ymd (+ 2000 dyr)))
                    (EO    (EO epoch))
                    (EO-e  (EO-e epoch)))
               (to-mas (- EO-e EO))))
           :clear t
           :title "EO-e - EO"
           :xtitle "Epoch - J2000.0 [yrs]"
           :ytitle "Diff (EO-e - EO) [mas]"
           :thick 2)

(let* ((epoch (ymd 2024))
       ;; (epoch +j2000+)
       (epoch (ymd 2050))
       (EO    (EO epoch))
       (EO-e  (EO-e epoch)))
  ;; EO-e and EO differ by 14.5 mas ≈ 0.967 ms in J2000 (= Frame Bias)
  ;; dropping to -5.5 mas ≈ -0.367 ms in J2050
  ;; Hence, no particular reason to change our EO(epoch) function.
  ;; Using it makes our GMST agree with USNO.
  (list
   :EO   (to-arcsec EO)
   :EO-e (to-arcsec EO-e)
   :ΔEO  (- (to-mas EO-e)
            (to-mas EO))))

(let* ((epoch +j2000+)
       (CIPe  (CIP epoch))
       (CIPa  (com.ral.astro.precession.cio-aa::CIP epoch)))
  (mapcar #'to-mas (mapcar #'- CIPe CIPa)))
 |#
