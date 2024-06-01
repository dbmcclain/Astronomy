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
  (vadd (CIP-mn epoch)
        (GCRS-XY-aa-Nut epoch))) ;; AA nutation

;; ------------------------------
;; Compute apparent EO for epoch, given an already known M_CIO.
;;
;; Assumes EO variation from mean is due solely to nutation of the
;; Equatorial pole, and that Ecliptic pole is unaffected by nutation
;; terms.  So, uses Long Term model for Ecliptic pole.

(defun R3EO-ap (M_CIO epoch)
  ;; From M_CIO = R3(-EO) . M_class => R3(-EO) = M_CIO . Trn(M_class)
  ;;
  ;; - or -
  ;;
  ;;  R3(EO) = M_class . (Trn M_CIO)
  ;;
  ;; To get M_class, we take nv = CIP unit vector (3rd row of M_CIO),
  ;; and unit vector kv = pole of Ecliptic (computed from PECL). Then
  ;; M_class = [(nv x kv), nv x (nv x kv), nv].
  ;;
  ;; Our PECL routine computes kv based on long-term models for Ecliptic Pole.
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
  (let* ((Tc      (c2k epoch))
         (kv      (pecl Tc))       ;; unit vector to pole of Ecliptic
         (Zv      (third M_CIO))
         (Xv      (vcross Zv kv))
         (Yv      (vcross Zv Xv))
         (M_class `(,Xv ,Yv ,Zv)))
    (mat-mulm M_class (trn M_CIO))
    ))

(defun EO-ap (M_CIO epoch)
  (let* ((R3EO  (R3EO-ap M_CIO epoch))
         (Row1  (first R3EO))
         (cEO   (first Row1))
         (sEO   (second Row1)))
    (atan sEO cEO)))

;; -------------------------------------------------
;; The final distortion for apparent positions (up to ±20 arcsec).
;; Annual aberration due to Earth's motion around the Sun.
;;
;; Adapted from 3rd Ed. Suppl to Astronomical Almanac.

(defun aberration (epoch)
  ;; Annual aberration
  (let* ((Tc  (c2k epoch))
         (L   (deg (+ 280.5d0 (* Tc 36_000.8))))  ;; 1 yr period, Ecliptic lon of Sun
         (cL  (cos L)))
  (vscale #.(/ 173.24)
          `(,(*  0.0172 (sin L))
            ,(* -0.0158 cL)
            ,(* -0.0068 cL) ))
  ))

#|
(map-mult (#'to-ra #'to-dec) (to-thphi (aberration +j2000+)))

(let ((v*  (vnormalize (aberration +j2000+))))
  (plt:fplot 'plt '(0 5)
             (lambda (dt)
               (let* ((epoch (ymd (+ 2000 dt)))
                      (ab    (aberration epoch)))
                 (to-arcsec (rad (vdot ab v*)))
                 ))
           :clear t
           :title "Worst-case Annual Aberration"
           :xtitle "Epoch - J2000 [yrs]"
           :ytitle "Aberration [arcsec]"
           :yrange '(-30 30)
           :thick 2))
 |#
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
    (values (mat-mulv M_CIO v2k)
            (EO to-epoch))))

(defun prec-GCRS-2k-to-CIRS-ap (v2k &optional (to-epoch (current-epoch)))
  ;; Precess mean J2000 GCRS(x,y,z) to apparent CIRS(x,y,z) at to-epoch.
  (let* ((CIP   (CIP-ap to-epoch))
         (M_CIO (M_CIO CIP))
         (ab    (aberration to-epoch)))
    (values (vadd ab (mat-mulv M_CIO v2k))
            (EO-ap M_CIO to-epoch) )))

;; ----------------------------------------------
;; Presuming that Catalog positions report Equinox based classical RA & Dec.
;; 

(defun EQX-to-CIRS-xyz (ra dec epoch)
  ;; On entry rotate to CIRS based position before forming the CIO 3-vector.
  ;; Only ever applied to mean positions.
  (to-xyz (+ ra (EO epoch)) dec))

(defun CIRS-xyz-to-EQX (vxyz EO)
  ;; Given a CIO-based 3-vector, and the EO to use,
  ;; Convert 3-vector to classical Equinox-based RA & Dec.
  (mvb (ra dec)
      (to-thphi vxyz)
    (values (- ra EO)
            dec)))

;; -------------------------------------------------
;; Just like we do for angle measures, converting on entry to a common
;; measure, on entry RADEC converts to J2000 GCRS 3-vector.

(defstruct pos-pm
  α2k δ2k αμ δμ μ)

(defun radec (ra dec &optional (epoch +j2000+) (μα* 0) (μδ 0))
  ;; RA & Dec assumed to be mean Catalog Equinox-based classical position.
  ;; μα* should be projected proper motion = μα * cos(δ)
  ;;
  ;; As for UI inconvenience... if you are going to the trouble to
  ;; specify proper motions, then having to enter an epoch is only a
  ;; minor inconvenience.
  ;;
  ;; It is anticipated that when you develop a mean-places catalog for
  ;; planning purposes, you will specify μα and μδ at that time.
  ;;
  ;; Then, during observing, you would only need to use RADEC with
  ;; your mean places and their epoch, eliding proper motions. As long
  ;; as your catalog is within a few years of the observing session,
  ;; that should be good enough.
  ;;
  (let* ((Ty    (y2k epoch))
         (vxyz  (eqx-to-cirs-xyz ra dec epoch))
         (vprec (prec-CIRS-mn-to-GCRS-2k vxyz epoch)))
    (mvb (α* δ*)
        (CIRS-xyz-to-EQX vprec +J2000+)
      ;; α*,δ* is position in J2000 with added pm, need to remove pm
      ;; form probe pos after 1 year of pm, starting from α*,δ*
      (let* ((α1  (+ α* (/ μα* (cos δ*))))
             (δ1  (+ δ* μδ))
             (v1  (to-xyz α1 δ1))
             (v*  (to-xyz α* δ*))
             (vμ  (vcross v* v1))     ;; axis of pm rot
             (μ   (asin (vnorm vμ)))) ;; magn of 1yr pm
        (mvb (αμ δμ)  ;; pos of pm rot axis pole
            (to-thphi vμ)
          ;; now remove pm since epoch
          (mvb (α2k δ2k)
              (rot α* δ*  αμ δμ  (- (* Ty μ)))
            (make-pos-pm
             :α2k  α2k   ;; pos in J2000 sans pm
             :δ2k  δ2k
             :αμ   αμ    ;; axis of pm
             :δμ   δμ
             :μ    μ)    ;; 1yr pm magn
            ))
        ))))

(defun pos-pm-to-vxyz (pos epoch)
  (with-accessors ((α2k pos-pm-α2k)
                   (δ2k pos-pm-δ2k)
                   (αμ  pos-pm-αμ)
                   (δμ  pos-pm-δμ)
                   (μ   pos-pm-μ)) pos
    (let ((Ty  (y2k epoch)))
      ;; apply pm for Ty years
      (mvb (α* δ*)
          (rot α2k δ2k  αμ δμ  (* Ty μ))
        ;; then precess to epoch
        (EQX-to-CIRS-xyz α* δ* +J2000+)
        ))))

;; ---------------------------------------------------------

(defun to-radec (pos &optional (epoch (current-epoch)))
  ;; Precess to apparent position at epoch
  ;; Report as classical Equinox-based RA & Dec.
  (let ((vxyz  (pos-pm-to-vxyz pos epoch)))
    (map-mult (#'to-ra #'to-dec)
      (multiple-value-call #'cirs-xyz-to-eqx
        (prec-gcrs-2k-to-cirs-ap vxyz epoch)
        ))))

(defun to-mn-radec (pos &optional (epoch (current-epoch)))
  ;; Precess to mean position at epoch.
  ;; Report as classical Equinox-based RA & Dec.
  (let ((vxyz  (pos-pm-to-vxyz pos epoch)))
    (map-mult (#'to-ra #'to-dec)
      (multiple-value-call #'CIRS-xyz-to-EQX
        (prec-gcrs-2k-to-cirs-mn vxyz epoch)
        ))))

;; ---------------------------------------------------------

#|
(radec (deg 0) (deg 0))
(to-mn-radec (radec (deg 0) (deg 0)) +j2000+)

(let* ((ra    (ra  02 31 49.0837)) ;; Polaris
       (dec   (dec 89 15 50.794))
       (ra    (ra 11 14 14.4052)) ;; θ Leo
       (dec   (dec 15 25 46.453)) 
       (epoch (+ (ymd 2024 05 30) 0.060257670))
       (epoch 246_0128.375)
       (v*    (radec ra dec)))
  (list (mvl (to-mn-radec v* epoch))
        (mvl (map-mult (#'to-ra #'to-dec) (prec ra dec epoch)))
        ))

(let ((v  (radec (RA 11 14 14.4052)   ;; θ Leo from J2000.0 Catalog
                 (Dec 15 25 46.453) ))) 
  (to-radec v))
|#
;; -----------------------------------------------------------
#|
;; Compare our computed Mean EO versus the EO we use for GMST
(plt:fplot 'plt '(-50 50)
           (lambda (dyr)
             (let* ((epoch (ymd (+ 2000 dyr)))
                    (EO    (EO epoch))
                    (CIP   (CIP-mn epoch))
                    (M_CIO (M_CIO CIP))
                    (EO-mn (EO-ap M_CIO epoch)))
               (to-mas (- EO-mn EO))))
           :clear t
           :title "EO-mn - EO"
           :xtitle "Epoch - J2000.0 [yrs]"
           :ytitle "Diff (EO-mn - EO) [mas]"
           :thick 2)

(let* ((epoch (ymd 2024))
       ;; (epoch +j2000+)
       (epoch (ymd 2050))
       (EO    (EO epoch))
       (CIP   (CIP-mn epoch))
       (M_CIO (M_CIO CIP))
       (EO-mn  (EO-ap M_CIO epoch)))
  ;; EO-mn and EO differ by 14.5 mas ≈ 0.967 ms in J2000 (= Frame Bias)
  ;; dropping to -5.5 mas ≈ -0.367 ms in J2050
  ;; Hence, no particular reason to change our EO(epoch) function.
  ;; Using it makes our GMST agree with USNO.
  (list
   :EO    (to-arcsec EO)
   :EO-mn (to-arcsec EO-mn)
   :ΔEO   (- (to-mas EO-mn)
             (to-mas EO))))

;; Check - should be the same... (within roundoff errors)
(let* ((epoch +j2000+)
       (CIPe  (CIP-ap epoch))
       (CIPa  (com.ral.astro.precession.cio-aa::CIP epoch)))
  (mapcar #'to-mas (mapcar #'- CIPe CIPa)))

;; M_CIO = R3(-EO) . M_class = R3(-EO) . R3(EO-s) . M_Σ = R3(-s) . M_Σ
;; M_Σ = R3(s) . M_CIO ≈ M_CIO

(defun ΔEO (M_CIO epoch)
  (let* ((EO     (EO epoch))
         (EOa    (EO-ap M_CIO epoch)))
    (- EOa EO)))

;; Looks like EO swings ±15 as ≈ ±1 sec
(plt:fplot 'plt '(0 30)
           (lambda (dt)
             (let* ((epoch (ymd (+ 2000 dt)))
                    (CIP   (CIP-ap epoch))
                    (M_CIO (M_CIO CIP)))
               (to-arcsec (ΔEO M_CIO epoch))
               ))
           :clear t
           :title "Apparent EO - Mean EO"
           :xtitle "Epoch - J2000 [yrs]"
           :ytitle "Apparent EO - Mean EO [arcsec]"
           :thick 2)
|#

