;; precession-expt.lisp - Experiments in Precession
;;
;; DM/RAL  2024/05/29 20:31:10 UTC
;; ------------------------------------------------------

(in-package #:com.ral.astro.precession.expt)

;; ------------------------------------------------------
;; Approx CIRS precession + nutation, with XY precession obtained from
;; more accurate long-term CIP model, and nutation obtained from AA
;; model.

(defun CIP (epoch)
  (let* ((Tc     (c2k epoch))
         (CIP    (pequ Tc))               ;; Long-Term EQU model
         (Nut    (gcrs-xy-aa-nut epoch))) ;; AA nutation
    (mapcar #'+ Nut CIP)))
  
(defun GCRS-to-CIRS-e (ra dec epoch)
  (let* ((CIP    (CIP epoch))
         (m_cio  (M_CIO CIP))
         (v_gcrs (to-xyz ra dec))
         (v_cirs (mat-mulv m_cio v_gcrs)))
    (to-thphi v_cirs)
    ))

(defun CIRS-to-GCRS-e (ra dec epoch)
  (let* ((CIP    (CIP epoch))
         (m_cio  (M_CIO CIP))
         (v_cirs (to-xyz ra dec))
         (v_gcrs (mat-mulv (trn m_cio) v_cirs)))
    (to-thphi v_gcrs)
    ))

(defun prece (ra dec &optional (to-epoch (current-epoch)) (from-epoch +j2000+))
  ;; CIRS-based precession
  ;; On entry, RA and Dec should refer to an EQX-based position.
  ;; Results seem identical with PREC based on Long Term Equator and Equinox.
  (mvb (rac decc) ;; convert to CIO-based position
      (eqx-to-cio ra dec from-epoch)
    (mvb (ra2k dec2k)  ;; unwind precession+nutation to reach J2000.0
        (CIRS-to-GCRS-e rac decc from-epoch)
      (mvb (rap decp) ;; apply precession+nutation for to-epoch
          (GCRS-to-CIRS-e ra2k dec2k to-epoch)
        (cio-to-eqx rap decp to-epoch) ;; convert to EQX-base position
        ))))

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
         ;; Bottom row of M_class is unit vector (X,Y,Z) to GCRS CIP
         (CIP      (third M_class))
         (X        (first CIP))
         (Y        (second CIP))
         (cX       (- 1 (* 1/2 X X)))
         (trnM_CIO `((,cX     0      ,X)
                     ( 0      1      ,Y)
                     (,(- X) ,(- Y)  ,cX)) ))
    (mat-mulm M_class trnM_CIO)
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
  ;; EO-e and EO differ by 14.5 mas ≈ 0.967 ms in J2000
  ;; dropping to -5.5 mas ≈ -0.367 ms in J2050
  ;; Hence, no particular reason to change our EO(epoch) function.
  ;; Using it makes our GMST agree with USNO.
  (list
   :EO   (to-arcsec EO)
   :EO-e (to-arcsec EO-e)
   :ΔEO  (- (to-mas EO-e)
            (to-mas EO))))
 |#
