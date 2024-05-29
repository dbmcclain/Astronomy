;; precession.lisp
;;
;; DM/RAL  2024/05/20 06:15:21 UTC
;; ----------------------------------

(in-package #:com.ral.astro.precession)

;; ----------------------------------
;; Precession - here we define items that are used by all the various
;; precession packages
;;

(defun cio-to-eqx (ra dec epoch)
  ;; Convert CIO-based RA,Dec to EQX-based values.
  (values (- ra (EO epoch))
          dec))

(defun eqx-to-cio (ra dec epoch)
  ;; Convert EQX-based RA, Dec to CIO-based values.
  (values (+ ra (EO epoch))
          dec))

;; -----------------------------------------

(defun M_CIO (xy)
  ;; Produce 2nd order M_CIO, given CIP (X,Y).
  ;; Simplified precession, good to 0.08 arcsec in 21st cy,
  ;; good to 0.38 arcsec over ±2 cy
  ;;
  ;;  v_TIRS = R(TT,UT) . v_GCRS
  ;;  v_CIRS = M_CIO(TT) . v_GCRS
  ;;
  (db (X Y . _) xy
    (declare (ignore _))
    (let ((cX  (- 1 (* 1/2 X X)) )) ;; ≈ (Cos X)
      `(( ,cX  0   ,(- X) )
        (  0   1   ,(- Y) )
        ( ,X  ,Y   ,cX    ))
      )))

;; -----------------------------------------

(defvar *CIP-fn*)

(defun GCRS-TO-CIRS (ra dec &optional (epoch (current-epoch)))
  ;; Apply precesssion + nutation to a GCRS J2000.0 position.  On
  ;; entry, RA and Dec should refer to a CIO-based position, not an
  ;; EQX-position.
  (let* ((CIP    (funcall *CIP-fn* epoch))
         (M_CIO  (M_CIO CIP))
         (v_GCRS (to-xyz ra dec))
         (v_CIRS (mat-mulv M_CIO v_GCRS)))
    (to-thphi v_CIRS)))

(defun CIRS-to-GCRS (ra dec &optional (epoch (current-epoch)))
  ;; Undo the precession + nutation from a CIO-based position at
  ;; epoch, to produce the equivalent J2000.0 GCRS position.  On
  ;; entry, RA and Dec should refer to a CIO-based position, not an
  ;; EQX-position.
  (let* ((CIP    (funcall *CIP-fn* epoch))
         (M_CIO  (M_CIO CIP))
         (v_CIRS (to-xyz ra dec))
         (V_GCRS (mat-mulv (trn M_CIO) v_CIRS)))
    (to-thphi v_GCRS)))
