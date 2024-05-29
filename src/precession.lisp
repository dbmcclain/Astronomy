;; precession.lisp
;;
;; DM/RAL  2024/05/20 06:15:21 UTC
;; ----------------------------------

(in-package #:com.ral.astro.precession)

;; ----------------------------------
;; Precession
;;

(defun cio-to-eqx (ra dec epoch)
  ;; Convert CIO-based RA,Dec to EQX-based values.
  (values (- ra (EO epoch))
          dec))

(defun eqx-to-cio (ra dec epoch)
  ;; Convert EQX-based RA, Dec to CIO-based values.
  (values (+ ra (EO epoch))
          dec))


