;; precession-grubby.lisp -- Precession based on grubby calculator approximation
;;
;; DM/RAL  2024/05/29 20:18:09 UTC
;; ------------------------------------------------------------------------

(in-package #:com.ral.astro.precession.grubby)

;; --------------------------------------------------
;; Grubby routine from years ago...

(defun precN (ra dec &optional (nyr (y2k (current-epoch))))
  (let* ((m  #.(secs 3.07496d0))
         (n  #.(arcsec 20.0431d0))
         (cs (* nyr n (cis ra))))
    (values (+ ra (* nyr m) (* (imagpart cs) (tan dec)))
            (+ dec (realpart cs))
            )))


