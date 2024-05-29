;; observatory.lisp
;;
;; DM/RAL  2024/05/20 06:11:25 UTC
;; ----------------------------------

(in-package #:com.ral.astro.observatory)

;; ----------------------------------

;; ----------------------------------
;; QTH for RAL/BYO

(defparameter *qth-lon*   (deg -115.8469)) ;; +E/-W
(defparameter *qth-lat*   (deg   35.2872)) ;; +N/-S
(defparameter *qth-elev*  820.3)           ;; meters
(defparameter *qth-tz*    (hrs -8))        ;; = (local - UT) hours offset

