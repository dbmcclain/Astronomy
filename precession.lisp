;; precession.lisp
;;
;; DM/RAL  2024/05/20 06:15:21 UTC
;; ----------------------------------

(in-package #:astro)

;; ----------------------------------
;; Precession
;;
;; Mean obliquity not really constant, but varies by
;; about -47" per century.

(defvar *j2000*            (jdn 2000 01 01 :hh 12 :lcl-ut 0))
(defvar *days-per-year*    365.25)
(defvar *days-per-century* 36525)
(defvar *mean-obliquity*   (arcsec 84381.406)) ;; J2000 from 2023 Almanac
(defvar *precession*       (arcsec  50.28796_195))   ;; annual general precession - 2023 Almanac
(defvar *eps-dot*          (arcsec -46.836_769))     ;; change in obliquity per century - 2023 Almanac

(defun to-ecliptic (ra dec)
  (rotx-ang ra dec (- *start-obliquity*)))

(defun from-ecliptic (long lat)
  (rotx-ang long lat *end-obliquity*))

(defvar *start-obliquity*  *mean-obliquity*)
(defvar *end-obliquity*    *mean-obliquity*)

(defun precessn (ra dec nyr)
  ;; Precess nyr using mean obliquity for J2000.
  ;; It would be more accurate to specify starting and ending epochs using PRECESS below.
  ;; Using Ecliptic coords prevents problems near NCP.
  (multiple-value-bind (lon lat)
      (to-ecliptic ra dec)
    (from-ecliptic (+ lon (* nyr *precession*)) lat)
    ))

(defun obliquity-for-epoch (epoch)
  ;; Mean obliquity is declining at rate of -47 arcsec/century.
  (+ *mean-obliquity* (* *eps-dot* (/ (- epoch *j2000*) *days-per-century*))))

(defun precess (ra dec from-epoch &optional (to-epoch (current-epoch)))
  ;; for RA, Dec expressed in deg, epochs as JDN
  (let ((*start-obliquity* (obliquity-for-epoch from-epoch))
        (*end-obliquity*   (obliquity-for-epoch to-epoch)))
    (precessn ra dec (/ (- to-epoch from-epoch) *days-per-year*))))
                      

#|
(defun qd-precess (ra dec nyr)
  (values (+ ra (* nyr (+ (secs 3.07496)
                          (* (secs 1.336219)
                             (sin ra)
                             (tan dec)))
                   ))
          (+ dec (* nyr (arcsec 20.0431)
                    (cos ra)))
          ))

(let ((to-epoch (jdn 2024 01 01 :lcl-ut 0))
      (ra       (ra 15 59 30.1))
      (dec      (dec 25 55 13)))
  (multiple-value-bind (rap decp)
      (precess ra dec *j2000* to-epoch)
    (terpri)
    (print (list (to-ra-h.ms ra)
                 (to-dec-d.ms dec)))
    (print (list (to-ra-h.ms rap)
                 (to-dec-d.ms decp)))
    (let ((djd (- to-epoch *j2000*)))
      (multiple-value-bind (raq decq)
          (qd-precess ra dec (/ djd *days-per-year*))
        (print (list (to-ra-h.ms ra)
                     (to-ra-h.ms rap)
                     (to-ra-h.ms raq)))
        (print (list (to-dec-d.ms dec)
                     (to-dec-d.ms decp)
                     (to-dec-d.ms decq)))
        (print (list (to-ra-h.ms raq)
                     (to-dec-d.ms decq)))
        (print (list (to-arcsec (bipolar (- rap raq)))
                     (to-arcsec (bipolar (- decp decq)))))
        (values)
        ))))
|#


