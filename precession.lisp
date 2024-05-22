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
(defvar *mean-obliquity*
  ;; J2000 from 2023 Almanac
  `(,(arcsec 84381.406)       ;; ≈ 23.4 deg
    ,(arcsec   -46.836_769))) ;; change in obliquity per century
  
(defvar *precession*       (arcsec    50.28796_195))   ;; annual general precession - 2023 Almanac

;; ------------------------------------------------------

(defun jyrs (dt)
  ;; Julian years for dt days
  (/ dt *days-per-year*))

(defun jcs (dt)
  ;; Julian centuries for dt days
  (/ dt *days-per-century*))

(defun d2k (epoch)
  ;; days since J2000
  (- epoch *j2000*))

(defun y2k (epoch)
  ;; years since J2000
  (jyrs (d2k epoch)))

(defun c2k (epoch)
  ;; centuries since J2000
  (/ (y2k epoch) 100))

(defun horner (x coffs)
  ;; Polynomial eval
  (if (endp coffs)
      0
    (+ (car coffs)
       (* x (horner x (cdr coffs))))
    ))

;; ------------------------------------------------------

(defun obliquity-for-epoch (epoch)
  ;; Mean obliquity is declining at rate of -47 arcsec/century.
  (horner (c2k epoch) *mean-obliquity*))

;; ------------------------------------------------------

(defvar *start-obliquity*  (obliquity-for-epoch *J2000*))
(defvar *end-obliquity*    (obliquity-for-epoch *J2000*))

(defun to-ecliptic (ra dec)
  (rotx-ang ra dec (- *start-obliquity*)))

(defun from-ecliptic (long lat)
  (rotx-ang long lat *end-obliquity*))

;; ------------------------------------------------------

(defun precessn (ra dec nyr)
  ;; Precess nyr using mean obliquity for J2000.
  ;; It would be more accurate to specify starting and ending epochs using PRECESS below.
  ;; Using Ecliptic coords prevents problems near NCP.
  (multiple-value-bind (lon lat)
      (to-ecliptic ra dec)
    (from-ecliptic (+ lon (* nyr *precession*)) lat)
    ))

;; ------------------------------------------------------

(defun precess (ra dec from-epoch &optional (to-epoch (current-epoch)))
  ;; for RA, Dec expressed in deg, epochs as JDN
  (let ((*start-obliquity* (obliquity-for-epoch from-epoch))
        (*end-obliquity*   (obliquity-for-epoch to-epoch)))
    (precessn ra dec (jyrs (- to-epoch from-epoch)))
    ))
                      
;; ------------------------------------------------------

#|
(defun qd-precess (ra dec nyr)
  (values (+ ra (* nyr (+ #.(secs 3.07496)
                          (* #.(secs 1.336219)
                             (sin ra)
                             (tan dec)))
                   ))
          (+ dec (* nyr #.(arcsec 20.0431)
                    (cos ra)))
          ))

(let ((to-epoch (jdn 2024 01 01 :lcl-ut 0))
      (ra       (ra  06 59 30.1))
      (dec      (dec 85 55 13  )))
  (multiple-value-bind (rap decp)
      (precess ra dec *j2000* to-epoch)
    (terpri)
    (print (list (to-ra-h.ms ra)
                 (to-dec-d.ms dec)))
    (print (list (to-ra-h.ms rap)
                 (to-dec-d.ms decp)))
    (let ((djd (y2k to-epoch)))
      (multiple-value-bind (raq decq)
          (qd-precess ra dec djd)
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


#|
;; From J.Vrondak,et al, "New precession expressions, valid for long time intervals", AA, 2011
;; Computed for J2000.0

(defun pmat (epoch)
  ;; generate precession matrix from J2000.0 to epoch
  )


(defun prec (ra dec from-epoch &optional (to-epoch (current-epoch)))
  (let* ((xyz1  (to-xyz ra dec))
         (pmati (mat-inv (pmat (- from-epoch *j2000*))))
         (xyz2k (if (= from-epoch *j2000*)
                    xyz1
                  (let ((pmati (mat-inv (pmat (- from-epoch *j2000*)))))
                    (mat-mul pmati xyz1))))
         (pmat  (pmat (- to-epoch *j2000*)))
         (xyz2  (if (= to-epoch *J2000*)
                    xyz2k
                  (let ((pmat (pmat (- to-epoch *j2000*))))
                    (mat-mul pmat xyz2k)))))
    (to-thphi xyz2)))
|#

  
