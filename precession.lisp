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
#|
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
|#

;; ------------------------------------------------------
;; From IAU SOFA, J.Vrondak,et al, "New precession expressions, valid for long time intervals", AA, 2011
;; Computed for J2000.0

(defun pecl (epoch)
  ;; Precession of the Ecliptic
  ;; Compute unit vector to Ecliptic pole at epoch.
  (let* ((dt    (c2k epoch))
         (eps0  #.(arcsec 84381.406))
         (pqpol #.#2A(( 5851.607687  -0.1189000  -0.00028913   0.000000101)
                      (-1600.886300   1.1689818  -0.00000020  -0.000000437)))
         (pqper #.#2A(( 708.15 -5486.751211 -684.661560   667.666730 -5523.863691)
                      (2309.00   -17.127623 2446.283880 -2354.886252  -549.747450)
                      (1620.00  -617.517403  399.671049  -428.152441  -310.998056)
                      ( 492.20   413.442940 -356.652376   376.202861   421.535876)
                      (1183.00    78.614193 -186.387003   184.778874   -36.776172)
                      ( 622.00  -180.732815 -316.800070   335.321713  -145.278396)
                      ( 882.00   -87.676083  198.296701  -185.138669   -34.744450)
                      ( 547.00    46.140315  101.135679  -120.972830    22.885731)))
         (p    0)
         (q    0))
    (let ((w  (turns dt)))
      (dotimes (ix (array-dimension pqper 0))
        (let* ((a  (/ w (aref pqper ix 0)))
               (z  (cis a))
               (c  (realpart z))
               (s  (imagpart z)))
          (incf p (+ (* c (aref pqper ix 1))
                     (* s (aref pqper ix 3))))
          (incf q (+ (* c (aref pqper ix 2))
                     (* s (aref pqper ix 4))))
          )))
    (let ((w 1))
      (dotimes (ix (array-dimension pqpol 1))
        (incf p (* w (aref pqpol 0 ix)))
        (incf q (* w (aref pqpol 1 ix)))
        (setf w (* w dt))))

    (let* ((p  (to-rad (arcsec p)))
           (q  (to-rad (arcsec q)))
           (w  (sqrt (max 0 (- 1 (* p p) (* q q)))))
           (z  (cis eps0))
           (c  (realpart z))
           (s  (imagpart z)))
      (list p
            (- (+ (* c q) (* s w)))
            (- (* c 2) (* s q)))
      )))
           
(defun pequ (epoch)
  ;; Precessoin of the Equator
  ;; Compute unit vector to Equatorial pole at epoch.
  (let* ((dt    (c2k epoch))
         (xypol #.#2A((  5453.282155   0.4252841   -0.00037173   -0.000000152)
                      (-73750.930350  -0.7675452   -0.00018725    0.000000231)))
         (xyper #.#2A(( 256.75  -819.940624 75004.344875 81491.287984  1558.515853)
                      ( 708.15 -8444.676815   624.033993   787.163481  7774.939698)
                      ( 274.20  2600.009459  1251.136893  1251.296102 -2219.534038)
                      ( 241.45  2755.175630 -1102.212834 -1257.950837 -2523.969396)
                      (2309.00  -167.659835 -2660.664980 -2966.799730   247.850422)
                      ( 492.20   871.855056   699.291817   639.744522  -846.485643)
                      ( 396.10    44.769698   153.167220   131.600209 -1393.124055)
                      ( 288.90  -512.313065  -950.865637  -445.040117   368.526116)
                      ( 231.10  -819.415595   499.754645   584.522874   749.045012)
                      (1610.00  -538.071099  -145.188210   -89.756563   444.704518)
                      ( 620.00  -189.793622   558.116553   524.429630   235.934465)
                      ( 157.87  -402.922932   -23.923029   -13.549067   374.049623)
                      ( 220.30   179.516345  -165.405086  -210.157124  -171.330180)
                      (1200.00    -9.814756     9.344131   -44.919798   -22.899655)))
         (x     0)
         (y     0))
    (let ((w  (turns dt)))
      (dotimes (ix (array-dimension xyper 0))
        (let* ((a  (/ w (aref xyper ix 0)))
               (z  (cis a))
               (c  (realpart z))
               (s  (imagpart z)))
          (incf x (+ (* c (aref xyper ix 1)) (* s (aref xyper ix 3))))
          (incf y (+ (* c (aref xyper ix 2)) (* s (aref xyper ix 4))))
          )))
    (let ((w  1))
      (dotimes (ix (array-dimension xypol 1))
        (incf x (* w (aref xypol 0 ix)))
        (incf y (* w (aref xypol 1 ix)))
        (setf w (* w dt))
        ))
    (let* ((x  (to-rad (arcsec x)))
           (y  (to-rad (arcsec y)))
           (z  (sqrt (max 0 (- 1 (* x x) (* y y))))))
      (list x y z)
      )))

(defun pmat (epoch)
  ;; Compute long term precession matrix.
  ;; Produces a precession matrix that will transform from J2000.0 to Epoch.
  ;; To be applied against an XYZ vector arising from RA, Dec at J2000.0.
  (let* ((z    (pequ epoch)) ;; pole of Equator
         (eclp (pecl epoch)) ;; pole of Ecliptic
         (x    (vnormalize (vcross z eclp)))
         (y    (vcross z x)))
    (list x y z)))


(defun trn (m)
  (when (car m)
    (cons (mapcar #'car m)
          (trn (mapcar #'cdr m)))))

#|                     
(let* ((m  '((1 2 3) (4 5 6))))
  (trn m))
|#       

(defun mat-mul (m v)
  (mapcar (um:curry #'vdot v) m))

(defun prec (ra dec from-epoch &optional (to-epoch (current-epoch)))
  ;; Precess using IAU long-term models for Ecliptic and Equatorial precession.
  (let* ((xyz1  (to-xyz ra dec))
         (xyz2k (if (= from-epoch *j2000*)
                    xyz1
                  (let ((pmati (trn (pmat from-epoch))))
                    (mat-mul pmati xyz1))))
         (xyz2  (if (= to-epoch *J2000*)
                    xyz2k
                  (let ((pmat (pmat to-epoch)))
                    (mat-mul pmat xyz2k)))))
    (to-thphi xyz2)))


#|
;; Grubby routine from years ago...
(defun qd-precess (ra dec nyr)
  (values (+ ra (* nyr (+ #.(secs 3.07496)
                          (* #.(secs 1.336219)
                             (sin ra)
                             (tan dec)))
                   ))
          (+ dec (* nyr #.(arcsec 20.0431)
                    (cos ra)))
          ))

;; Test out the various precession methods
(let ((to-epoch (jdn 2024 01 01 :lcl-ut 0))
      (ra       (ra  06 59 30.1))
      (dec      (dec 85 55 13  )))
  (terpri)
  (print (list :START (to-ra ra) (to-dec dec)))

  (multiple-value-bind (rap decp)
      (precess ra dec *j2000* to-epoch)
    (print (list :PRECESS (to-ra rap)
                 (to-dec decp))))
  
  (let ((djd (y2k to-epoch)))
    (multiple-value-bind (ran decn)
        (precessn ra dec djd)
      (print (list :PRECESSN (to-ra ran) (to-dec decn))))
    
    (multiple-value-bind (raq decq)
        (qd-precess ra dec djd)
      #|
      (print (list (to-ra-h.ms ra)
                   (to-ra-h.ms rap)
                   (to-ra-h.ms raq)))
      (print (list (to-dec-d.ms dec)
                   (to-dec-d.ms decp)
                   (to-dec-d.ms decq)))
      |#
      (print (list :GRUBBY (to-ra raq)
                   (to-dec decq)))
      ))
  
  (multiple-value-bind (ra2 dec2)
      (prec ra dec *j2000* to-epoch)
    (print (list :PREC (to-ra ra2)
                 (to-dec dec2)))

    #|
      (print (list (to-arcsec (bipolar (- rap raq)))
                   (to-arcsec (bipolar (- decp decq)))))
      |#
    (values)
    ))
|#


  
