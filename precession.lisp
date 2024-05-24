;; precession.lisp
;;
;; DM/RAL  2024/05/20 06:15:21 UTC
;; ----------------------------------

(in-package #:astro)

;; ----------------------------------
;; Precession
;;

(defvar *j2000*            (jdn 2000 01 01 :hh 12 :lcl-ut 0))
(defvar *days-per-year*    365.25)
(defvar *days-per-century* 36525)

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
  (car
   (reduce (lambda (coff state)
             (destructuring-bind (accum xx) state
               (list (+ coff (* xx accum)) (* x xx))
               ))
           coffs
           :from-end t
           :initial-value '(0 1))))

(defun poly-eval (x coffs)
  (if (< (abs x) 1.0)
      (horner x coffs)
    (car
     (reduce (lambda (state coff)
               (destructuring-bind (accum xx) state
                 (list (+ accum (* xx coff)) (* x xx))
                 ))
             coffs
             :from-end nil
             :initial-value '(0 1))
     )))

;; ------------------------------------------------------
#|
 
;; Mean obliquity not really constant, but varies by
;; about -47" per century.
(defvar *mean-obliquity*
  ;; J2000 from 2023 Almanac
  `(,(arcsec 84381.406)       ;; ≈ 23.4 deg
    ,(arcsec   -46.836_769))) ;; change in obliquity per century
  
(defvar *precession*       (arcsec    50.28796_195))   ;; annual general precession - 2023 Almanac

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
;; From IAU/SOFA 2023 C Library
;; See also, J.Vrondak,et al, "New precession expressions, valid for long time intervals", 2011 AA
;; Computed for Mean Ecliptic and Equator of J2000.0

(defun epj (epoch)
  ;; Compute the Julian Epoch for a given JDN
  (+ 2000.0 (y2k epoch)))

(defun epjc (epj)
  ;; Centuries between 2000 and epj.
  (/ (- epj 2000) 100))

(defun per-sum (w coffs)
  ;; perform a pair of periodic component sums
  (let ((s1 0)
        (s2 0))
    (dotimes (ix (array-dimension coffs 0))
      (let* ((a  (/ w (aref coffs ix 0)))
             (cs (cis a))
             (c  (realpart cs))
             (s  (imagpart cs)))
        (incf s1 (+ (* c (aref coffs ix 1))
                    (* s (aref coffs ix 3))))
        (incf s2 (+ (* c (aref coffs ix 2))
                    (* s (aref coffs ix 4))))
        ))
    (values s1 s2)
    ))
  
(defun pol-sum (x coffs)
  ;; perform a pair of polynomial component sums
  (values
   (poly-eval x (aref coffs 0))
   (poly-eval x (aref coffs 1))
   ))

(defun pol-per-sum (x pol-coffs per-coffs)
  (multiple-value-bind (s1-per s2-per)
      (per-sum (turns x) per-coffs)
    (multiple-value-bind (s1-pol s2-pol)
        (pol-sum x pol-coffs)
      (values (+ s1-pol s1-per)
              (+ s2-pol s2-per))
      )))
                       
;; --------------------------------------------------

(defun pecl (epj)
  ;; Precession of the Ecliptic
  ;; Compute unit vector to Ecliptic pole at epoch.
  ;; EPJ is a Julian Epoch
  (let* ((dt    (epjc epj))
         (eps0  #.(arcsec 84381.406))
         (pqpol #.#(( 5851.607687  -0.1189000  -0.00028913   0.000000101)
                    (-1600.886300   1.1689818  -0.00000020  -0.000000437)))
         (pqper #.#2A(( 708.15 -5486.751211 -684.661560   667.666730 -5523.863691)
                      (2309.00   -17.127623 2446.283880 -2354.886252  -549.747450)
                      (1620.00  -617.517403  399.671049  -428.152441  -310.998056)
                      ( 492.20   413.442940 -356.652376   376.202861   421.535876)
                      (1183.00    78.614193 -186.387003   184.778874   -36.776172)
                      ( 622.00  -180.732815 -316.800070   335.321713  -145.278396)
                      ( 882.00   -87.676083  198.296701  -185.138669   -34.744450)
                      ( 547.00    46.140315  101.135679  -120.972830    22.885731))))
    (multiple-value-bind (psum qsum)
        (pol-per-sum dt pqpol pqper)
      (let* ((p  (to-rad (arcsec psum)))
             (q  (to-rad (arcsec qsum)))
             (r  (sqrt (max 0 (- 1 (* p p) (* q q)))))
             (cs (cis eps0))
             (c  (realpart cs))
             (s  (imagpart cs)))
        (list p
              (- (+ (* c q) (* s r)))
              (- (* c r) (* s q)))
        ))))

#|
(pecl (epj 1219339.078000))
=> (4.172478576400136E-4 -0.4049549137582655 0.9143365593299115)
Check from Vondrak:
For JDN = 1219339.078000
pecl = ( +0.00041724785764001342 −0.40495491104576162693 +0.91433656053126552350 )
 |#

(defun pequ (epj)
  ;; Precession of the Equator
  ;; Compute unit vector to Equatorial pole at epoch.
  ;; EPJ is a Julian Epoch
  (let* ((dt    (epjc epj))
         (xypol #.#((  5453.282155   0.4252841   -0.00037173   -0.000000152)
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
                      (1200.00    -9.814756     9.344131   -44.919798   -22.899655))))
    (multiple-value-bind (xsum ysum)
        (pol-per-sum dt xypol xyper)
      (let* ((x  (to-rad (arcsec xsum)))
             (y  (to-rad (arcsec ysum)))
             (z  (sqrt (max 0 (- 1 (* x x) (* y y))))))
        (list x y z)
        ))))

#|
(pequ (epj 1219339.078000))
=> (-0.2943764379736904 -0.11719098023370263 0.9484770882408209)
Check from Vondrak:
For JDN = 1219339.078000
pequ = ( −0.29437643797369031532 −0.11719098023370257855 +0.94847708824082091796 )
 |#

(defun pmat (epoch)
  ;; Compute long term precession matrix.
  ;; Produces a precession matrix that will transform from J2000.0 to Epoch.
  ;; To be applied against an XYZ vector arising from RA, Dec at J2000.0.
  (let* ((epj  (epj epoch))
         (zv   (pequ epj)) ;; pole of Equator
         (eclp (pecl epj)) ;; pole of Ecliptic
         (xv   (vnormalize (vcross zv eclp)))
         (yv   (vcross zv xv)))
    (list xv yv zv)))

#|
(pmat 1219339.078000)
=>
((0.6847339092712664 0.6664779364917481 0.2948671457856752)
 (-0.6666948224337814 0.7362563645372211 -0.11595076290574136)
 (-0.2943764379736904 -0.11719098023370263 0.9484770882408209))
Check from Vondrak:
For JDN = 1219339.078000
Rp = ((+0.68473390570729557360 +0.66647794042757610444 +0.29486714516583357655)
      (−0.66669482609418419936 +0.73625636097440967969 −0.11595076448202158534)
      (−0.29437643797369031532 −0.11719098023370257855 +0.94847708824082091796))
 |#
;; --------------------------------------------

(defun trn (m)
  ;; Compute matrix transpose.
  ;; Matrix is a list of 3 element lists representing row vectors.
  ;; For a unitory transform matrix, the transpose is its inverse.
  (apply #'mapcar #'list m))

(defun mat-mulv (m v)
  (mapcar (um:curry #'vdot v) m))

(defun mat-mulm (m1 m2)
  (mapcar (um:curry #'mat-mulv m1) (trn m2)))

;; --------------------------------------------

(defun prec (ra dec from-epoch &optional (to-epoch (current-epoch)))
  ;; Precess using IAU long-term models for Ecliptic and Equatorial precession.
  (let* ((xyz1  (to-xyz ra dec))
         (xyz2k (mat-mulv (trn (pmat from-epoch)) xyz1))
         (xyz2  (mat-mulv (pmat to-epoch) xyz2k)))
     (to-thphi xyz2)))

;; --------------------------------------------------
;; Grubby routine from years ago...

(defun precN (ra dec nyr)
  (let* ((m  #.(secs 3.07496))
         (n  #.(arcsec 20.0431))
         (cs (* nyr n (cis ra))))
    (values (+ ra (* nyr m) (* (imagpart cs) (tan dec)))
            (+ dec (realpart cs))
            )))

;; ------------------------------------------------
#|
(let ((to-epoch   (jdn 2024 01 01 :lcl-ut 0))
      (ra         (ra  06 59 30.1))
      (dec        (dec 85 55 13  )))
  (map-mult (#'to-ra #'to-dec)
            (prec ra dec *j2000* to-epoch)))

(let* ((epoch (jdn 2024 01 01 :lcl-ut 0))
       (m     (pmat epoch)))
  (print m)
  (mat-mulm m (trn m)))

(let* ((to-epoch   (jdn 2024 01 01 :lcl-ut 0))
       (from-epoch (- to-epoch (* 10 *days-per-year*)))
       (ra         (ra  16 59 30.1))
       (dec        (dec 85 55 13  ))
       (m1         (trn (pmat from-epoch)))
       (m2         (pmat to-epoch)))
  (mat-mulm m2 m1))

;; --------------------------------------------------

(let ((to-epoch   (jdn 2050 01 01 :lcl-ut 0))
      (ra         (ra  00 00 00))
      (dec        (dec 87 00 00  )))
  (print (multiple-value-list
          (map-mult (#'to-ra #'to-dec)
                    (prec ra dec *j2000* to-epoch))))
  (print (multiple-value-list
          (map-mult (#'to-ra #'to-dec)
                    (precN ra dec 24))))
  (values))

;; How bad is the grubby precession algorithm?  Over the whole sky,
;; avoiding just 1 deg Dec near the poles, it looks like our maximum
;; error would be 2.8 arcmin, on the sky, 50 years from J2000.

(let* ((to-epoch   (jdn 2050 01 01 :lcl-ut 0))
       (img        (make-array '(181 360)
                               :element-type 'single-float
                               :initial-element 0.0f0))
       (maxa   0)
       (mina   most-positive-single-float))
  (loop for ix from 0 below 360 do
        (let ((ra (deg ix)))
          (loop for jx from 1 below 178 do
                  (let ((dec (deg (- jx 90))))
                    (multiple-value-bind (rap decp)
                        (prec ra dec *j2000* to-epoch)
                      (multiple-value-bind (rag decg)
                          (precN ra dec (round (y2k to-epoch)))
                        (let* ((vp (to-xyz rap decp))
                               (vg (to-xyz rag decg))
                               (vx (vcross vp vg))
                               (ang (to-arcsec (asin (vnorm vx)))))
                          (unless (zerop ang)
                            (setf mina (min mina ang)
                                  maxa (max maxa ang)))
                          (setf (aref img jx ix) (float ang 1f0))
                          )))))))
  (plt:plot-image 'plt-ra '(0 24) '(-90 90) img :clear t)
  (print (list mina maxa))
  )

;; How large is the precession correction on the sky? Over the same
;; region, whole sky, 50 years from J2000, the maximum precession
;; correction amounts to a shift of 43 arcmin. So the grubby algorithm
;; is within 7%.

(let* ((to-epoch   (jdn 2050 01 01 :lcl-ut 0))
       (img        (make-array '(181 360)
                               :element-type 'single-float
                               :initial-element 0.0f0))
       (maxa   0)
       (mina   most-positive-single-float))
  (loop for ix from 0 below 360 do
        (let ((ra (deg ix)))
          (loop for jx from 1 below 180 do
                  (let ((dec (deg (- jx 90))))
                    (multiple-value-bind (rap decp)
                        (prec ra dec *j2000* to-epoch)
                      (let* ((v0 (to-xyz ra dec))
                             (vp (to-xyz rap decp))
                             (vx (vcross vp v0))
                             (ang (to-arcsec (asin (vnorm vx)))))
                        (unless (zerop ang)
                          (setf mina (min mina ang)
                                maxa (max maxa ang)))
                        (setf (aref img jx ix) (float ang 1f0))
                        ))))))
  (plt:plot-image 'plt-ra '(0 24) '(-90 90) img :clear t
                  :title "Precession Correction on the Sky"
                  :xtitle "Right Ascension [hrs]"
                  :ytitle "Declination [deg]")
  (print (list mina maxa))
  )
       

;; --------------------------------------------------
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
        (precN ra dec djd)
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

;; ------------------------------------------
;; IAU 2006 Precession
;; ICRS Procedures - International Celestial Reference System
;; Ref, P.T.Wallace and N.Capitaine: "IAU 2006 precession-nutation procedures"

(defun EO (epoch)
  (- (ERA epoch) (#|GST0|# LMST0 epoch)))

(defun ERA (epoch)
  ;; Earth Rotation Angle
  (let ((Tu  (d2k epoch))) ;; should actually use UT1 instead of UTC, but, oh well...
    (turns (+ (mod epoch 1.0)
              0.7790572732640
              (* Tu 0.00273781191135448 )))
    ))

(defun M_CIO (epoch)
  ;; Simplified precession, good to 0.12 arcsec in 21st cy,
  ;; good to 0.85 arcsec over ±2 cy
  ;;
  ;;  v_TIRS = R(TT,UT) . v_GCRS
  ;;  v_CIRS = M_CIO(TT) . v_GCRS
  ;;
  ;; This function is M_CIO(TT).
  ;;
  (let* ((τ  (d2k epoch))
         (Ω  (rad (+ 2.182 (* τ -9.242d-4))))
         (cs (cis Ω))
         (X  (+ (* τ 2.6603d-7)   (* -33.2d-6 (imagpart cs))))
         (Y  (+ (* τ τ -8.14d-14) (*  44.6d-6 (realpart cs)))))
    `(( 1  0  ,(- X))
      ( 0  1  ,(- Y))
      (,X ,Y  1))
    ))

(defun GCRS-to-CIRS (ra dec &optional (epoch (current-epoch)))
  (let* ((v_GCRS  (to-xyz ra dec))
         (M_CIO   (M_CIO epoch))
         (v_CIRS  (mat-mulv M_CIO v_GCRS)))
    (to-thphi v_CIRS)))

#|
;; Check from Wallace and Capitaine
;; For TT = 2400000.5+53750.892855138888889(JD)
;;   MCIO ≈ (( +1.00000000000000000 +0.00000000000000000 −0.00058224012792061)
;;           ( +0.00000000000000000 +1.00000000000000000 −0.00004374943683668)
;;           ( +0.00058224012792061 +0.00004374943683668 +1.00000000000000000))
;;
(let* ((tt (+ 2400000.5 53750.892855138888889)))
  (M_CIO tt))
=>
(( 1                    0                    -5.822401279206334E-4)
 ( 0                    1                    -4.374943683668478E-5)
 ( 5.822401279206334E-4 4.374943683668478E-5 1                    ))
|#

#|
(let ((epoch (jdn 2024 01 01 :lcl-ut 0))
      (ra    (ra  06 59 30.1))
      (dec   (dec 85 55 13  )))
  (terpri)
  (print (list :ERA (to-ra (era epoch))))
  (print (list :EO  (to-ra (eo epoch))))
  (print (list :START (to-ra ra) (to-dec dec)))
  (multiple-value-bind (rap decp)
      (map-mult (#'to-ra #'to-dec)
                 (prec ra dec epoch))
    (print (list rap decp)))
  (multiple-value-bind (rap decp)
      (map-mult (#'to-ra #'to-dec)
                 (GCRS-to-CIRS ra dec epoch))
    (print (list rap decp)))
  (values)
  )
 |#