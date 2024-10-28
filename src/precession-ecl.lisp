;; precession-ecl.lisp -- Precession based on Equator and Ecliptic of Epoch
;;
;; DM/RAL  2024/05/29 20:18:09 UTC
;; ------------------------------------------------------------------------

(in-package #:com.ral.astro.precession.ecl)

;; ------------------------------------------------------
;; From IAU/SOFA 2023 C Library
;;
;; See also, J.Vrondak,et al, "New precession expressions, valid for
;; long time intervals", 2011 AA
;;
;; Many thanks also to P.T.Wallace and N.Capitaine, and G.H.Kaplan, for
;; their papers and for the SOFA library at IAU.
;;
;; N. Capitaine and P. T. Wallace: "High precision methods for
;; locating the celestial intermediate pole and origin”
;;
;; P. T. Wallace and N. Capitaine: "IAU 2006 precession-nutation
;; procedures”
;;
;; G.H.Kaplan, "The IAU Resolutions on Astronomical Reference Systems,
;; Time Scales, and Earth Rotation Models - Explanation and
;; Implementation", US Naval Observatory, Circular No. 179., Oct 2005.
;;
;; "Explanatory Supplement to the Astronomical Almanac", E.Urban and
;; P.Kenneth Seidelmann, 3rd edition, University Science Books.
;;
;; "The Astronomical Almanac for the Year 2023", US Govt Printing Office.
;; ------------------------------------------------------

;; Computed for Mean Ecliptic and Equator of J2000.0

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
                    (* s (aref coffs ix 3.))))
        (incf s2 (+ (* c (aref coffs ix 2.))
                    (* s (aref coffs ix 4.))))
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

(defun pecl (Tc)
  ;; Precession of the Ecliptic
  ;; Compute unit vector to Ecliptic pole at epoch.
  ;; Tc is Julian Centuries from J2000.
  (let* ((eps0  #.(arcsec 84381.406d0))
         (pqpol #.#(( 5851.607687d0  -0.1189000d0  -0.00028913d0   0.000000101d0)
                    (-1600.886300d0   1.1689818d0  -0.00000020d0  -0.000000437d0)))
         (pqper #.#2A(( 708.15d0 -5486.751211d0 -684.661560d0   667.666730d0 -5523.863691d0)
                      (2309.00d0   -17.127623d0 2446.283880d0 -2354.886252d0  -549.747450d0)
                      (1620.00d0  -617.517403d0  399.671049d0  -428.152441d0  -310.998056d0)
                      ( 492.20d0   413.442940d0 -356.652376d0   376.202861d0   421.535876d0)
                      (1183.00d0    78.614193d0 -186.387003d0   184.778874d0   -36.776172d0)
                      ( 622.00d0  -180.732815d0 -316.800070d0   335.321713d0  -145.278396d0)
                      ( 882.00d0   -87.676083d0  198.296701d0  -185.138669d0   -34.744450d0)
                      ( 547.00d0    46.140315d0  101.135679d0  -120.972830d0    22.885731d0))))
    (multiple-value-bind (psum qsum)
        (pol-per-sum Tc pqpol pqper)
      (let* ((p  (to rad (arcsec psum)))
             (q  (to rad (arcsec qsum)))
             (r  (sqrt (max 0 (- 1 (* p p) (* q q)))))
             (cs (cis eps0))
             (c  (realpart cs))
             (s  (imagpart cs)))
        (list p
              (- (+ (* c q) (* s r)))
              (- (* c r) (* s q)))
        ))))

#|
(pecl (c2k 1219339.078000d0))
=> (4.172478576400136E-4 -0.4049549137582655 0.9143365593299115)
Check from Vondrak:
For JDN = 1219339.078000
pecl = ( +0.00041724785764001342 −0.40495491104576162693 +0.91433656053126552350 )
 |#

(defun pequ (Tc)
  ;; Precession of the Equator
  ;; Compute unit vector to Equatorial pole at epoch.
  ;; Tc is Julian Centuries from J2000.
  (let* ((xypol #.#((  5453.282155d0   0.4252841   -0.00037173   -0.000000152)
                    (-73750.930350d0  -0.7675452   -0.00018725    0.000000231)))
         (xyper #.#2A(( 256.75d0  -819.940624d0 75004.344875d0 81491.287984d0  1558.515853d0)
                      ( 708.15d0 -8444.676815d0   624.033993d0   787.163481d0  7774.939698d0)
                      ( 274.20d0  2600.009459d0  1251.136893d0  1251.296102d0 -2219.534038d0)
                      ( 241.45d0  2755.175630d0 -1102.212834d0 -1257.950837d0 -2523.969396d0)
                      (2309.00d0  -167.659835d0 -2660.664980d0 -2966.799730d0   247.850422d0)
                      ( 492.20d0   871.855056d0   699.291817d0   639.744522d0  -846.485643d0)
                      ( 396.10d0    44.769698d0   153.167220d0   131.600209d0 -1393.124055d0)
                      ( 288.90d0  -512.313065d0  -950.865637d0  -445.040117d0   368.526116d0)
                      ( 231.10d0  -819.415595d0   499.754645d0   584.522874d0   749.045012d0)
                      (1610.00d0  -538.071099d0  -145.188210d0   -89.756563d0   444.704518d0)
                      ( 620.00d0  -189.793622d0   558.116553d0   524.429630d0   235.934465d0)
                      ( 157.87d0  -402.922932d0   -23.923029d0   -13.549067d0   374.049623d0)
                      ( 220.30d0   179.516345d0  -165.405086d0  -210.157124d0  -171.330180d0)
                      (1200.00d0    -9.814756d0     9.344131d0   -44.919798d0   -22.899655d0))))
    (multiple-value-bind (xsum ysum)
        (pol-per-sum Tc xypol xyper)
      (let* ((x  (to rad (arcsec xsum)))
             (y  (to rad (arcsec ysum)))
             (z  (sqrt (max 0 (- 1 (* x x) (* y y))))))
        (list x y z)
        ))))

#|
(pequ (c2k 1219339.078000d0))
=> (-0.2943764379736904 -0.11719098023370263 0.9484770882408209)
Check from Vondrak:
For JDN = 1219339.078000
pequ = ( −0.29437643797369031532 −0.11719098023370257855 +0.94847708824082091796 )
 |#

(defun pmat (epoch)
  ;; Compute long term precession matrix. (= M_class)
  ;; Produces a precession matrix that will transform from J2000.0 to Epoch.
  ;; To be applied against an XYZ vector arising from RA, Dec at J2000.0.
  (let* ((Tc   (c2k epoch))
         (zv   (pequ Tc)) ;; pole of Equator
         (eclp (pecl Tc)) ;; pole of Ecliptic
         (xv   (vnormalize (vcross zv eclp)))
         (yv   (vcross zv xv)))
    (list xv yv zv)))

#|
(pmat 1219339.078000d0)
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
;; Using precision long-term Equatorial and Ecliptic pole positions

(defun prec (ra dec &optional (to-epoch (current-epoch)) (from-epoch +j2000+))
  ;; Precess using IAU long-term models for Ecliptic and Equatorial precession.
  ;; Good long-term precession, but no nutation
  (let* ((xyz1  (to-xyz ra dec))
         (xyz2k (mat-mulv (trn (pmat from-epoch)) xyz1))
         (xyz2  (mat-mulv (pmat to-epoch) xyz2k)))
     (to-thphi xyz2)))

