;; precession.lisp
;;
;; DM/RAL  2024/05/20 06:15:21 UTC
;; ----------------------------------

(in-package #:astro)

;; ----------------------------------
;; Precession
;;
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
      (let* ((x  (to-rad (arcsec xsum)))
             (y  (to-rad (arcsec ysum)))
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
  ;; Compute long term precession matrix.
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

(defun trn (m)
  ;; Compute matrix transpose.
  ;; Matrix is a list of 3 element lists representing row vectors.
  ;; For a unitory transform matrix, the transpose is its inverse.
  (apply #'mapcar #'list m))

(defun mat-mulv (m v)
  (mapcar (um:curry #'vdot v) m))

(defun mat-mulm (m1 m2)
  (trn (mapcar (um:curry #'mat-mulv m1) (trn m2))))

;; --------------------------------------------
;; Using precision long-term Equatorial and Ecliptic pole positions

(defun prec (ra dec &optional (to-epoch (current-epoch)) (from-epoch +j2000+))
  ;; Precess using IAU long-term models for Ecliptic and Equatorial precession.
  (let* ((xyz1  (to-xyz ra dec))
         (xyz2k (mat-mulv (trn (pmat from-epoch)) xyz1))
         (xyz2  (mat-mulv (pmat to-epoch) xyz2k)))
     (to-thphi xyz2)))

;; --------------------------------------------------
;; Grubby routine from years ago...

(defun precN (ra dec &optional (nyr (y2k (current-epoch))))
  (let* ((m  #.(secs 3.07496d0))
         (n  #.(arcsec 20.0431d0))
         (cs (* nyr n (cis ra))))
    (values (+ ra (* nyr m) (* (imagpart cs) (tan dec)))
            (+ dec (realpart cs))
            )))

;; ------------------------------------------------
;; ------------------------------------------
;; IAU 2006 GCRS/CIRS Precession
;; ICRS Procedures - International Celestial Reference System
;; Ref, P.T.Wallace and N.Capitaine: "IAU 2006 precession-nutation procedures"

(defun GCRS-XYZ (epoch)
  ;; Compute CIP of epoch.
  ;; Approx, assumes small angle s=0
  (let* ((D  (d2k epoch))
         (Ω  (rad (+ 2.182d0 (* D -9.242d-4))))
         (cs (cis Ω))
         (X  (+ (* D 2.6603d-7)   (* -33.2d-6 (imagpart cs))))
         (Y  (+ (* D D -8.14d-14) (*  44.6d-6 (realpart cs))))
         (Z  (sqrt
              (max 0
                   (- 1 (* x x) (* y y))))
             ))
    `(,X ,Y ,Z)
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
  (db (X Y _) (GCRS-XYZ epoch)
    (declare (ignore _))
    `((  1  0  ,(- X))
      (  0  1  ,(- Y))
      ( ,X ,Y  1    ))
    ))

(defun cio-to-eqx (ra dec epoch)
  ;; Convert CIO-based RA,Dec to EQX-based values.
  (values (- ra (EO epoch))
          dec))

(defun eqx-to-cio (ra dec epoch)
  ;; Convert EQX-based RA, Dec to CIO-based values.
  (values (+ ra (EO epoch))
          dec))

(defun GCRS-to-CIRS (ra dec &optional (epoch (current-epoch)))
  ;; Apply precesssion + nutation to a GCRS J2000.0 position.  On
  ;; entry, RA and Dec should refer to a CIO-based position, not an
  ;; EQX-position.
  (let* ((v_GCRS  (to-xyz ra dec))
         (M_CIO   (M_CIO epoch))
         (v_CIRS  (mat-mulv M_CIO v_GCRS)))
    (to-thphi v_CIRS)
    ))

(defun CIRS-to-GCRS (ra dec &optional (epoch (current-epoch)))
  ;; Undo the precession + nutation from a CIO-based position at
  ;; epoch, to produce the equivalent J2000.0 GCRS position.  On
  ;; entry, RA and Dec should refer to a CIO-based position, not an
  ;; EQX-position.
  (let* ((v_CIRS  (to-xyz ra dec))
         (M_CIO   (trn (M_CIO epoch)))
         (v_GCRS  (mat-mulv M_CIO v_CIRS)))
    (to-thphi v_GCRS)))

;; ------------------------------------------------------------

(defun preca (ra dec &optional (to-epoch (current-epoch)) (from-epoch +j2000+))
  ;; CIRS-based precession
  ;; On entry, RA and Dec should refer to an EQX-based position.
  (mvb (rac decc) ;; convert to CIO-based position
      (eqx-to-cio ra dec from-epoch)
    (mvb (ra2k dec2k)  ;; unwind precession+nutation to reach J2000.0
        (CIRS-to-GCRS rac decc from-epoch)
      (mvb (rap decp) ;; apply precession+nutation for to-epoch
          (GCRS-to-CIRS ra2k dec2k to-epoch)
        (cio-to-eqx rap decp to-epoch) ;; convert to EQX-base position
        ))))

;; ------------------------------------------------------------
#|
;; Check from Wallace and Capitaine
;; For TT = 2400000.5+53750.892855138888889(JD)  20060115T21:24:37.5Z
;;   MCIO ≈ (( +1.00000000000000000 +0.00000000000000000 −0.00058224012792061)
;;           ( +0.00000000000000000 +1.00000000000000000 −0.00004374943683668)
;;           ( +0.00058224012792061 +0.00004374943683668 +1.00000000000000000))
;;
(let* ((tt (+ 2400000.5d0 53750.892855138888889d0))
       (jd (jd_ut1-to-tt
            (jdn 2006 01 15
                 :time (hms 21 24 37.5)
                 :lcl-ut 0))))
  (list (- tt 2400000.5)
        (- jd 2400000.5)
        (to-secs (- jd tt))
        (to-hms (frac (+ 1/2 tt)))
        (to-hms (frac (+ 1/2 jd)))
        (M_CIO tt)))
=>
(53750.89285513898  ;; tt
 53750.892901435495 ;; JD_TT
 4.000018537044525  ;; JD-tt secs
 (HMS 21 25 42.684) ;; tt
 (HMS 21 25 46.684) ;; JD
 ((1 0 -5.822401279206334E-4)
  (0 1 -4.374943683668478E-5)
  (5.822401279206334E-4 4.374943683668478E-5 1)))
;; ---------------
(let* ((ut1 (+ 2400000.5d0 53750.892855138888889d0 (/ 0.3341 +sec/day+))))
  (list
   :era (to-deg (ERA ut1))
   :era (to-hms (ERA ut1))
   (-
   (to-mas (ERA ut1))
   (to-mas
    (unipolar
     (turns (+ 0.7790572732640
               (* 1.00273781191135448
                  (- ut1 2451545.0))))
     )))))
|#

(defun R1 (x)
  (let* ((cs  (cis x))
         (cx  (realpart cs))
         (sx  (imagpart cs)))
    `((  1    0     0)
      (  0   ,cx   ,sx)
      (  0 ,(- sx) ,cx))
    ))

(defun R2 (x)
  (let* ((cs  (cis x))
         (cx  (realpart cs))
         (sx  (imagpart cs)))
    `((  ,cx    0  ,(- sx))
      (   0     1     0)
      (  ,sx    0   ,cx))
    ))

(defun R3 (x)
  (let* ((cs  (cis x))
         (cx  (realpart cs))
         (sx  (imagpart cs)))
    `((  ,cx    ,sx    0)
      (,(- sx)  ,cx    0)
      (   0       0    1))
    ))

(defun R_TIRS (epoch)
  ;; Simplified precession, good to 0.12 arcsec in 21st cy,
  ;; good to 0.85 arcsec over ±2 cy
  ;;
  ;;  v_TIRS = R(TT,UT) . v_GCRS
  ;;  v_CIRS = M_CIO(TT) . v_GCRS
  ;;
  ;; This function is R(TT,UT).
  ;;
  ;; A positive rotation, R3, subtracts the ERA(epoch) from the RA.
  ;;
  (let* ((M_CIO (M_CIO epoch))
         (ERA   (ERA epoch))
         (R3    (R3 ERA)))
    (mat-mulm R3 M_CIO)))

(defun GCRS-to-TIRS (ra dec &optional (epoch (current-epoch)))
  ;; On entry, RA and Dec should refer to a CIO-based J2000.0
  ;; position.  Computes:
  ;;
  ;;      (RA,Dec) -> Prec(RA,Dec,epoch) - ERA(epoch) = -HA(epoch).
  ;;
  ;; This is the -HA of the position at Greenwich on epoch. Done
  ;; entirely without needing knowledge of GMST, nor the Equinox.
  (let* ((v_GCRS  (to-xyz ra dec))
         (R_TIRS  (R_TIRS epoch))
         (v_TIRS  (mat-mulv R_TIRS v_GCRS)))
    (to-thphi v_TIRS)
    ))

(defun TIRS-to-GCRS (ra dec &optional (epoch (current-epoch)))
  ;; On entry, RA and Dec should refer to a CIO-based position. This
  ;; computes CIO-based RA, Dec at J2000.0, given the RA = -HA at
  ;; Greenwich on epoch.
  (let* ((v_TIRS  (to-xyz ra dec))
         (R_TIRS  (trn (R_TIRS epoch)))
         (v_GCRS  (mat-mulv R_TIRS v_TIRS)))
    (to-thphi v_GCRS)
    ))

;; -------------------------------------------------------------
;; -------------------------------------------------------------

(defun chk-prec (epoch_TT)
  ;; Check our system
  ;; Needs values for: s, Δψ_2000A, Δε_2000A
  (let* ((τ   (c2k epoch_TT))
         (f   (* -2.7774d-6 τ))
         
         (γ_  (arcsec
               (poly-eval τ '(   -0.052928d0
                                 10.556378d0
                                 0.4932044d0
                                 -0.00031238d0
                                 -0.000002788d0
                                 0.0000000260d0)) ))
         (csγ (cis γ_))
         (cγ  (realpart csγ))
         (sγ  (imagpart csγ))
         
         (φ_  (arcsec
               (poly-eval τ '( 84381.412819d0
                               -46.811016d0
                               0.0511268d0
                               0.00053289d0
                               -0.000000440d0
                               -0.0000000176d0)) ))
         (csφ (cis φ_))
         (cφ  (realpart csφ))
         (sφ  (imagpart csφ))
         
         (ψ_  (arcsec
               (poly-eval τ '(   -0.041775d0
                                 5038.481484d0
                                 1.5584175d0
                                 -0.00018522d0
                                 -0.000026452d0
                                 -0.0000000148d0)) ))
         (Δψ_2000A  0) ;; ??
         (Δψ  (* Δψ_2000A (+ 1 0.4697d-6 f))) 
         (csψ (cis (+ ψ_ Δψ)))
         (cψ  (realpart csψ))
         (sψ  (imagpart csψ))
         
         (ε_A (arcsec
               (poly-eval τ '(84381.406d0
                              -46.836769d0
                              -0.0001831d0
                              0.00200340d0
                              -0.000000576d0
                              -0.0000000434d0)) ))
         (Δε_2000A  0) ;; ??
         (Δε  (* Δε_2000A (+ 1 f)))
         (csε (cis (+ ε_A Δε)))
         (cε  (realpart csε))
         (sε  (imagpart csε))

         ;; CIP is (X, Y, Z)
         (tmp (- (* sε cψ cφ) (* cε sφ)))
         (X   (- (* sε sψ cγ) (* sγ tmp)))
         (Y   (+ (* sε sψ sγ) (* cγ tmp)))
         (Z   (sqrt
               (max 0
                    (- 1 (* X X) (* Y Y)))))

         (_   (let ()
                (db (Xq Yq Zq) (GCRS-XYZ epoch_TT) ;; get quick est
                  ;; shows 7.7 as for J2000
                  ;;       8.9 as for J2024
                  (print `(:ΔCIP  ,(to-arcsec (acos (vdot `(,X ,Y ,Z) `(,Xq ,Yq ,Zq))))))
                  )))

         ;; top row of M_clas, Γ
         (Γ   `(,(+ (* cψ cγ) (* sψ cφ sγ))
                ,(- (* cψ sγ) (* sψ cφ cγ))
                ,(- (* sψ sφ)) ))

         ;; middle row of M_class, yv
         (tmp (+ (* cε cψ cφ) (* sε sφ)))
         (yv  `(,(- (* cε sψ cγ) (* sγ tmp))
                ,(+ (* cε sψ sγ) (* cγ tmp))
                ,(- (* cε cψ sφ) (* sε cφ)) ))

         ;; unit vector directed to point Σ, top row of M_Σ
         (a   (/ (1+ z)))
         (Σ   `(,(- 1 (* a X X))
                ,(- (* a X Y))
                ,(- X)))
         
         ;; Check 1st way, against explicitly constructed M_class.
         (EO   (EO epoch_TT))
         (s    0) ;; (* -1/2 X Y)) ;; ??
         
         (EO1  (- s (atan (vdot yv Σ) (vdot Γ Σ))))
         
         ;; Check 2nd way...
         ;; Construct M_class by introducing the Ecliptic pole, kv.
         ;; Then M_class = [(nv >< kv), nv >< (nv >< kv), nv], where
         ;; >< denotes vector cross-product, nv = (X, Y, Z) is CIP.

         ;; Ecliptic Pole, kv
         (kv   `(,(* sφ sγ)
                 ,(- (* sφ cγ))
                 ,cφ))

         ;; CIP, nv
         (nv   `(,X ,Y ,Z))
         
         (Γn   (vcross nv kv))
         (yvn  (vcross nv Γn))
         (EO2  (- s (atan (vdot yvn Σ) (vdot Γn Σ))))

         ;; Compare Long Term Ecliptic model
         ;; Shows +52.8 mas for J2000
         ;;       +51.3 mas for J2024   
         (kvlt (pecl (c2k epoch_TT)))
         (Γlt  (vcross nv kvlt))
         (yvlt (vcross nv Γlt))
         (EOlt (- s (atan (vdot yvlt Σ) (vdot Γlt Σ))))

         ;; Compae with quick XYZ. using quick XYZ to compute CIP
         ;; against true form of ecliptic, kv from above.
         (nv_q (GCRS-XYZ epoch_TT))
         (Σq   (db (Xq Yq Zq) nv_q
                 (let ((aq (/ (1+ Zq))))
                   `(,(- 1 (* aq Xq Xq))
                     ,(- (* aq Xq Yq))
                     ,(- Xq))
                   )))
         (Γq   (vcross nv_q kv))
         (yvq  (vcross nv_q Γq))
         (EOq  (- s (atan (vdot yvq Σq) (vdot Γq Σq))))

         ;; Compare using Long Term Ecliptic with Quick XYZ
         (Γqlt  (vcross nv_q kvlt))
         (yvqlt (vcross nv_q Γq))
         (EOqlt (- s (atan (vdot yvqlt Σq) (vdot Γqlt Σq))))
         )
    (declare (ignore _))
    ;; Shows ΔEO_1 = ΔEO_2
    ;;       = -0.094 mas for J2000, ΔEO_lt = 52.8 mas, ΔEO_q = 12.9 as, ΔEO_qlt = 12.9 as
    ;;       = -1.5 mas for J2024, ΔEO_lt = 51.3 mas, ΔEO_q = 5.6 as, ΔEO_qlt = 5.6 as
    (let ((*print-length* nil))
      (print
       (list
        :EO      (to-hms EO)
        :ΔEO_1   (to-mas (- EO1  EO))
        :ΔEO_2   (to-mas (- EO2  EO))
        :ΔEO_lt  (to-mas (- EOlt EO))
        :ΔEO_q   (to-arcsec (- EOq  EO))
        :ΔEO_qlt (to-arcsec (- EOqlt EO)) ))
      (values))))

;; -------------------------------------------------------------
;; ---------------------------------------------------
;;
;; From Explanatory Supplement to American Almanac
;; Is this better than M_CIO? Ought to be about the same.
;;
(defun GCRS-XY-aa (epoch)
  (let* ((Tc  (c2k epoch))
         (L   (deg (+ 280.5d0 (* Tc 36_000.8))))
         (Ω   (deg (+ 125.0d0 (* Tc -1934.1d0))))
         (X   (arcsec (+ (* Tc 2004.19d0)
                         (* Tc Tc -0.43d0)
                         (* -6.84d0 (sin Ω))
                         (* -0.52d0 (sin (+ L L))))
                      ))
         (Y   (arcsec (+ (* Tc -0.03d0)
                         (* Tc Tc -22.41d0)
                         (* 9.21d0 (cos Ω))
                         (* 0.57d0 (cos (+ L L))))
                      )))
    `(,(to-rad X)  ;; ≈ (Sin X)
      ,(to-rad Y)) ;; ≈ (Sin Y)
    ))
  
(defun mcio-aa (epoch)
  (db (X Y)
      (GCRS-XY-aa epoch)
    (let ((cX  (- 1 (* 1/2 X X)) )) ;; ≈ (Cos X)
      `(( ,cX  0   ,(- X) )
        (  0   1   ,(- Y) )
        ( ,X  ,Y    ,cX   ))
    )))

(defun GCRS-to-CIRS-aa (ra dec &optional (epoch (current-epoch)))
  (let* ((mat     (mcio-aa epoch))
         (v_GCRS  (to-xyz ra dec))
         (v_CIRS  (mat-mulv mat v_GCRS)))
    (to-thphi v_CIRS)
    ))

(defun CIRS-to-GCRS-aa (ra dec &optional (epoch (current-epoch)))
  (let* ((mat     (trn (mcio-aa epoch)))
         (v_CIRS  (to-xyz ra dec))
         (v_GCRS  (mat-mulv mat v_CIRS)))
    (to-thphi v_GCRS)
    ))

;; ------------------------------------------------------

(defun prec-aa (ra dec &optional (to-epoch (current-epoch)) (from-epoch +j2000+))
  ;; CIRS-based precession
  ;; On entry, RA and Dec should refer to an EQX-based position.
  (mvb (rac decc) ;; convert to CIO-based position
      (eqx-to-cio ra dec from-epoch)
    (mvb (ra2k dec2k)  ;; unwind precession+nutation to reach J2000.0
        (CIRS-to-GCRS-aa rac decc from-epoch)
      (mvb (rap decp) ;; apply precession+nutation for to-epoch
          (GCRS-to-CIRS-aa ra2k dec2k to-epoch)
        (cio-to-eqx rap decp to-epoch) ;; convert to EQX-base position
        ))))

;; ------------------------------------------------------

