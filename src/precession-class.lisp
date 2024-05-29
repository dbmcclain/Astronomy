;; precession-class - Precession based on Classical Equinox transforms
;;
;; DM/RAL  2024/05/29 20:27:43 UTC
;; -------------------------------------------------------------

(in-package #:com.ral.astro.precession.class)

;; --------------------------------------------------------------

(defun fuku (epoch_TT)
  ;; PB angles from Fukushima-Williams wrt GCRS.
  ;; For IAU 2006 Precession
  (let* ((τ   (c2k epoch_TT))
         ;;
         ;;  Naming the following points:
         ;;
         ;;           e = J2000.0 ecliptic pole,
         ;;           p = GCRS pole,
         ;;           E = mean ecliptic pole of date,
         ;;     and   P = mean pole of date,
         ;;
         ;;     the four Fukushima-Williams angles are as follows:
         ;;
         ;;        γ_  = epE
         ;;        φ_  = pE
         ;;        ψ_  = pEP
         ;;        ε_A = EP
         ;;

         (γ_  (arcsec
               (poly-eval τ '(   -0.052928d0
                                 10.556378d0
                                  0.4932044d0
                                 -0.00031238d0
                                 -0.000002788d0
                                  0.0000000260d0)) ))
         (φ_  (arcsec
               (poly-eval τ '( 84381.412819d0
                                 -46.811016d0
                                   0.0511268d0
                                   0.00053289d0
                                  -0.000000440d0
                                  -0.0000000176d0)) ))
         (ψ_  (arcsec
               (poly-eval τ '(     -0.041775d0
                                 5038.481484d0
                                    1.5584175d0
                                   -0.00018522d0
                                   -0.000026452d0
                                   -0.0000000148d0)) ))
         (ε_A (arcsec
               (poly-eval τ '(84381.406d0
                                -46.836769d0
                                 -0.0001831d0
                                  0.00200340d0
                                 -0.000000576d0
                                 -0.0000000434d0)) )) )
    (values γ_ φ_ ψ_ ε_A)
    ))

(defun nut2000A (epoch_TT)
  (values 0 0)) ;; can't we do better here?

(defun nut2006 (epoch_TT)
  ;; 2000A Nutation corrected to be consistent with IAU 2006 Precesssion
  (mvb (Δψ_2000A Δε_2000A)
      (nut2000A epoch_TT)
    (let* ((τ  (c2k epoch_TT))
           (f  (* -2.774d-6 τ)))
      
      (values (* Δψ_2000A (+ 1 0.4697d-6 f))
              (* Δε_2000A (+ 1 f)))
      )))

(defun M_class (epoch_TT)
  ;; Equinox-based NPB matrix
  (mvb (γ_ φ_ ψ_ ε_A)
      (fuku epoch_TT)
    (mvb (Δψ Δε)
        (nut2006 epoch_TT)
      (let* ((ψ    (+ ψ_  Δψ))
             (ε    (+ ε_A Δε))
             (R3γ_ (R3 γ_))
             (R1φ_ (R1 φ_))
             (R3mψ (R3 (- ψ)))
             (R1mε (R1 (- ε))))
        ;; Bottom row will be the GCRS unit vector to CIP (X, Y, Z)
        (mat-mulm R1mε
                  (mat-mulm R3mψ
                            (mat-mulm R1φ_ R3γ_)))
        ))))
                            
#|
(let* ((epoch +j2000+))
  (list
   (pmat    epoch)
   (m_class epoch)))

(plt:fplot 'plt '(0 1)
           (lambda (dt)
             (let* ((epoch (ymd (+ 2000 dt)))
                    (xyz   (gcrs-xy epoch))
                    (xya   (gcrs-xy-aa epoch)))
               (to-arcsec (rad (- (first xya) (first xyz))))
               ))
           :clear t
           :thick 2)

(plt:paramplot 'plt '(-50 50)
               (lambda (dt)
                 (let* ((epoch (ymd (+ 2000 dt)))
                        (pmat    (pmat epoch)))
                   (first (third pmat))
                   ))
               (lambda (dt)
                 (let* ((epoch (ymd (+ 2000 dt)))
                        (pmat    (pmat epoch)))
                   (second (third pmat))
                   ))
                :clear t
                ;; :aspect 1
                :thick 2
                ;; :xrange '(-1 1)
                ;; :yrange '(-1 1)
                )
(plt:paramplot 'plt '(-50 50)
               (lambda (dt)
                 (let* ((epoch (ymd (+ 2000 dt)))
                        (m_class (m_class epoch)))
                   (first (third m_class))
                   ))
               (lambda (dt)
                 (let* ((epoch (ymd (+ 2000 dt)))
                        (m_class (m_class epoch)))
                   (second (third m_class))
                   ))
                :color :red
                ;; :aspect 1
                :thick 2
                )
|#

#|
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
                (db (Xq Yq) (GCRS-XY epoch_TT) ;; get quick est
                  (let ((zq (sqrt (max 0 (- 1 (* xq xq) (* yq yq))))))
                    ;; shows 7.7 as for J2000
                    ;;       8.9 as for J2024
                    (print `(:ΔCIP  ,(to-arcsec (acos (vdot `(,X ,Y ,Z) `(,Xq ,Yq ,Zq))))))
                    ))))

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
         (nv_q (GCRS-XY epoch_TT))
         (Σq   (db (Xq Yq) nv_q
                 (let* ((Zq (sqrt (max 0 (- 1 (* Xq Xq) (* Yq Yq)))))
                        (aq (/ (1+ Zq))))
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
|#
