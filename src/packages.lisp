;; packages.lisp -- package declarations for Astro
;; DM/RAL  2024/05/20 06:02:05 UTC

(in-package :cl-user)

(project:defproject
 (#:fdpl  #:com.ral.useful-macros.fdpl))

(defpackage #:com.ral.astro
  (:use #:common-lisp)
  (:shadow
   #:sin
   #:cos
   #:tan
   #:asin
   #:acos
   #:atan
   #:cis
   #:phase)
  
  (:export
 
   ;; Convenience Macros
   #:db
   #:mvb
   #:mvl
   #:map-mult
   #:define-astro-package

   ;; Angle Measure
   #:set-ang-mode
   #:report-ang-mode
   #:1turn
   
   #:turns
   #:deg
   #:arcmin
   #:arcsec
   #:mas
   #:hrs
   #:mins
   #:secs
   #:rad
   #:mrad
   #:μrad
   #:d.ms
   #:h.ms

   #:to-turns
   #:to-deg
   #:to-arcmin
   #:to-arcsec
   #:to-mas
   #:to-hrs
   #:to-mins
   #:to-secs
   #:to-rad
   #:to-mrad
   #:to-μrad
   
   #:dms
   #:to-dms
   #:to-d.ms

   #:hms
   #:to-hms
   #:to-h.ms

   #:to

   #:>dms
   #:>dm
   #:>hms
   #:>ha
   #:>hm
   #:angle
   
   ;; Redefined Trig Functions
   #:sin
   #:cos
   #:tan
   #:asin
   #:acos
   #:atan
   #:cis
   #:phase

   ;; Generalized Rotations
   #:rtop
   #:ptor
   #:to-xyz
   #:to-thphi
   #:rotx
   #:rotx-ang

   ;; Modulo Functions
   #:frac
   #:unipolar
   #:bipolar

   ;; Vector Operations
   #:vdot
   #:vcross
   #:vnorm
   #:vnormalize
   #:vadd
   #:vscale
   #:rot

   ;; Matrix Operations
   #:trn
   #:mat-mulv
   #:mat-mulm

   #:r1
   #:r2
   #:r3

   ;; Polynomial Evaluation
   #:poly-eval
   
   ;; Astronomical Angles
   #:ra
   #:dec

   #:pic-fmt
   #:format-ra
   #:format-dec
   
   #:to-ra
   #:to-dec

   #:to-ra-h.ms
   #:to-dec-d.ms
   #:to-ha-h.ms

   #:ra-to-ha
   #:ha-to-ra

   ;; Astronomical Epochs
   #:+J2000+
   #:+sec/day+
   #:+days/year+
   #:+days/cent+
   
   #:ymd
   #:jdn
   #:current-epoch
   #:date.time
   #:d.t

   #:jyrs
   #:jcs
   
   #:d2k
   #:y2k
   #:c2k
   
   ;; Astronomical Precession + Nutation
   #:prec
   #:precn
   #:preca
   #:prec-aa

   #:radec
   #:to-radec
   #:to-mn-radec
   
   ;; Siderial Time
   #:*ΔAT*
   #:*DUT1*
   #:+TAI-OFFSET+

   #:JD_UTC-to-UT1
   #:JD_UT1-to-TT
   #:JD_TT-to-UT1
   
   #:gmst
   #:lmst
   #:ha

   #:era
   #:eo

   ;; Observatory Location
   #:*qth-lon*
   #:*qth-lat*
   #:*qth-elev*
   #:*qth-tz*

   ;; Observing Parameters
   #:parallactic-angle
   #:hadec-to-azel
   #:azel-to-hadec
   #:radec-to-azel
   #:azel-to-radec
   #:airmass
   #:radec-airmass
   #:hadec-airmass
   ))

(defpackage #:com.ral.astro.convenience
  (:use #:common-lisp #:com.ral.astro)
  (:shadowing-import-from #:com.ral.astro
      #:sin
      #:cos
      #:tan
      #:asin
      #:acos
      #:atan
      #:cis
      #:phase
      ))
