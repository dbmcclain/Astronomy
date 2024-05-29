;; packages.lisp -- package declarations for Astro
;; DM/RAL  2024/05/20 06:02:05 UTC

(in-package :cl-user)

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
   #:cmttr
   #:vcross
   #:vnorm
   #:vnormalize
   #:vadd
   #:vscale
   #:rot

   ;; Matrix Operations
   #:trn
   #:mat-mulv
   #:mat-milm

   ;; Astronomical Angles
   #:ra
   #:dec

   #:to-ra
   #:to-dec

   #:to-ra-h.ms
   #:to-dec-d.ms
   #:to-ha-h.ms

   #:ra-to-ha
   #:ha-to-ra

   ;; Astronomical Epochs
   #:+J2000+
   #:ymd
   #:jdn
   #:current-epoch
   #:date.time
   #:d.t

   ;; Astronomical Precession + Nutation
   #:era
   #:prec
   #:precn
   #:preca
   #:prec-aa

   ;; Siderial Time
   #:gmst
   #:lmst
   #:ha

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
