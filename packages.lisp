;; packages.lisp -- package declarations for Astro
;; DM/RAL  2024/05/20 06:02:05 UTC

(in-package :cl-user)

(defpackage #:astro.convenience
  (:use #:common-lisp)
  (:export
   #:db
   #:mvb
   #:mvl
   #:map-mult
   ))

(defpackage #:astro.angle
  (:use #:common-lisp #:astro.convenience)
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
   #:set-ang-mode
   
   #:turns
   #:deg
   #:arcmin
   #:arcsec
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

   #:sin
   #:cos
   #:tan
   #:asin
   #:acos
   #:atan
   #:cis
   #:phase

   #:rtop
   #:ptor
   #:to-xyz
   #:to-thphi
   #:rotx
   #:rotx-ang

   #:frac
   #:unipolar
   #:bipolar

   #:vdot
   #:cmttr
   #:vcross
   #:vnorm
   #:vnormalize
   #:vadd
   #:vscale
   #:rot
   ))


(defpackage #:astro
  (:use #:common-lisp #:astro.convenience #:astro.angle)
  (:shadowing-import-from #:astro.angle
   #:sin
   #:cos
   #:tan
   #:asin
   #:acos
   #:atan
   #:cis
   #:phase)
  #.`(:export
      ,@(loop for sym being the external-symbols of '#:astro.angle
              collect sym))
  #.`(:export
      ,@(loop for sym being the external-symbols of '#:astro.convenience
              collect sym))
  (:export
   #:ra
   #:dec

   #:to-ra
   #:to-dec

   #:to-ra-h.ms
   #:to-dec-d.ms
   #:to-ha-h.ms

   #:ra-to-ha
   #:ha-to-ra

   #:ymd
   #:jdn
   #:current-epoch
   #:date.time
   #:d.t

   #:era
   #:prec
   #:precn

   #:gmst
   #:lmst
   #:ha

   #:*qth-lon*
   #:*qth-lat*
   #:*qth-elev*
   #:*qth-tz*

   #:*j2000*
   #:*precession*
   #:*mean-obliquity*
   #:to-ecliptic
   #:from-ecliptic
   #:obliquity-for-epoch

   #:parallactic-angle
   #:hadec-to-azel
   #:azel-to-hadec
   #:radec-to-azel
   #:azel-to-radec
   #:airmass
   #:radec-airmass
   #:hadec-airmass
))


