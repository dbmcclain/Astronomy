(in-package #:user)

(asdf:defsystem "com.ral.astro"
  :description "astro: Useful routines for Astronomy"
  :version     "1.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2012 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :components  ((:file "packages")
                (:file "convenience")
                (:file "astro-packages")
                (:file "angle-measure")
                (:file "angle-input")
                (:file "angle-output")
                (:file "angle-rotations")
                (:file "matrices")
                (:file "poly")
                (:file "observatory")
                (:file "radec")
                (:file "epoch")
                (:file "utc")
                (:file "siderial-time")
                (:file "precession")
                (:file "precession-ecl")
                (:file "precession-grubby")
                (:file "precession-cio")
                (:file "precession-cio-aa")
                (:file "precession-expt")
                (:file "hour-angle")
                (:file "azel"))
  :serial       t
  :depends-on   ("com.ral.useful-macros"
                 ))

