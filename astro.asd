(in-package #:user)

(asdf:defsystem "astro"
  :description "astro: Useful routines for Astronomy"
  :version     "1.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :license     "Copyright (c) 2012 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :components  ((:file "packages")
                (:file "convenience")
                (:file "angle-measure")
                (:file "angle-input")
                (:file "angle-output")
                (:file "angle-rotations")
                (:file "observatory")
                (:file "radec")
                (:file "epoch")
                (:file "precession")
                (:file "siderial-time")
                (:file "hour-angle")
                (:file "azel"))
  :serial       t
  :depends-on   ("com.ral.useful-macros"
                 ))

