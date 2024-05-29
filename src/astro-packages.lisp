;; astro-packages.lisp - sub packages split out for convenience definitions
;; DM/RAL  2024/05/29 19:08:47 UTC
;; ----------------------------------------------------------------

(in-package #:com.ral.astro)

(define-astro-package #:com.ral.astro.angle.measure)
(define-astro-package #:com.ral.astro.angle.input)
(define-astro-package #:com.ral.astro.angle.output)
(define-astro-package #:com.ral.astro.angle.rotation)
(define-astro-package #:com.ral.astro.matrices)
(define-astro-package #:com.ral.astro.poly)

(define-astro-package #:com.ral.astro.observatory)
(define-astro-package #:com.ral.astro.utc)
(define-astro-package #:com.ral.astro.radec)
(define-astro-package #:com.ral.astro.azel)
(define-astro-package #:com.ral.astro.ha)
(define-astro-package #:com.ral.astro.epoch)
(define-astro-package #:com.ral.astro.siderial)
(define-astro-package #:com.ral.astro.precession
  (:export
   #:pmat
   #:pequ
   #:pecl
   #:cio-to-equ
   #:eqx-to-cio
   #:M_CIO
   ))
(define-astro-package #:com.ral.astro.precession.ecl
  (:use #:com.ral.astro.precession))
(define-astro-package #:com.ral.astro.precession.grubby
  (:use #:com.ral.astro.precession))
(define-astro-package #:com.ral.astro.precession.cio
  (:use #:com.ral.astro.precession))
(define-astro-package #:com.ral.astro.precession.cio-aa
  (:use #:com.ral.astro.precession))

(define-astro-package #:com.ral.astro.precession.class
  (:use #:com.ral.astro.precession))

(define-astro-package #:com.ral.astro.precession.expt
  (:use #:com.ral.astro.precession))






