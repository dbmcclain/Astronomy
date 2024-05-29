;; astro-packages.lisp - sub packages split out for convenience definitions
;; DM/RAL  2024/05/29 19:08:47 UTC
;; ----------------------------------------------------------------

(in-package #:com.ral.astro)

(define-astro-package #:com.ral.astro.angle.measure)
(define-astro-package #:com.ral.astro.angle.input)
(define-astro-package #:com.ral.astro.angle.output)
(define-astro-package #:com.ral.astro.angle.rotation)

(define-astro-package #:com.ral.astro.observatory)
(define-astro-package #:com.ral.astro.radec)
(define-astro-package #:com.ral.astro.azel)
(define-astro-package #:com.ral.astro.ha)
(define-astro-package #:com.ral.astro.epoch)
(define-astro-package #:com.ral.astro.siderial)
(define-astro-package #:com.ral.astro.precession)




