;; angle-output-suppl.lisp
;;
;; DM/RAL  2024/10/15 15:19:46 UTC
;; ----------------------------------

(in-package #:com.ral.astro.angle.output)

;; --------------------------------------------

(defclass >dms+ (um:fdpl)
  ;; ±DDD MM SS.s
  ()
  (:default-initargs
   :ndpl       0
   :>dms+flags '(:colon-char #\space)
   :fmt        '(sign+ ds ddc ddc)
   ))

(defmethod um:fdpl-prepval ((x >dms+))
  (to arcsec (bipolar (um:val-of x))))

(defun >dms+ (x &rest args)
  (apply #'um:fdpl-maker '>dms+ x args))

#|
(>dms+ (turns 0.4))                      
|#

;; --------------------------------------------

(defclass >dms (>dms+)
  ;; -DDD MM SS.s
  ()
  (:default-initargs
   :fmt   '(sign ds ddc ddc)
   ))

(defun >dms (x &rest args)
  (apply #'um:fdpl-maker '>dms x args))

#|
(>dms (turns 0.4))                      
|#

;; --------------------------------------------

(defclass >dm+ (>dms+)
  ;; ±DDD MM.m
  ()
  (:default-initargs
   :fmt '(sign+ ds ddc)
   ))

(defmethod um:fdpl-prepval ((x >dm+))
  (to arcmin (bipolar (um:val-of x))))

(defun >dm+ (x &rest args)
  (apply #'um:fdpl-maker '>dm+ x args))

#|
(>dm+ (turns 0.4))                      
|#
;; --------------------------------------------

(defclass >dm (>dm+)
  ;; -DDD MM.m
  ()
  (:default-initargs
   :fmt '(sign ds ddc)
   ))

(defun >dm (x &rest args)
  (apply #'um:fdpl-maker '>dm x args))

#|
(>dm (turns 0.4))                      
|#
;; --------------------------------------------

(defclass >ha (>dms+)
  ;; Bipolar ±HH MM SS.s
  ()
  (:default-initargs
   :ndpl 1
   :fmt  '(sign+ ds ddc ddc dp ndpl)
   ))

(defmethod um:fdpl-prepval ((x >ha))
  (to secs (bipolar (um:val-of x))))

(defun >ha (x &rest args)
  (apply #'um:fdpl-maker '>ha x args))

#|
(>ha (turns 0.4))                      
|#
;; --------------------------------------------

(defclass >hms (>ha)
  ;; Unipolar HH MM SS.s
  ()
  (:default-initargs
   :fmt   '(ds ddc ddc dp ndpl)
   ))

(defmethod um:fdpl-prepval ((x >hms))
  (to secs (unipolar (um:val-of x))))
  
(defun >hms (x &rest args)
  (apply #'um:fdpl-maker '>hms x args))

#|
(>hms (turns 0.4))                      
|#
;; --------------------------------------------

(defclass >hm (>hms)
  ;; Unipolar HH MM.m
  ()
  (:default-initargs
   :ndpl  1
   :fmt   '(ds ddc dp ndpl)
   ))

(defmethod um:fdpl-prepval ((x >hm))
  (to mins (unipolar (um:val-of x))))

(defun >hm (x &rest args)
  (apply #'um:fdpl-maker '>hm x args))

#|
(>hm (turns 0.4))                      
|#
;; --------------------------------------------
#|
(to arcsec 0.123456)
(to arcsec 0.0123456)

(>dms 0.123456)
(>dms -0.123456)
(>dms 0.123456 :ndpl 2 :fmt '(sign+ ds ddc ddc dp nd))

(>dm 0.123456)
(>dm -0.123456)
(>dm 0.123456 :ndpl 2 :fmt '(sign+ ds ddc dp nd))

(>hms 0.123456)
(>hms -0.123456)
(>hms 0.123456 :ndpl 0 :fmt '(ds ddc ddc))

(>hm 0.123456)
(>hm -0.123456)
(>hm 0.123456 :ndpl 0 :fmt '(ds ddc))

(inspect (>ha 0.123456))
(>ha -0.123456)
(>ha 0.123456 :ndpl 0 :fmt '(sign+ ds ddc ddc))
|#

