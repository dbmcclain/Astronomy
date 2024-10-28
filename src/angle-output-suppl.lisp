;; angle-output-suppl.lisp
;;
;; DM/RAL  2024/10/15 15:19:46 UTC
;; ----------------------------------

(in-package #:com.ral.astro.angle.output)

;; --------------------------------------------

(defclass >dms (um:fdpl)
  ;; ±DDD MM SS.s
  ()
  (:default-initargs
   :ndpl  0
   :flags '(:colon-char #\space)
   :fmt   '(sign+ ds ddc ddc)
   ))

(defmethod um:fdpl-prepval ((x >dms))
  (to arcsec (bipolar (um:val-of x))))

(defun >dms (x &rest args)
  (apply #'make-instance '>dms
         :val   x
         args))

#|
(>dms (turns 0.8))                      
|#

;; --------------------------------------------

(defclass >dm (>dms)
  ;; ±DDD MM.m
  ()
  (:default-initargs
   :fmt '(sign+ ds ddc)
   ))

(defmethod um:fdpl-prepval ((x >dm))
  (to arcmin (bipolar (um:val-of x))))

(defun >dm (x &rest args)
  (apply #'make-instance '>dm
         :val   x
         args))

#|
(>dm (turns 0.8))                      
|#
;; --------------------------------------------

(defclass >ha (>dms)
  ;; Bipolar ±HH MM SS.s
  ()
  (:default-initargs
   :ndpl 1
   :fmt  '(sign+ ds ddc ddc dp nd)
   ))

(defmethod um:fdpl-prepval ((x >ha))
  (to secs (bipolar (um:val-of x))))

(defun >ha (x &rest args)
  (apply #'make-instance '>ha
         :val   x
         args))

#|
(>ha (turns 0.8))                      
|#
;; --------------------------------------------

(defclass >hms (>ha)
  ;; Unipolar HH MM SS.s
  ()
  (:default-initargs
   :ndpl  1
   :fmt   '(ds ddc ddc dp nd)
   ))

(defmethod um:fdpl-prepval ((x >hms))
  (to secs (unipolar (um:val-of x))))
  
(defun >hms (x &rest args)
  (apply #'make-instance '>hms
         :val   x
         args))

#|
(>hms (turns 0.8))                      
|#
;; --------------------------------------------

(defclass >hm (>ha)
  ;; Unipolar HH MM.m
  ()
  (:default-initargs
   :ndpl  1
   :fmt   '(ds ddc dp nd)
   ))

(defmethod um:fdpl-prepval ((x >hm))
  (to mins (unipolar (um:val-of x))))

(defun >hm (x &rest args)
  (apply #'make-instance '>hm
         :val   x
         args))

#|
(>hm (turns 0.8))                      
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

