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
   :fmt        'cl-stk:dms+
   ))

(defmethod um:fdpl-prepval ((x >dms+))
  (to turns (bipolar (um:val-of x))))

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
   :fmt   'cl-stk:dms
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
   :fmt 'cl-stk:dm+
   ))

(defmethod um:fdpl-prepval ((x >dm+))
  (to turns (bipolar (um:val-of x))))

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
   :fmt 'cl-stk:dm
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
   :fmt  'cl-stk:hms+
   ))

(defmethod um:fdpl-prepval ((x >ha))
  (to turns (bipolar (um:val-of x))))

(defun >ha (x &rest args)
  (apply #'um:fdpl-maker '>ha x args))

#|
(>ha (turns 0.6))                      
|#
;; --------------------------------------------

(defclass >hms (>ha)
  ;; Unipolar HH MM SS.s
  ()
  (:default-initargs
   :fmt   'cl-stk:hms
   ))

(defmethod um:fdpl-prepval ((x >hms))
  (to turns (unipolar (um:val-of x))))
  
(defun >hms (x &rest args)
  (apply #'um:fdpl-maker '>hms x args))

#|
(>hms (turns 0.6))                      
|#
;; --------------------------------------------

(defclass >hm (>hms)
  ;; Unipolar HH MM.m
  ()
  (:default-initargs
   :ndpl  1
   :fmt   'cl-stk:hm
   ))

(defmethod um:fdpl-prepval ((x >hm))
  (to turns (unipolar (um:val-of x))))

(defun >hm (x &rest args)
  (apply #'um:fdpl-maker '>hm x args))

#|
(>hm (turns 0.6))                      
|#
;; --------------------------------------------
#|
(to arcsec 0.123456)
(to arcsec 0.0123456)

(>dms -0.423456 :width 10)
(>dms -0.123456 :width 10)
(>dms  0.123456 :ndpl 2 :width 13)

(>dm 0.123456)
(>dm -0.123456)
(>dm 0.123456 :ndpl 2)

(>hms  0.123456 :width 10)
(>hms -0.123456 :width 10)
(>hms 0.123456 :ndpl 0 :width 8)

(>hm 0.123456)
(>hm -0.123456)
(>hm 0.123456 :ndpl 0)

(>ha 0.123456 :width 11)
(>ha -0.423456 :width 11)
(>ha 0.123456 ::width 9 :ndpl 0)

(let ((ha   (>ha 0.123456 :width 11))
      (degs (>dms -0.45882 :width 10)))
  (princ #1>.end
 HA(+W/-E)        RA
 hh mm ss       °  '  "
-----------  ----------
$ha  $degs
.end)
  (values))

|#

