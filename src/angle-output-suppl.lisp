;; angle-output-suppl.lisp
;;
;; DM/RAL  2024/10/15 15:19:46 UTC
;;
;; Type     NDpl     Uni/Bi
;; >dms+     0       Bipolar
;; >dms      0       Unipolar
;; >dm+      0       Bipolar
;; >dm       0       Unipolar
;; >ha       1       Bipolar
;; >hms      1       Unipolar
;; >hm       1       Unipolar
;;
;; Bipolar always prints leading sign, +/-, values modulo [-0.5, 0.5] turns
;; Unipolar never prints leading sign, values modulo [0.0, 1.0] turns.
;; ----------------------------------

(in-package #:com.ral.astro.angle.output)

;; --------------------------------------------

(defclass >dms+ (um:fdpl)
  ;; ±DDD MM SS.s
  ;; Bipolar, always shows leading sign, even when >0.
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
(>dms+ (turns 0.6))                      
|#

;; --------------------------------------------

(defclass >dms (>dms+)
  ;; DDD MM SS.s
  ;; Unipolar, never has a leading sign.
  ()
  (:default-initargs
   :fmt   'cl-stk:dms
   ))

(defmethod um:fdpl-prepval ((x >dms))
  (to turns (unipolar (um:val-of x))))

(defun >dms (x &rest args)
  (apply #'um:fdpl-maker '>dms x args))

#|
(>dms (turns 0.4))
(>dms (turns 0.6))
|#

;; --------------------------------------------

(defclass >dm+ (>dms+)
  ;; ±DDD MM.m
  ;; Always shows leading sign, even when >0.
  ()
  (:default-initargs
   :fmt 'cl-stk:dm+
   ))

#| ;; inherited from >dms+
(defmethod um:fdpl-prepval ((x >dm+))
  (to turns (bipolar (um:val-of x))))
|#

(defun >dm+ (x &rest args)
  (apply #'um:fdpl-maker '>dm+ x args))

#|
(>dm+ (turns 0.4))                      
(>dm+ (turns 0.6))                      
|#
;; --------------------------------------------

(defclass >dm (>dms)
  ;; -DDD MM.m
  ()
  (:default-initargs
   :fmt 'cl-stk:dm
   ))

(defun >dm (x &rest args)
  (apply #'um:fdpl-maker '>dm x args))

#|
(>dm (turns 0.4))                      
(>dm (turns 0.6))                      
|#
;; --------------------------------------------

(defclass >ha (>dms+)
  ;; Bipolar ±HH MM SS.s
  ()
  (:default-initargs
   :ndpl 1
   :fmt  'cl-stk:hms+
   ))

#| ;; inherited from >dms+
(defmethod um:fdpl-prepval ((x >ha))
  (to turns (bipolar (um:val-of x))))
|#

(defun >ha (x &rest args)
  (apply #'um:fdpl-maker '>ha x args))

#|
(>ha (turns 0.4))
(>ha (turns 0.6))                      
|#
;; --------------------------------------------

(defclass >hms (>dms)
  ;; Unipolar HH MM SS.s
  ()
  (:default-initargs
   :ndpl  1
   :fmt   'cl-stk:hms
   ))

#| ;; inherited from >dms
(defmethod um:fdpl-prepval ((x >hms))
  (to turns (unipolar (um:val-of x))))
|#

(defun >hms (x &rest args)
  (apply #'um:fdpl-maker '>hms x args))

#|
(>hms (turns 0.4))                      
(>hms (turns 0.6))                      
|#
;; --------------------------------------------

(defclass >hm (>hms)
  ;; Unipolar HH MM.m
  ()
  (:default-initargs
   :fmt   'cl-stk:hm
   ))

#| ;; inherited from >hms
(defmethod um:fdpl-prepval ((x >hm))
  (to turns (unipolar (um:val-of x))))
|#

(defun >hm (x &rest args)
  (apply #'um:fdpl-maker '>hm x args))

#|
(>hm (turns 0.4))                      
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
.end ) ;; (")
 (values))

|#

