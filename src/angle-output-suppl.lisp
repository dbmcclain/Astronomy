;; angle-output-suppl.lisp
;;
;; DM/RAL  2024/10/15 15:19:46 UTC
;; ----------------------------------

(in-package #:com.ral.astro.angle.output)

;; --------------------------------------------

(defclass >dms (um:fdpl)
  ;; ±DDD:MM:SS.s
  ()
  (:default-initargs
   :n     1
   :flags '(:sign :sep (#\: 2))
   ))

(defun >dms (x &optional (ndpl 1) &rest flags)
  (let ((val (to-arcsec (bipolar x))))
    (multiple-value-bind (q r)
        (um:int-round ndpl val)
      (apply #'make-instance '>dms
             :val   x
             :n     ndpl
             :w     q
             :f     r
             (when flags
               `(:flags ,flags)))
      )))

(defmethod print-object ((x >dms) out-stream)
  (if *print-readably*
      (call-next-method)
    (um:without-abbrev
     (multiple-value-bind (nosep sign dpl comma-char comma-interval sep-char sep-interval)
         (um:fdpl-parse-flags x)
       (declare (ignore comma-char comma-interval))
       (multiple-value-bind (mins ss)
           (truncate (um:fdpl-w x) 60)
         (multiple-value-bind (dd mm)
             (truncate mins 60)
           (let ((frac  (um:fdpl-format-frac x nosep sep-char sep-interval))
                 (fmt   (format nil "~~2,'0~A:~~2,'0D:~~2,'0D~C~~A" sign dpl)))
             (format out-stream fmt dd (abs mm) (abs ss) frac)
             )))
       ))
    ))

;; --------------------------------------------

(defclass >dm (>dms)
  ;; ±DDD:MM.m
  ())

(defun >dm (x &optional (ndpl 1) &rest flags)
  (let ((val (to-arcmin (bipolar x))))
    (multiple-value-bind (q r)
        (um:int-round ndpl val)
      (apply #'make-instance '>dm
             :val   x
             :n     ndpl
             :w     q
             :f     r
             (when flags
               `(:flags ,flags)))
      )))

(defmethod print-object ((x >dm) out-stream)
  (if *print-readably*
      (call-next-method)
    (um:without-abbrev
     (multiple-value-bind (nosep sign dpl comma-char comma-interval sep-char sep-interval)
         (um:fdpl-parse-flags x)
       (declare (ignore comma-char comma-interval))
       (multiple-value-bind (dd mm)
           (truncate (um:fdpl-w x) 60)
         (let ((fmt   (format nil "~~2,'0~A:~~2,'0D~C~~A" sign dpl))
               (frac  (um:fdpl-format-frac x nosep sep-char sep-interval)))
           (format out-stream fmt dd (abs mm) frac)
           ))
       ))
    ))

;; --------------------------------------------

(defclass >hms (>dms)
  ;; Unipolar HH:MM:SS.s
  ()
  (:default-initargs
   :flags '(:sep (#\: 2))
   ))

(defun >hms (x &optional (ndpl 1) &rest flags)
  (let ((val (to-secs (unipolar x))))
    (multiple-value-bind (q r)
        (um:int-round ndpl val)
      (apply #'make-instance '>hms
             :val   x
             :n     ndpl
             :w     q
             :f     r
             (when flags
               `(:flags ,flags)))
      )))

;; --------------------------------------------

(defclass >ha (>dms)
  ;; Bipolar ±HH:MM:SS.s
  ()
  (:default-initargs
   :flags '(:sign :sep (#\: 2))
   ))

(defun >ha (x &optional (ndpl 1) &rest flags)
  (let ((val (to-secs (bipolar x))))
    (multiple-value-bind (q r)
        (um:int-round ndpl val)
      (apply #'make-instance '>hms
             :val   x
             :n     ndpl
             :w     q
             :f     r
             (when flags
               `(:flags ,flags)))
      )))

;; --------------------------------------------

(defclass >hm (>dm)
  ;; Unipolar HH:MM.m
  ()
  (:default-initargs
   :flags '(:sep (#\: 2))
   ))

(defun >hm (x &optional (ndpl 1) &rest flags)
  (let ((val (to-mins (unipolar x))))
    (multiple-value-bind (q r)
        (um:int-round ndpl val)
      (apply #'make-instance '>hm
             :val   x
             :n     ndpl
             :w     q
             :f     r
             (when flags
               `(:flags ,flags)))
      )))


