;; radec.lisp
;;
;; DM/RAL  2024/05/20 06:12:27 UTC
;; ----------------------------------

(in-package #:com.ral.astro.radec)

;; ----------------------------------

;; --------------------------------
;; Introducers for RA, Dec

(defvar *ang-delims*  '(#\space #\tab #\return))

(defun radec-string-reader (str ang-fn)
  (db (hh &optional mm ss)
      (um:split-string (string-left-trim *ang-delims* str)
                       :delims *ang-delims*)
    (funcall ang-fn
             (read-from-string hh)
             (if mm
                 (read-from-string mm)
               0)
             (if ss
                 (read-from-string ss)
               0))
    ))

(defgeneric ra (hrs &optional min sec)
  ;; convert RA to canonical angle
  (:method ((hrs integer) &optional (min 0) (sec 0))
   (hms hrs min sec))
  (:method ((hrs real) &optional min sec)
   (h.ms hrs))
  (:method ((str string) &optional min sec)
   (radec-string-reader str #'hms)))

(defgeneric dec (deg &optional min sec)
  ;; convert Dec to degs
  (:method ((deg integer) &optional (min 0) (sec 0))
   (dms deg min sec))
  (:method ((deg real) &optional min sec)
   (d.ms deg))
  (:method ((str string) &optional min sec)
   (radec-string-reader str #'dms)))

#|
(ra 15 59 30.1)
(dec 25 55 13)
|#
;; --------------------------
;; Special formatting for RA and Dec

(defun pic-fmt (s)
  ;; Construct a Lisp format string from a pictured output.
  ;;
  ;; S = text place, eg., SSSS => ~4A
  ;; # = digit place, eg, ###  => ~3D
  ;; 0 = forced zero-fill digit place, must be first in pic, e.g., 0## => ~3,'0D
  ;; - = sign place       -    => ~C
  ;; f = float place, eg., fff.ff => ~6,2F
  ;;
  (let* ((fmt  "")
         (start 0)
         (dpl   nil)
         state)
    (macrolet ((state (new-state)
                 `(setf state #',new-state)))
      (labels ((start (ix c)
                 (setf start ix)
                 (case (char-downcase c)
                   (#\s         (state str))
                   ((#\0 #\#)   (state num))
                   (#\-         (accum "~c"))
                   (#\f         (setf dpl nil)
                                (state flt))
                   (#\Null )
                   (t           (state idle))
                   ))
               (idle (ix c)
                 (case (char-downcase c)
                   ((#\s #\0 #\# #\- #\f)
                    (accum (subseq s start ix))
                    (start ix c))
                   (#\Null
                    (accum (subseq s start ix)))
                   ))
               (str (ix c)
                 (case (char-downcase c)
                   (#\s )
                   (t   (accum (format nil "~~~dA" (- ix start)))
                        (start ix c))
                   ))
               (num (ix c)
                 (case c
                   ((#\0 #\#) )
                   (t
                    (cond ((char= #\0 (char s start))
                           (accum (format nil "~~~d,'0D" (- ix start))))
                          (t
                           (accum (format nil "~~~dD" (- ix start)))))
                    (start ix c))
                   ))
               (flt (ix c)
                 (case c
                   (#\f )
                   (#\.
                    (setf dpl ix))
                   (t
                    (if dpl
                        (accum (format nil "~~~d,~dF" (- ix start) (- ix dpl 1)))
                      (accum (format nil "~~~~dF" (- ix start))))
                    (start ix c))
                   ))
               (accum (s)
                 (setf fmt (concatenate 'string fmt s))))
        (setf state #'start)
        (loop for ix from 0 below (length s) do
                (funcall state ix (char s ix)))
        (funcall state (length s) #\Null)
        fmt
        ))))

#|
(pic-fmt "ssss  ssssss  ssssss  ssssssssss  0# 0# 0#.#  -0# 0# 0#    ff.fff  ff.fff   ff.ff~%")
|#

(defun format-ra (ra)
  (db (_ hh mm ss.s) (to-hms (unipolar ra))
    (declare (ignore _))
    (mvb (ss sfrac)
        (truncate (round (* 10 ss.s)) 10)
      (when (>= ss 60)
        (decf ss 60)
        (incf mm)
        (when (>= mm 60)
          (decf mm 60)
          (incf hh)
          (when (>= hh 24)
            (decf hh 24)
            )))
      (format nil #.(pic-fmt "00 00 00.0")
              hh mm ss sfrac)
      )))

(defun format-dec (dec)
  (let ((decl (bipolar dec)))
    (db (_ dd mm ss) (to-dms (abs decl))
      (declare (ignore _))
      (setf ss (round ss))
      (when (>= ss 60)
        (decf ss 60)
        (incf mm)
        (when (>= mm 60)
          (decf mm 60)
          (incf dd)
          ))
      (format nil #.(pic-fmt "-00 00 00")
              (if (minusp decl)
                  #\-
                #\Space)
              dd mm ss)
      )))
    
;; -------------------------------------------
;; Conversion of angle back to RA, DEC forms

(defun to-ra (x)
  `(RA ,(format-ra x)))

(defun to-dec (x)
  `(DEC ,(format-dec x)))

#|
(to-dec (deg 25.92028))
(to-ra  (deg 239.875417))
|#

(defun to-ra-h.ms (x)
  (to-h.ms (unipolar x)))

(defun to-dec-d.ms (x)
  (to-d.ms (bipolar x)))

