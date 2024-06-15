;; radec.lisp
;;
;; DM/RAL  2024/05/20 06:12:27 UTC
;; ----------------------------------

(in-package #:com.ral.astro.radec)

;; ----------------------------------

;; --------------------------------
;; Introducers for RA, Dec

(defvar *ang-delims*  '(#\space #\tab #\return #\:))

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

