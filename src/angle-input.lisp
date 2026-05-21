;; angle-input.lisp
;;
;; DM/RAL  2024/05/20 06:05:10 UTC
;; ----------------------------------

(in-package #:com.ral.astro.angle.input)

;; ----------------------------------
;; Degrees, minutes, and seconds
;;   - or -
;; Hours, minutes, and seconds
;; ------------------------------------------
;; DMS & HMS list forms
#|
(defgeneric sexi-in (d &optional m s)
  (:method ((d real) &optional (m 0) (s 0))
   (let ((sgn  (if (some #'minusp (list d m s))
                   -1
                 1)))
     (* sgn
        (/ (+ (abs s) (* 60. (+ (abs m) (* 60. (abs d)))))
           3600.))
     ))
  (:method ((d string) &optional m s)
   ;; intended for "±DD:MM:SS.ss"
   (declare (ignore m s))
   (/ (com.ral.useful-macros.reader-macros::convert-sexigisimal d)
      3600.)))

(defun dms (deg &optional (min 0) (sec 0))
  (deg (sexi-in deg min sec)))

(defun hms (hrs &optional (min 0) (sec 0))
  (hrs (sexi-in hrs min sec)))
|#
#|
;; E.g.,
(to μrad (dms 0 0 1)) => 4.848
(to deg (hms 6 0 0)) => 90
(to μrad (dms "00:00:01")) => 4.848
(to deg (hms "06:00")) => 90
 |#
;; -------------------------------------------
;; D.MS and H.MS forms

#|
(defun dot-conv-in (x)
  (multiple-value-bind (w f)
      (truncate (* 10000. (abs x)))
    (multiple-value-bind (q s)
        (truncate w 100.)
      (multiple-value-bind (d m)
          (truncate q 100.)
        (* (signum x)
           (/ (+ s f (* 60. (+ m (* 60. d))))
              3600.))
        ))))

(defun d.ms (x)
  (deg (dot-conv-in x)))

(defun h.ms (x)
  (hrs (dot-conv-in x)))
|#
#|
;; E.g.,
(to μrad (d.ms 0.0001)) => 4.848
(to deg  (h.ms 6.0000)) => 90.0

(to-dms (dms " -25 55 13"))
(to-dms (dms " -25 55"))
(to-dms (dms " 0 0 -25"))
(to-dec (dms " 0 0 -25"))
(um:split-string "-00 00 25"
                 :delims *ang-delims*)
(dec "-00 00 25")
|#

(defvar *ang-delims*  '(#\space #\tab #\return #\:))

(defun dms (&rest args)
  ;; very flexible input forms
  ;; (dms DDD)             integer DDD
  ;; (dms DDD MM)          integer DDD, MM
  ;; (dms DDD MM SS.sss)   integer DDD, MM, decimal fractional SS.sss
  ;; (dms DDD.dddd 0)      decimal fractional DDD.dddd - use DEG instead
  ;; (dms DDD.MMSSssss)    fractional abbrev repr
  ;; (dms "DD MM SS.ssss") string form input
  ;; (dms "DD:MM:SS.ssss") string form input
  (ac:match args
    ((d m s)
     ;; Either integers (dms DDD MM SS) or decimal fraction (dms DDD.dddd 0 0)
     (let* ((neg (or (minusp d)
                     (and (zerop d)
                          (minusp m))
                     (and (zerop d)
                          (zerop m)
                          (minusp s))
                     ))
            (aas (arcsec (+ (abs s) (* 60. (+ (abs m) (* 60. (abs d))))))))
       (if neg
           (- aas)
         aas)))
    
    ((d m)
     (dms d m 0))

    ((str) / (or (stringp str)
                 (symbolp str))
     (let* ((sgn  1)
            (strs (mapcan (lambda (s)
                            (cond ((equal s "-")
                                   (setf sgn -1)
                                   nil)
                                  ((or (equal s "+")
                                       (equal s ""))
                                   nil)
                                  ((eql #\- (char s 0))
                                   (setf sgn -1)
                                   (list (subseq s 1)))
                                  (t
                                   (list s))
                                  ))
                          (um:split-string (string str)
                                           :delims *ang-delims*))))
       (* sgn
          (apply #'dms
                 (mapcar #'read-from-string strs)))
       ))

    ((d)
     ;; Either (dms DDD) integer form, or (dms DDD.MMSSssss) abbrev form
     (multiple-value-bind (w f)
         (truncate (* 10000. (abs d)))
       (multiple-value-bind (q ss)
           (truncate w 100.)
         (multiple-value-bind (dd mm)
             (truncate q 100.)
           (arcsec (* (signum d)
                      (+ ss f (* 60. (+ mm (* 60. dd))))))
           ))))

    (_
     (error "Invalid DMS syntax: ~S" args))
    ))

(defun hms (&rest args)
  (* 15. (apply #'dms args)))

(defun d.ms (&rest args)
  ;; alternate name for DMS
  (apply #'dms args))

(defun h.ms (&rest args)
  ;; alternate name for HMS
  (apply #'hms args))

