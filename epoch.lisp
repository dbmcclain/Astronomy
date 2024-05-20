;; epoch.lisp
;;
;; DM/RAL  2024/05/20 06:14:28 UTC
;; ----------------------------------

(in-package #:astro)

;; ----------------------------------

#|
-----------------------------
From AAVSO conversion website:
-----------------------------
The Current Universal Time is:
Wed May 15 20:44:31 2024
The Julian date for the current Universal Time is:
2460446.3642
-----------------------------
(jdnx 2024 05 15 :hh 20 :mm 44 :ss 31 :lcl-ut 0)
(jdn 2024 05 15 :hh 20 :mm 44 :ss 31 :lcl-ut 0)
|#

;; NOTE: Divisions are truncating
;; JDN = (1461 (Y + 4800 + (M − 14)/12))/4 +
;;       (367 (M − 2 − 12 × ((M − 14)/12)))/12 −
;;       (3 ((Y + 4900 + (M - 14)/12))/100)/4 +
;;       D − 32075
#|
(defun jdnx (y m d &key (hh 0) (mm 0) (ss 0) (lcl-ut *qth-tz*))
  (+
   (let* ((mx  (truncate (- m 14.) 12.)))
     (+ (truncate (* 1461. (+ y 4800. mx)) 4.)
        (truncate (* 367. (- m 2. (* 12. mx))) 12.)
        (truncate (* -3. (truncate (+ y 4900. mx) 100.)) 4.)
        d
        -32075.))
   -0.5
   (/ (+ ss (* 60. (+ mm (* 60. (- hh lcl-ut))))) 86400.)
   ))
|#
;; ------------------------------------------------------------------------
;; Julian Day Numbers

(defun jdn (y m d &key (hh 0) (mm 0) (ss 0) (lcl-ut *qth-tz*) &allow-other-keys)
  (let* ((ddx (+ d (/ (+ ss (* 60. (+ mm (* 60. (- hh lcl-ut))))) 86400.))))
    (multiple-value-bind (mx yx)
        (if (> m 2.)
            (values m y)
          (values (+ m 12.) (1- y)))
      (let* ((a (truncate yx 100.))
             (b (- 2. a (truncate a -4.)))) ;; for Gregorian dates, for Julian dates use B = 0
        (+ (truncate (* 365.25 (+ yx 4716.)))
           (truncate (* 30.6001 (1+ mx)))
           ddx b -1524.5)
        ))))
         
(defun current-epoch ()
  (multiple-value-bind (ss mm hh dd mo yyyy dow daylight-p ut-lcl)
      (decode-universal-time (get-universal-time))
    (declare (ignore dow))
    (when daylight-p
      (incf hh))
    (jdn yyyy mo dd :hh hh :mm mm :ss ss :lcl-ut (- ut-lcl))))

#|
(current-epoch)
|#

(defun date.time (x &optional (lcl-ut *qth-tz*))
  ;; For convenient entry of date/time. Converts to JDN.
  ;; Date analog of d.ms introducer.
  ;;
  ;;   yyyymodd.hhmmssσσσ...
  ;;
  ;; represents:
  ;;   yyyy - year
  ;;   mo   - month
  ;;   dd   - date
  ;;   hh   - hour
  ;;   mm   - minute
  ;;   ss.σσσ.. - seconds and fractions thereof
  ;;
  (multiple-value-bind (date time)
      (truncate x)
    (multiple-value-bind (yyyy yrem)
        (truncate date 10_000.)
      (multiple-value-bind (mo dd)
          (truncate yrem 100.)

        (multiple-value-bind (hh hfrac)
            (truncate (* time 100.))
          (multiple-value-bind (mm mfrac)
              (truncate (* hfrac 100.))
            (let ((ss (* 100. mfrac)))
              (jdn yyyy mo dd
                   :hh hh
                   :mm mm
                   :ss ss
                   :lcl-ut lcl-ut)
              )))))))

(defun d.t (x &optional (lcl-ut *qth-tz*))
  (date.time x lcl-ut))

