;; epoch.lisp
;;
;; DM/RAL  2024/05/20 06:14:28 UTC
;; ----------------------------------

(in-package #:astro)

;; ----------------------------------
;;
;; We Earthlings usually reckon with time reported as UTC, available
;; from any number of time services. Atomic Time, TAI = UTC + ΔAT, is
;; as kept by an Atomic Clock. ΔAT is the accumulated number of leap
;; seconds applied to UTC. You have to look it up. It is currently
;; 37s, as of 2024/05/20.
;;
;; And, finally, TT = TAI + 32.184s is "Terrestrial Time", used for
;; all Astronomy calculations. That 32.184s will never change. It is
;; part of the definition of TT arising from a special moment in 1977.
;;
;; So in this body of code, whenever you enter a UTC time for an
;; epoch, we need to first add 37s to get TAI, and then add 32.184s,
;; to reach TT. That is the reason for the two offsets being added in
;; the code below.
;;
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
   (/ (+ ss (* 60. (+ mm (* 60. (- hh (to-hrs lcl-ut)))))) 86400.)
   ))
|#
;; ------------------------------------------------------------------------
;; Julian Day Numbers

(defun jdn (y m d &key (hh 0) (mm 0) (ss 0) (lcl-ut *qth-tz*) &allow-other-keys)
  (let* ((ddx (+ d (/ (+ ss 37 32.184 (* 60. (+ mm (* 60. (- hh (to-hrs lcl-ut)))))) 86400.))))
    ;;                 --------------
    ;;                        |
    ;;                        +-- Computation of TT from UTC
    ;;                            (someday you might have to change the 37 leap secs)
    ;;
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
    (jdn yyyy mo dd :hh hh :mm mm :ss ss :lcl-ut (hrs (- ut-lcl)))))

#|
(current-epoch)
|#

(defun date.time (x &key (lcl-ut *qth-tz*))
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

(defun d.t (x &key (lcl-ut *qth-tz*))
  (date.time x :lcl-ut lcl-ut))

