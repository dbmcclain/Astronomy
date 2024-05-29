;; epoch.lisp
;;
;; DM/RAL  2024/05/20 06:14:28 UTC
;; ----------------------------------

(in-package #:com.ral.astro.epoch)

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

(defconstant +J2000+       2_451_545.0)  ;; Standard Epoch for Jan 1, 2000 at 12:00
(defconstant +sec/day+     86400)
(defconstant +days/year+   365.25)
(defconstant +days/cent+   36525)

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
   (hms hh mm ss)
   (- lcl-ut)
   ))
|#
#|
(defun lmst (&key (lon *qth-lon*) (epoch (current-epoch)))
  ;; Local Mean Sidereal Time at longitude long, expressed in degs.
  ;; = RA on Meridian
  (unipolar (+ (lmst0 epoch) lon)))
|#


;; ------------------------------------------------------------------------
;; Julian Day Numbers

(defun ymd (yyyy &optional (mm 1) (dd 1))
  (mvb (mx yx)
       (if (> mm 2.)
           (values mm yyyy)
         (values (+ mm 12.) (1- yyyy)))
    (let* ((a (truncate yx 100.))
           (b (+ (- 2. a) (truncate a 4.)))) ;; for Gregorian dates, for Julian dates use B = 0
      (+ (truncate (* 365.25 (+ yx 4716.)))
         (truncate (* 30.6001 (1+ mx)))
         dd b -1524.5)
      )))
    
(defun jdn (y m d &key (time 0) (lcl-ut *qth-tz*) &allow-other-keys)
  ;; Compute JDN(UTC)
  (+ (ymd y m d)
     (- time lcl-ut)))
         
(defun current-epoch ()
  (multiple-value-bind (ss mm hh dd mo yyyy dow daylight-p ut-lcl)
      (decode-universal-time (get-universal-time))
    (declare (ignore dow))
    (when daylight-p
      (incf hh))
    (jdn yyyy mo dd :time (hms hh mm ss) :lcl-ut (hrs (- ut-lcl)))))

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
                   :time (hms hh mm ss)
                   :lcl-ut lcl-ut)
              )))))))

(defun d.t (x &key (lcl-ut *qth-tz*))
  (date.time x :lcl-ut lcl-ut))

;; -----------------------------------------------

(defun jyrs (dt)
  ;; Julian years for dt days
  (/ dt +days/year+))

(defun jcs (dt)
  ;; Julian centuries for dt days
  (/ dt +days/cent+))

(defun d2k (epoch)
  ;; days since J2000
  (- epoch +j2000+))

(defun y2k (epoch)
  ;; years since J2000
  (jyrs (d2k epoch)))

(defun c2k (epoch)
  ;; centuries since J2000
  (/ (y2k epoch) 100))

