;; angle-output.lisp
;;
;; DM/RAL  2024/05/20 06:07:25 UTC
;; ----------------------------------

(in-package #:astro.angle)

;; -----------------------------------------------
;; used mainly for output prep

(defun frac (x)
  (mod x 1.0))

(defun unipolar (x)
  ;; Normalize background angle value to Principal Values, (0 360) deg
  (turns (frac (to-turns x))))

(defun bipolar (x)
  ;; Normalize background angle value to Principal Values, (-180 180) deg
  (turns (- (frac (+ (to-turns x) 1/2)) 1/2)))

;; ----------------------------------

(defun fp3 (x)
  ;; Round seconds fraction to 3 decimal places
  (let ((sf  1d-3))
    (* sf (round x sf))))

(defun degs-to-dms (x)
  (multiple-value-bind (d dfrac)
      (truncate x)
    (multiple-value-bind (m mfrac)
        (truncate (* 60. dfrac))
      (let ((s  (fp3 (* 60. mfrac))))
        (when (>= s 60.)
          (decf s 60.)
          (incf m)
          (when (>= m 60.)
            (decf m 60.)
            (incf d)
            ))
        (values d m s)
        ))))

(defun sexi-out (x pref)
  (multiple-value-bind (d m s)
      (degs-to-dms (abs x))
    `(,pref ,(if (minusp x)
                 (- d)
               d)
            ,(if (and (minusp x)
                      (zerop d))
                 (- m)
               m)
            ,(if (and (minusp x)
                      (zerop d)
                      (zerop m))
                 (- s)
               s))
    ))
    
(defun to-dms (x)
  (sexi-out (to-deg x) 'dms))

(defun to-hms (x)
  (sexi-out (to-hrs x) 'hms))

#|
;; E.g.,
(to-dms (asin 0.5)) => (dms 30 00 00.0)
(to-hms (hrs 3.5))  => (hms  3 30 00.0)
 |#

;; ---------------------------------------------

(defun dot-conv-out (x)
  (multiple-value-bind (d m s)
      (degs-to-dms (abs x))
    (* (signum x) (/ (+ s (* 100. (+ m (* 100. d)))) 10_000.))
    ))

(defun to-d.ms (x)
  (dot-conv-out (to-deg x)))

(defun to-h.ms (x)
  (dot-conv-out (to-hrs x)))

#|
;; E.g.,
(to-d.ms (asin 0.5)) => 30.0000
(to-h.ms (hrs 3.5))  =>  3.3000
 |#

#|
(to-d.ms (d.ms -101.32599995))
(to-dms  (d.ms -101.32599995))
|#

