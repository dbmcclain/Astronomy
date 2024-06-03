;; convenience.lisp
;;
;; DM/RAL  2024/05/24 22:51:05 UTC
;; ----------------------------------

(in-package #:com.ral.astro.convenience)

;; ----------------------------------

(defmacro db (names form &body body)
  `(destructuring-bind ,names ,form ,@body))

#+:LISPWORKS
(editor:setup-indent "db" 2)

(defmacro mvb (names form &body body)
  `(multiple-value-bind ,names ,form ,@body))

#+:LISPWORKS
(editor:setup-indent "mvb" 2)

(defmacro mvl (form)
  `(multiple-value-list ,form))

(defmacro map-mult (fn form)
  (if (and (consp fn)
           (not (eql (car fn) 'function)))
      `(values-list (mapcar #'funcall (list ,@fn) (multiple-value-list ,form)))
    `(values-list (mapcar ,fn (multiple-value-list ,form)))))

#+:LISPWORKS
(editor:setup-indent "map-mult" 1)

;; -----------------------------------------------

(defmacro define-astro-package (name &body body)
  ;; Astro-packages all inherit from parent ASTRO package
  ;; Also inherit redefined Trig functions
  `(defpackage ,name
     (:use #:common-lisp #:com.ral.astro)
     (:shadowing-import-from #:com.ral.astro
      #:sin
      #:cos
      #:tan
      #:asin
      #:acos
      #:atan
      #:cis
      #:phase
      )
     ,@body
     ))

;; -----------------------------------------------

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

