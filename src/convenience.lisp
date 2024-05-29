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