;; convenience.lisp
;;
;; DM/RAL  2024/05/24 22:51:05 UTC
;; ----------------------------------

(in-package #:astro.convenience)

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

