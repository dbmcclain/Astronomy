;; poly.lisp -- Polynomial Evaluation
;;
;; DM/RAL  2024/05/29 20:54:26 UTC
;; ----------------------------------------------

(in-package #:com.ral.astro.poly)

;; ----------------------------------------

(defun poly-eval (x coffs)
  ;; Horner's rule...
  (reduce (lambda (c acc)
            (+ c (* x acc)))
          coffs
          :from-end t
          :initial-value 0))

