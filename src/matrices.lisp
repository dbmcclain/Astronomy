;; Matrices.lisp -- Matrix operations
;; DM/RAL  2024/05/29 20:03:58 UTC
;; ---------------------------------------

(in-package #:com.ral.astro.matrices)

;; --------------------------------------------

(defun trn (m)
  ;; Compute matrix transpose.
  ;; Matrix is a list of 3 element lists representing row vectors.
  ;; For a unitory transform matrix, the transpose is its inverse.
  (apply #'mapcar #'list m))

(defun mat-mulv (m v)
  ;; Multiply a vector by a matrix, M . v
  (mapcar (um:curry #'vdot v) m))

(defun mat-mulm (m1 m2)
  ;; Multiply two matrices, M1 . M2
  (trn (mapcar (um:curry #'mat-mulv m1) (trn m2))))

;; -----------------------------------------------

(defun R1 (x)
  (let* ((cs  (cis x))
         (cx  (realpart cs))
         (sx  (imagpart cs)))
    `((  1    0     0)
      (  0   ,cx   ,sx)
      (  0 ,(- sx) ,cx))
    ))

(defun R2 (x)
  (let* ((cs  (cis x))
         (cx  (realpart cs))
         (sx  (imagpart cs)))
    `((  ,cx    0  ,(- sx))
      (   0     1     0)
      (  ,sx    0   ,cx))
    ))

(defun R3 (x)
  (let* ((cs  (cis x))
         (cx  (realpart cs))
         (sx  (imagpart cs)))
    `((  ,cx    ,sx    0)
      (,(- sx)  ,cx    0)
      (   0       0    1))
    ))

