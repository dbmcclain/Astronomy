;; angle-rotations.lisp
;;
;; DM/RAL  2024/05/20 06:08:54 UTC
;; ----------------------------------

(in-package #:com.ral.astro.angle.rotation)

;; ----------------------------------
;;
;; Rotations in 3D are easily expressed using 2D in stages.
;;
;; In 2D there is only one angle, θ, measured counter-clockwise from
;; +X axis.
;;
;; In 3D we envision a RHS coord framem, and use two angles, (θ, φ) to
;; represent vectors on the unit sphere. Angle θ is as for the 2D
;; case, within the XY plane. Angle φ is as for the 2D case in the Z-V 
;; plane, which contains the vector, V, and the Z axis.
;;
;; We normally work on the sky in Lon,Lat coordinates, on the unit
;; sphere.  Longitude is measured as for θ in 2D in the XY plane. But
;; we use Latitude here, measured as an elevation above the XY plane,
;; and so φ=(90-Lat), for φ a co-latitude.
;;
;; Note that while RA and Ecliptic Longitude are measured
;; counter-clockwise, viewed from above the XY plane, Azimuth and HA
;; are mesured positively in the opposite (clockwise) direction.
;;
;; Furthermore, both RA and Ecliptic Longitude have coincident zero
;; points located at the ascending node of the Ecliptic plane with the
;; Equatorial plane. But Azimuth is normally measured from North
;; toward the East. And HA is measured from the South Meridian toward
;; the West.
;;
;; ---------------------------------------
;; 2D Rect/Polar Conversions
;; Angles measured in degrees

(defun rtop (x y)
  (let ((z  (complex x y)))
    (values (abs z)
            (phase z))
    ))

(defun ptor (rho thet)
  (let* ((z (* rho (cis thet))))
    (values (realpart z)
            (imagpart z))))

;; -----------------------------------------
;; 3D Conversions Rect, Polar

(defun to-xyz (th phi)
  ;; th = Lon, RA
  ;; phi = Lat, Dec - i.e., *not* the φ of Spherical Coords.
  ;; (lon lat) -> (x y z) RHS coords
  (multiple-value-bind (r z)
      (ptor 1 phi)
    (multiple-value-bind (x y)
        (ptor r th)
      (list x y z)
      )))

(defun to-thphi (v)
  ;; (x y z) -> (lon lat)
  (destructuring-bind (x y z) v
    (multiple-value-bind (r th)
        (rtop x y)
      (multiple-value-bind (_ phi)
          (rtop r z)
        (declare (ignore _))
        (values th phi)
        ))))

#|
(let ((v (to-xyz (deg 45) (deg 30))))
  (print v)
  (multiple-value-bind (θ φ)
      (to-thphi v)
    (list (to-deg θ) (to-deg φ))
    ))
|#
;; ---------------------------------------------
;; Rotations about the X axis

(defun rotx (v dphi)
  ;; Rot in RHS sense about X axis.
  ;; Positive rotation moves y into z, leaving x unchanged.
  ;; E.g., (rotx 0 1 0 (deg 30)) => 0, 0.866, 0.5
  (destructuring-bind (x y z) v
    (multiple-value-bind (r phi)
        (rtop y z)
      (multiple-value-bind (ye ze)
          (ptor r (+ phi dphi))
        (list x ye ze)
        ))))

(defun rotx-ang (lon lat dphi)
  ;; rot in RHS sense. The same rotation as for ROTX, but expressed
  ;; entirely between Lon,Lat coords in each frame.
  (let ((v (to-xyz lon lat)))
    (to-thphi (rotx v dphi))
    ))

#|
(let ((v (to-xyz (deg 45) (deg 30))))
  (print v)
  (let ((vr (rotx v (deg 30))))
    (print vr)
    (multiple-value-bind (θ φ)
        (to-thphi vr)
      (list (to-deg θ) (to-deg φ))
      )))
|#

;; ----------------------------------------------------
;;
;; Rotations about an arbitrary axis, denoted as (θ_p, φ_p) for the
;; pole of the axis.
;;
;; The component of a vector along the axis will be unchanged under
;; rotation, while the orthogonal component will be rotated up out of
;; the plane defined by the axis and the vector.
;;
;; Let P denote a unit vector along the rotation axis, V the
;; unit vector being rotated.  The we can decompose V as:
;;
;;   V = (P • V) P + ((P ✕ V) ✕ P)
;;
;; The first term is a vector along the rotation axis of length equal
;; to the parallel component of vector V. The second term represents
;; the perpendicular component of V.
;;
;; We can imagine that the axis vector P represents an X axis in some
;; frame. The plane defined by P and V is the XY plane.  Vector (P ✕ V)
;; projects into the frame's Z axis. And that Z component crossed
;; against the rotation (X) axis produces the Y-component of vector V.
;;
;; Rotation about the P (now X) axis will move the Y component into Z:
;;
;;   For V, under rotation by angle ζ about P:
;;   |x| = (P • V)   -> |x|
;;   |y| = |(P ✕ V)| -> |y| Cos ζ
;;   |z| = 0         -> |y| Sin ζ
;;
;; But in computing (P ✕ V) we already have the perpendicular
;; component along the correct Z axis. The second cross product,
;; ((P ✕ V) ✕ P), defines the Y axis and copies its magnitude.
;;
;; The initial vector V is only composed of the X and Y components.
;; But the resulting rotated vector will have the same X, and scaled
;; vectors along Y and Z. So just keep that Z vector, and scale the Y
;; and Z values by Cos ζ and Sin ζ, respectively, to obtain the
;; rotated vector..
;;
;; The vector cross products can be written, for two vectors,
;;
;;    P = (P_i,P_j,P_k) and V = (V_i,V_j,V_k) 
;;
;; as
;;
;;   P ✕ V = ([P,V]_jk, [P,V]_ki, [P,V]_ij)
;;
;; where [P,V]_rs denotes the commutator (P_r V_s - V_r P_s) = Im(P_r-iP_s * V_r+iV_s).
;;
;; ----------------------------------------------

(defun vdot (av bv)
  ;; vector dot product - symmetric
  (let ((sum  0))
    (mapc (lambda (ae be)
            (incf sum (* ae be)))
         av bv)
    sum))

;; ------------------------------------

(defun vcross (av bv)
  ;; vector cross product - anti-symmetric
  #F
  (declare (cons av bv))
  (destructuring-bind (av_i av_j av_k) av
    (destructuring-bind (bv_i bv_j bv_k) bv
      (macrolet ((a (r)
                   (ecase r
                     (i 'av_i)
                     (j 'av_j)
                     (k 'av_k)))
                 (b (s)
                   (ecase s
                     (i 'bv_i)
                     (j 'bv_j)
                     (k 'bv_k)))
                 (|[a,b]| (r s)
                   ;; the commutator
                   `(- (* (a ,r) (b ,s))
                       (* (b ,r) (a ,s)))))
        
        `(,(|[a,b]| j k)
          ,(|[a,b]| k i)
          ,(|[a,b]| i j))
        ))))

;; --------------------------------------

(defun vnorm (v)
  ;; length of vector
  (sqrt (vdot v v)))

(defun vnormalize (v)
  ;; Convert vector v to unit vector
  (let ((sf (vnorm v)))
    (if (zerop sf)
        v
      (vscale (/ sf) v))
    ))

(defun vadd (v &rest args)
  ;; form vector from summed corresponding components
  (apply #'mapcar #'+ v args))

(defun vsub (a b)
  ;; vector subtraction. We'll only need two terms.
  (mapcar #'- a b))

(defun vscale (sf v)
  ;; form vector as scaled components
  (mapcar (um:curry #'* sf) v))

;; ---------------------------------------

(defun rot (θ_v φ_v  θ_a φ_a  dζ)
  ;; Rotate unit vector (θ_v,φ_v) around axis with pole at (θ_a,φ_a) by angle dζ.
  (let* ((v    (to-xyz θ_v φ_v))             ;; unit vector to be rotated
         (a    (to-xyz θ_a φ_a))             ;; unit vector along axis toward pole
         (xv   (vscale (vdot a v) a))        ;; parallel component unchanged by rotation
         (yv   (vsub v xv))                  ;; what's left after subtracting the parallel component
         (zv   (vcross a v))                 ;; = (A ✕ V), perpendicular components
         (cζ   (cis dζ)))
    (to-thphi (vadd xv                       ;; a kind of a hyper-dot-product
                    (vscale (realpart cζ) yv)
                    (vscale (imagpart cζ) zv)))
    ))

#|
(rot 0 0 0 0 (deg 10))
(mapcar #'to-deg
        (multiple-value-list
         (rot (deg 90) 0
              0 0
              (deg 30))))
|#


                   
