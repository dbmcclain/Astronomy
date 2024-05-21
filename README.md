# Astronomy
Useful computations with angles on the unit sphere. The bread and butter stuff for everyday ops at the observatory.

Unified angle arithmetic based on canonical measure. User choice, with SET-ANG-MODE, of :RAD, :DEG, :HRS, :TURNS. But with the input/output variety you really shouldn't care. Possibly handy for debugging, and there could be differences of a few bits in the ULP of various results. (down around the 15th or 16th digit.)

---
Convenient angle entry in a variety of measures:
  ```
    deg - 1 Turn = 360 deg, e.g., (deg 90)
    arcmin
    arcsec
    dms
    d.ms - (remember your old HP Calculator?)

    hrs - 1 Turn = 24 hrs, or 15 deg/hr
    mins
    secs
    hms
    h.ms
  
    rad  - 1 Turn = 2π rad
    mrad
    μrad

    turns  - 1 Turn = 1 turn
```
---
View any angle in any measure, e.g., ```(to-μrad (arcsec 1)) => 4.848.```
```
  to-rad
  to-mrad
  to-μrad

  to-deg
  to-arcmin
  to-arcsec
  to-dms
  to-d.ms

  to-hrs
  to-mins
  to-secs
  to-hms
  to-h.ms

  to-turns

  unipolar - convert angle to principal values in (0 360) deg
  bipolar  - convert angle to principal values in (-180 180) deg

```
---
Trig functions redefined to work against any angular measure. ```(sin (deg 30)) => 0.5```
```
  sin
  cos
  tan
  asin
  acos
  atan - optional 2nd arg
  cis
  phase
```
---
MAP-MULT - a convenience macro to deal with multiple return values, applying a function to each on the way out. Many of the Astronomical routines return multiple values.
```
  (map-mult #'to-deg (values 0.25 0.5 0.75 1))
  =>
  90.0  ;; hmm..., we must be in Turns mode
  180.0
  270.0
  360
```
---
Angular rotation of vectors about arbitrary axis - specify vector and rotation axis with angular pole position on the unit sphere. No Euler angle stuff needed. No singularities near poles. No gimbal lock near zenith - but your telescope might not be so forgiving.

General rotations can be computed in a reference frame agnostic manner. Any vector, V, can be decomposed into a component parallel to the rotation axis, P, and a vector perpendicular to the axis. P is a unit vector pointing toward the pole of the rotation axis. 

We use RHS conventions here. So point your right-hand thumb in the direction toward the pole of rotation, and your fingers curl in the direction of positive rotation angles.

Under rotation, the parallel component remains unchanged. The parallel component has magnitude equal to (P•V) (a vector dot-product), and the perpendicular component is ((P✕V)✕P) - using vector cross-products. Rotation of the vector diminishes that perpendicular component, and adds a component in the direction of (P✕V).
```
  rot - vectors are unit vectors specified as pole positions on the unit sphere,
        using longitude and latitude pairs.

    (map-mult #'to-deg (rot (deg 20) (deg 30)  ;; the vector
                            (deg 12) (deg 80)  ;; the axis
                            (deg 10)) )        ;; amount to rotate
    => 
    28.86352390689511
    30.3734050448251

    
```
---
Astronomical angle entry:
```
  RA - a synonym for HMS.

        (to-deg (RA 22 30 15.3)) => 337.56375

  Dec - a synonym for DMS.

        (to-deg (DEC -12 20 32)) => -12.3422

  to-ra - ensures reported RA in 0..24 hrs,

        (to-ra (deg 270)) => (RA 18 0 0.0)

  to-dec - ensures reported Dec in -90..90 deg

        (to-dec (deg -45)) => (DEC -45 0 0.0)
```
---

Your observatory location and time zone should be set in Observatory.lisp. These values are used in several places for default values when, e.g., you haven't specified a location longitude, or a time zone offset.
```
*qth-lon* - longitude (+E, -W)

*qth-lat* - latitude (+N, -S)

*qth-elev* - elevation in meters. Not currently used for anything.

*qth-tz* - Time zone offset, in hours, from UTC. (+E, -W)
```

---
Epoch construction:
```
  *J2000* - for fast reference to the standard epoch = 2451545.0.

  JDN - for specified date & time, Defaults to zero hours and local timezone offset.

         (JDN 2000 01 01 :hh 12 :lcl-ut 0) => 2451545.0  ;; = *J2000*

  current-epoch - JDN for this very instant.

         (current-epoch) => 2460451.4686574075

  date.time - date & time entry analogous to d.ms format.

         (date.time 2024_05_20.12_30) => 2460451.312962959

  d.t - abbrev for date.time
```
---
Mean Siderial Time:
```
  lmst0 - siderial time at Greenwich for given epoch.
          To get the LMST anywhere else, add your longitude to this result.

         (to-hms (unipolar (lmst0 *j2000*))) => (HMS 18 41 50.548)

  lmst  - siderial time at your observatory longitude, now, or for given epoch.
          I.e., what is on the meridian?

         (to-ra (lmst)) => (RA 7 59 36.19) 
```
---
Hour Angles:
```
  ra-to-ha - now, or for any other epoch, at your observatory location.

  ha-to-ra

  parallactic-angle - requires an HA, Dec. Result is negative when pointing East of the Meridian,
                      or positive when West. So if your frame is aligned with the horizon, then East pointing
                      has celestial North tilted toward East azimuths (negative).
                      Very useful for reconstructing events from a session on Az/El telescopes.

      (let ((ra    (ra  12 20))
            (dec   (dec 05 15))
            (epoch (d.t 2024_05_15.01_30))) ;; just the other night
        (to-deg (parallactic-angle (ra-to-ha ra epoch) dec)))
      =>
      56.94430856595515 <-- tilt of Equatorial North in my frames from the Alt/Az telescope



```
---
Accurate Precession between any two epochs - uses intermediate Ecliptic coord frame and obliquity at start/end epochs. No Euler angle matrices needed. The quick version isn't really that much quicker, but it allows you to forego the statement of epochs. Just give it some number of years. As you can see below, the failings aren't that bad.

There was a different quick and dirty version that we used many years ago. It did not invoke Ecliptic coordinate frames, and it simply approximated the rate of change in RA and Dec. Compared to the two routines here, that old method is distinctly inferior. It is so easy to just convert things to Ecliptic coordinates, rotate the whole frame by 50"/yr, then convert back to Equatorial. Again, no Euler angles are needed to do any of this.
```
  precess - Accurate precession for anywhere on the sky. Needs RA, Dec, and starting epoch.
            Target epoch can be stated, but defaults to (current-epoch).

            (map-mult (#'to-ra #'to-dec)
                (precess (ra 9 20) (dec 80 15) *j2000* (d.t 2024_01_01))) ;; at my obs last New Year's
            =>
            (RA 9 23 8.557)
            (DEC 80 8 42.965)

      Defaults to current epoch as target.

  precessN -- for N years, can be used for quick & dirty, assuming J2000 obliquity

              (map-mult (#'to-ra #'to-dec)
                  (precessN (ra 9 20) (dec 80 15) 24))
              =>
              (RA 9 23 11.916)
              (DEC 80 8 50.086)

```
---
Az/El and Equatorial coords: Azimuth measured from North toward East.
```
  azel-to-hadec

  azel-to-radec - now, or for any stated epoch

             ;; What is rising now in the East, with at least 40 deg elevation?
             (map-mult (#'to-ra #'to-dec)
                 (azel-to-radec (deg 90) (deg 40)))
             =>
             (RA 12 35 0.417)
             (DEC 20 4 52.791000000000004)

  hadec-to-azel

  radec-to-azel
```
---
Eventual plans to accumulate addional featurs: Galactic coords, Nutation, Aberration, Refraction, more...
