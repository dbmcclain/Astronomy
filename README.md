# Astronomy
Useful computations with angles on the unit sphere. The bread and butter stuff for everyday ops at the observatory.

Unified angle arithmetic based on canonical measure. User choice, with SET-ANG-MODE, of :RAD, :DEG, :HRS, :TURNS. But with the input/output variety you really shouldn't care. Possibly handy for debugging, and there could be differences of a few bits in the ULP of various results. (down around the 15th or 16th digit.)

Convenient angle entry in a variety of measures:
  ```
    deg - e.g., (deg 90)
    arcmin
    arcsec
    dms
    d.ms - (remember your old HP Calculator?)

    hrs - 1 hour = 15 deg
    mins
    secs
    hms
    h.ms
  
    rad
    mrad
    μrad

    turns - 1 Turn = 360 deg
```

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

Angular rotation of vectors about arbitrary axis - specify vector and rotation axis with angular pole position on the unit sphere. No Euler angle stuff needed. No singularities near poles. No gimbal lock near zenith - but your telescope might not be so forgiving.

Astronomical angle entry:
```
  RA - e.g., (to-deg (RA 22 30 15.3)) => 337.56375
  Dec - e.g., (to-deg (DEC -12 20 32)) => -12.3422
  to-ra - ensures RA in 0..24 hrs, (to-ra (deg 270)) => (RA 18 0 0.0)
  to-dec - ensures Dec in -90..90 deg (to-dec (deg -45)) => (DEC -45 0 0.0)
```
Epoch construction:
```
  *J2000* - for fast reference to the standard epoch.
  JDN - for specified date & time, (JDN 2000 01 01 :hh 12 :lcl-ut 0) => 2451545.0  (= *J2000*)
  current-epoch - JDN for this very instant., (current-epoch) => 2460451.4686574075
  date.time - date & time entry analogous to d.ms format, (date.time 20240520.1230) => 2460451.312962959
  d.t - abbrev for date.time
```

Mean Siderial Time:
```
  lmst0 - siderial time at Greenwich for given epoch.
  lmst  - siderial time at location longitude, now, or for given epoch.
```

Hour Angles:
```
  ra-to-ha - now, or for any other epoch
  ha-to-ra
  parallactic-angle - very useful for reconstucting events from a session on AzEl telescopes.
```

Accurate Precession between any two epochs - uses intermediate Ecliptic coord frame and obliquity at start/end epochs.
```
  precess
  precessN -- for N years, can be used for quick & dirty, assuming J2000 obliquity
```

Az/El and Equatorial coords:
```
  azel-to-hadec
  azel-to-radec - now, or for any stated epoch
  hadec-to-azel
  radec-to-azel
```

Eventual plans to accumulate addional featurs: Galactic coords, Nutation, Aberration, Refraction, more...
