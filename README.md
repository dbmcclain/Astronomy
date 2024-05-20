# Astronomy
Useful computations with angles on the unit sphere. The bread and butter stuff for everyday ops at the observatory.

Unified angle arithmetic based on canonical measure. User choice of :RAD, :DEG, :HRS, :TURNS

Convenient angle entry in a variety of measures:
  ```
  deg - e.g., (deg 90)
  arcmin
  arcsec
  dms
  d.ms - (remember your old HP Calculator?)

  hrs
  mins
  secs
  hms
  h.ms
  
  rad
  mrad
  μrad

  turns - 1 Turn = 360 deg

  unipolar - convert angle to principal values in (0 360) deg
  bipolar  - convert angle to principal values in (-180 180) deg
```

View any angle in any measure, e.g., ```(to-μrad (arcsec 1)) => 4.484.```
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

Angular rotation of vectors about arbitrary axis - specify vector and rotation axis with angular pole position on the unit sphere.

Astronomical angle entry:
```
  RA - e.g., (RA 22 30 15.3)
  Dec
  to-ra - ensures RA in 0..24 hrs
  to-dec - ensures Dec in -90..90 deg
```
Epoch construction:
```
  JDN - for specified date & time
  current-epoch - JDN for this very instant.
  date.time - date & time entry analogous to d.ms format
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
