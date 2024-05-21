# Astronomy
Useful computations with angles on the unit sphere. The bread and butter stuff for everyday ops at the observatory.

This code comes from a lifetime accumulation of stuff that I used at professional observatories. I typically had these tools on my programmable calculator for use at the telescope. (remember those?) I decided to reformulate my experience, using Lisp, for myself. It may be useful to others too.

My experience began as a Radio Astronomer observing Carbon Monoxide emission lines, at 115 GHz, originating from within our galaxy. The telescope was a 1m Cassegrain dish atop Pupin Hall, at Columbia University, in NYC. Then I moved blue-ward to the Infrared and the first computer-controlled telescope - the 100-inch WIRO on Jelm Mtn, about 25 miles South of Laramie, WY. From there I moved, even further blue, into the Visible spectrum at the (then) world's largest telescope, which I taught how to point and track. That was the 7m MMT (Multiple Mirror Telescope), a telescope with 6x72-inch confocal primaries, atop Mt. Hopkins, about 50 miles South of Tucson, AZ. Today, that telescope has been repurposed to carry a single 8m primary mirror.

That was a long time ago... And, of course, in those days we used Forth. Lisp is what Forth always wanted to grow up to be... But, although I moved on to bigger things, I still kept returning to Astronomy throughout my lifetime. As today, again.

Today, the advent of small, affordable, robotic telescopes has spawned fresh interest in the amateur community for Astronomy. And today, all you need to do is pull up an interactive star map, click a mouse button, or swipe and stab on your cell-phone, and the telecope slews to the spot and starts making nice pics of the sky. Right?

But I found that many of the consumer-grade programs have irritating shortcomings. Some make it difficult or impossible to type in the RA & Dec coordinates that you want - never mind the lame input formatting required by them. And for planning purposes, the tools available are stilted and (IMHO) insufficient to help you answer some basic questions while you plan your observing runs. That's why I finally sat down and wrote this collection of Observational Astronomy Tools.

These tools, at your fingertips in the Lisp REPL, augment the programs you need to use to run your telescope. The tools will help answer some basic questions you will have as you operate your telecope or plan your observing runs. Maybe, taken together, we can speed along better than the clumsy approaches either currently offers alone.

---
## Canonical Angle Measure
Unified angle arithmetic based on canonical measure. User choice, with **SET-ANG-MODE**. But with the input/output variety available here, you really shouldn't care. Possibly handy for debugging, and there could be differences of a few bits in the ULP of various results. (down around the 15th or 16th digit.)

The code here depends on Double-Precision Floating Point arithmetic. Single-Precision will probably fail in some places. Astronomical Epochs require substantial numeric precision, on the order of 40-bits. Sky angles can be reasonably computed and maintained with only 24-bits of precision -- after all, we ran the world's largest telescope (at the time) using a 24-bit fractional integer math system. For amateur equipment you possibly only need 16 bits?

For context, a precision of 1 arcsec requires 21-bits. For 1 arcmin, 15-bits. So the problem isn't in the angular measure. The problem occurs because the integer portion of a typical Epoch takes, currently, 22 bits. And that sits atop the fractional precision needed for good sky angle computations.

You can easily define additional angle measures. All you need to provide is an input function that takes user input, and which converts those user values to Turns. And you should also define a **TO-_your-unit_** function that converts from Turns to your chosen measure. Then, regardless of the system angle mode, you should see consistent and correct results when using your new angle measure.

**set-ang-mode** _mode-kw => num_ 
- Mode must be one of _:RAD_, _:DEG_, _:HRS_, or _:TURNS_. If mode is changed then you really ought to recompile the whole body of this code to ensure that reader-macros and DEFVARs have correct canonical values. The number reported by **set-ang-mode** is the number of mode canonical angle units per Turn.

---
## Angle Conversions To Canonical Form
Convenient angle entry in a variety of measures. Here, _ang_ represents a real number in internal canonical units.

**deg** _degs => ang_ 
- 360 deg = 1 turn 
- `(to-turns (deg 90)) => 0.25`	

**arcmin** _arcmins => ang_

**arcsec** _arcsecs => ang_

**dms** _ddd &optional mm ss => ang_

**d.ms** _DDD.MMSSsss => ang_  
- (remember your old HP Calculator?)

**hrs** _hrs => ang_ 
- 24 hrs = 1 turn, 1 hrs = 15 deg.

**mins** _mins => ang_

**secs** _secs => ang_

**hms** _hh &optional mm ss => ang_

**h.ms** _HH.MMSSsss => ang_


**rad** _rad => ang_ 
- 2π rad = 1 turn

**mrad** _mrad => ang_

**μrad** _μrad => ang_


**turns** _turns => ang

---
## Angle Conversions from Canonical Form
View any angle in any measure, e.g., `(to-μrad (arcsec 1)) => 4.848.`

**to-deg** _ang => degs_
  
**to-arcmin** _ang => arcmins_
  
**to-arcsec** _ang => arcsecs_
  
**to-dms** _ang => (DMS ddd mm ss.sss)_
  
**to-d.ms** _ang => DDD.MMSSsss_

**to-hrs** _ang => hrs_
  
**to-mins** _ang => mins_
  
**to-secs** _ang => secs_
  
**to-hms** _ang => (HMS hh mm ss.sss)_
  
**to-h.ms** _ang => HH.MMSSsss_

**to-rad** _ang => rads_
  
**to-mrad** _ang => mrads_

**to-μrad** _ang => μrads_

**to-turns** _ang => turns_

**unipolar** _ang => ang_
- Convert angle to principal values in (0 360) deg. The result remains in canonical angle measure. We are simply renormalizing the value to be in the principle domain corresponding to an unsigned range from 0 to 1 Turn. The Riemann surface has a branch cut along the positive Real axis, with angles measured counter-clockwise from the positive Real axis in the complex plane. 
```
;; The following are equivalent:

(unipolar ang)

(turns
   (mod (to-turns ang) 1.0))
```

**bipolar**  _ang => ang_
- Convert angle to principal values in (-180 180) deg. The result remains in canonical angle measure. We are simply renormalizing the value to be in the principle domain corresponding to a signed range from -1/2 to +1/2 Turn. The Riemann surface has a branch cut along the negative Real axis, with angles measured counter-clockwise from the positive Real axis in the complex plane. 
```
;; The following are equivalent:

(bipolar ang)

(turns
  (- (mod (+ (to-turns ang)
             1/2)
          1.0)
     1/2))
```

At the major observatories we always used integer arithmetic representing 24-bit fractions of a Turn (no floating point), to represent all of our angles. We did all the Ephemeris calculations using 24-bit fractional integer arithmetic. We wrote all of our trig functions to accept those kinds of arguments. The telescope's axis drive encoders were 24-bits. Heck, in the early days, FP Arithmetic was either simulated (slow!), or took expensive coprocessor boards that weren't easily available.

The use of integer arithmetic preserves LSB precision while allowing for unlimited wrapping dynanic range. Floating point, on the other hand, would have gradually eroded LSB precision as numbers grow. They don't wrap, they just grow the exponent while keeping a fixed number of mantissa bits. We don't need huge dynamic range in angular measure. We need precision modular arithmetic.

I remember once, in the Aerospace Industry, dealing with an Electrical Engineer who was trying to control a servo system. He insisted on using single-precision floating-point Radian angle measure for his control system. He probably had that mindset because all the trig functions take radian measure, right? :-) The odd thing here is that he was using a Motorola 56K DSP - all integer based arithmetic! He became so confused by the crappy results he was getting. I finally convinced him to just let me take care of things. There is nothing better than integer arithmetic and integer fractions of a Turn. The ancient Greeks had it right!

I did carefully consider my options when I wrote this code. I wrote a 24-bit integer version of this code, but I finally decided that double-precision FP would be okay to use here. Single precision? Definitely not! Simply becuase FP arithmetic seeks to preserve range at the cost of LSB precision. And single precision FP has barely enough bits in the range from 0.0 to 1.0. Anything larger erodes the LSB.

---
## Redefined Trig Functions
Trig functions redefined to work with canonical angular measure. `(sin (deg 30)) => 0.5`

**sin** _ang => num_

**cos** _ang => num_

**tan** _ang => num_

**asin** _num => ang_

**acos** _num => ang_

**atan** _y &optional x => ang_

**cis** _ang => #C(cos sin)_

**phase** _#C(x y) => ang_

---
## MAP-MULT - Mapping Over Multiple Return Values
This is a convenience macro to deal with multiple return values, applying a function to each on the way out. Many of the Astronomical routines return multiple values.

**map-mult** _fn form_
- Applies _fn_ to each of the multiple values returned by execution of _form_, and returning the results as multiple values. 

**map-mult** _(fns...) form_ 
- Applies the first _fn_ to the first of multiple values returned from the execution of _form_, applies the second _fn_ to the second returned value, etc., returning the results as multiple values.

```
  (map-mult #'to-deg (values 0.25 0.5 0.75 1))
  =>
  90.0  ;; hmm..., we must be in Turns mode
  180.0
  270.0
  360
```
---
## Generalized Vector Rotation
Angular rotation of vectors about arbitrary axis - specify vector and rotation axis with angular pole positions on the unit sphere. No Euler angle stuff needed. No singularities near poles. No gimbal lock near zenith - but your telescope might not be so forgiving.

General rotations can be computed in a reference frame agnostic manner. Any vector, _**V**_, can be decomposed into a component parallel to the rotation axis, _**P**_, and a vector perpendicular to the axis. _**P**_ is a unit vector pointing toward the pole of the rotation axis. 

We use RHS conventions here. So point your right-hand thumb in the direction toward the pole of rotation, and your fingers curl in the direction of positive rotation angles.

Under rotation, the parallel component remains unchanged. The parallel component has magnitude _(**P** • **V**)_ (a vector dot-product), and the perpendicular component is _((**P** ✕ **V**) ✕ **P**)_ - using vector cross-products. Rotation of the vector diminishes that perpendicular component, and adds a component in the direction of _(**P** ✕ **V**)_.

Final result is _**V'**_ = (_**P**_ • _**V**_)_**P**_ + ((_**P**_ ✕ _**V**_) ✕ _**P**_) Cos ζ + (_**P**_ ✕ _**V**_) Sin ζ , for rotation angle ζ.

**rot** _vec-lon-ang vec-lat-ang axis-lon-ang axis-lat-ang rot-ang => lon-ang, lat-ang_ 
- Vectors are unit vectors specified as pole positions on the unit sphere, using longitude and latitude pairs.
```
(map-mult #'to-deg (rot (deg 20) (deg 30)  ;; the vector
                        (deg 12) (deg 80)  ;; the axis
                        (deg 10)) )        ;; amount to rotate
=> 
28.86352390689511 ;; new lon
30.3734050448251  ;; new lat
 ```
---
## Astronomical Angle Conversions

**RA** _hh &optional mm ss => ang_
- A synonym for HMS.
- `(to-deg (RA 22 30 15.3)) => 337.56375`

**Dec** _dd &optional hh ss => ang_ 
- A synonym for DMS.
- `(to-deg (DEC -12 20 32)) => -12.3422`

**to-ra** _ang => (RA hh mm ss.sss)_ 
- Ensures reported RA in 0..24 hrs,
- `(to-ra (deg 270)) => (RA 18 0 0.0)`

**to-dec** _ang => (DEC ddd mm ss.sss)_
- Ensures reported Dec in -90..90 deg
- `(to-dec (deg -45)) => (DEC -45 0 0.0)`

---
## Observatory Location Bindings
Your observatory location and time zone should be set in Observatory.lisp. These values are used in several places for default values when, e.g., you haven't specified a location longitude, or a time zone offset.
```
*qth-lon*  -- longitude (+E, -W)

*qth-lat*  -- latitude (+N, -S)

*qth-elev* -- elevation in meters. Not currently used for anything.

*qth-tz*   -- Time zone offset, in hours, from UTC, in the sense of (Local - UTC). (+E, -W)
```

---
## Epoch Construction
```
*J2000* - for fast reference to the standard epoch = 2451545.0.
```
  
**JDN** _yyyy mm dd &key hh mm ss lcl-ut &allow-other-keys => epoch_
- Compute JDN for specified date & time.
- Defaults to zero hours and local timezone offset.
- `(JDN 2000 01 01 :hh 12 :lcl-ut 0) => 2451545.0  ;; = *J2000*`
- While Lisp is fond of using `-` as a spacer in symbol names, we also mean that quite literally here for keyword _:LCL-UT_. IOW, we mean Local minus UT.

**current-epoch** _=> epoch_
- Get JDN for this very instant.
- `(current-epoch) => 2460451.4686574075`

**date.time** _YYYYMMDD.HHMMSS &key lcl-ut => epoch_ 
- Compute JDB for Date & time entry analogous to d.ms format.
- `(date.time 2024_05_20.12_30) => 2460451.312962959`
- [Yes... my Lisp Reader allows '_' anywhere within numbers. Very nice to have.]

**d.t** _YYYYMMDD.HHMMSS &key lcl-ut => epoch_
- Abbrev for date.time

---
## Mean Siderial Time

**lmst0** _epoch => ang_
- Compute siderial time at Greenwich for given epoch.
- To get the LMST anywhere else, add your longitude to this result.
- `(to-hms (unipolar (lmst0 *j2000*))) => (HMS 18 41 50.548)`

**lmst** _&key lon epoch => ang_
- Compute siderial time at your observatory longitude, now, or for given epoch.
- I.e., what is on the meridian?
- `(to-ra (lmst)) => (RA 7 59 36.19)` 

---
## Hour Angles

**ra-to-ha** _RA-ang &key lon epoch => Ha-ang_
- Convert RA to HA.
- Default is for now, and your observatory location.
- The result, shown here as _HA-ang_, is an angle in canonical representation. Same holds, in general, for anything else labeled with suffix _-ang_.

**ha-to-ra** _HA-ang &key lon epoch => RA-ang_
- Compute the RA for a given HA
- Defaults to now, and your observatory location

**parallactic-angle** _HA-ang Dec-ang &key lat => ang_
- Compute the parallactic angle for the stated HA and Dec.
- Result is negative when pointing East of the Meridian, or positive when West.
- So if your frame is aligned with the horizon, then East pointing has celestial North tilted toward East azimuths (negative).
- Very useful for reconstructing events from a session on Az/El telescopes.
```
(let ((ra    (ra  12 20))
      (dec   (dec 05 15))
      (epoch (d.t 2024_05_15.01_30))) ;; just the other night
  (to-deg (parallactic-angle (ra-to-ha ra epoch) dec)))
=>
56.94430856595515 <-- tilt of Equatorial North in my frames from the Alt/Az telescope
```
---
## Accurate Precession
Accurate Precession between any two epochs - uses intermediate Ecliptic coord frame and obliquity at start/end epochs. No Euler angle matrices needed. The quick version isn't really that much quicker, but it allows you to forego the statement of epochs. Just give it some number of years. As you can see below, the failings aren't that bad.

There was a different quick and dirty version that we used many years ago. It did not invoke Ecliptic coordinate frames, and it simply approximated the rate of change in RA and Dec. Compared to the two routines here, that old method is distinctly inferior. It is so easy to just convert things to Ecliptic coordinates, rotate the whole frame by 50"/yr, then convert back to Equatorial. Again, no Euler angles are needed to do any of this.

**precess** _RA-ang Dec-ang from-epoch &optional to-epoch => RA-ang, Dec-ang_
- Accurate precession for anywhere on the sky.
- Target epoch defaults to `(current-epoch)`.

```
(map-mult (#'to-ra #'to-dec)
  (precess (ra 9 20) (dec 80 15) *j2000* (d.t 2024_01_01))) ;; at my obs last New Year's
=>
(RA 9 23 8.557)
(DEC 80 8 42.965)
```

**precessN** _RA-ang Dec-ang NYears => RA-ang, Dec-ang_ 
- Precess for N years.
- Can be used for quick & dirty
- Assumes constant obliquity = J2000 obliquity.
```
(map-mult (#'to-ra #'to-dec)
  (precessN (ra 9 20) (dec 80 15) 24))
=>
(RA 9 23 11.916)
(DEC 80 8 50.086)
```
---
## Az/El and Equatorial Coordinates
Az/El and Equatorial coords, and Airmass: Azimuth measured from North toward East. No singularities at NCP or Zenith. And, by now, you should realize that we eschew Euler angles arithmetic.

**azel-to-hadec** _Az-ang El-ang &key lat = HA-ang, Dec-ang_

**azel-to-radec** _Az-ang El-ang &key lon lat epoch = RA-ang, Dec-ang_
- Compute RA & Dec for stated Az & El
- Default is now, and your observatory location.
```
;; What is rising now in the East, with at least 40 deg elevation?
(map-mult (#'to-ra #'to-dec)
   (azel-to-radec (deg 90) (deg 40)))
=>
(RA 12 35 0.417)
(DEC 20 4 52.791000000000004)
```

**hadec-to-azel** _HA-ang Dec-ang &key lat => Az-ang, El-ang_

**radec-to-azel** _RA-ang Dec-ang &key lon lat epoch => Az-ang, El-ang_
```
;; Is my object above 40 deg elevation?
(map-mult #'to-deg (radec-to-azel (ra 22 15) (dec 12 20)))
=>
158.38675666215835  ;; az
68.78774440308138   ;; el - Yes!
```

**airmass** _El-ang => airmass_ -- (= 1/(Sin El))
```
;; What have I been accepting?
(airmass (deg 40)) => 1.5557238268604126 ;; hmm... is this too high?
```

**hadec-airmass** _HA-ang Dec-ang &key lat => airmass_
- Compute airmass for stated HA, Dec.
- Defaults to observatory location.

**radec-airmass** _RA-ang Dec-ang &key lon lat epoch => airmass_
- Compute airmass for stated RA, Dec.
- Defaults to now, and observatory location.
                  
---
Eventual plans to accumulate addional featurs: Galactic coords, Nutation, Aberration, Refraction, more...
