# Astronomy
Useful computations with angles on the unit sphere. The bread and butter stuff for everyday ops at the observatory.

This code comes from a lifetime accumulation of stuff that I used at professional observatories. I typically had these tools on my programmable calculator _(remember those?)_ for use at the telescope. I decided to reformulate my experience, using Lisp, for myself. It may be useful to others too.

My experience began as a Radio Astronomer observing Carbon Monoxide emission lines at 115 GHz (2.6mm wavelength), originating from within our galaxy. The telescope was a 1m Cassegrain dish atop Pupin Hall, at Columbia University, in NYC. Then I moved blue-ward to the Infrared region (10-20 μm wavelength) and the world's first computer-controlled telescope - the 100-inch WIRO on Jelm Mtn, about 25 miles South of Laramie, WY. From there I moved, even further blue-ward, into the Visible spectrum (500 nm wavelength) at the _[then]_ world's largest telescope, which I taught how to point and track. That was the 7m MMT (Multiple Mirror Telescope), a telescope with 6x72-inch confocal primaries, atop Mt. Hopkins, about 60km South of Tucson, AZ. Today, that telescope has been repurposed to carry a single 8m primary mirror. Other notable telescopes that I have used include the KPNO 50-inch atop Kitt Peak, and the 90-inch U.Minn metal-mirror telescope on Mt. Lemmon. So, for much of my life, I have been around big telescopes.

That was now a long time ago... And, of course, in those days we used Forth. Lisp is what Forth always wanted to grow up to be... But, although I moved on to bigger things, I still kept returning to Astronomy throughout my lifetime. As today, again. But now I'm sporting a 50mm telescope (2 inches!), and having a blast with it. It is so small that it lives inside of a big flower pot to guard against the breeze shaking my camera during exposures.

Today, the advent of small, affordable, robotic telescopes has spawned fresh interest in the amateur community for Astronomy. And today, all you need to do is pull up an interactive star map, click a mouse button, or swipe and stab on your cell-phone, and the telecope slews to the spot and starts making nice pics of the sky. Right?

But I find that many of the consumer-grade programs have irritating shortcomings. Some make it difficult or impossible to type in the RA & Dec coordinates that you want - never mind the lame input formatting required by them. And for planning purposes, the tools available are stilted and (IMHO) insufficient to help you answer some basic questions while you plan your observing runs. That's why I finally sat down and wrote this collection of Observational Astronomy Tools.

These tools, at your fingertips in the Lisp REPL, augment the programs you need to use to run your telescope. The tools will help answer some basic questions you will have as you operate your telecope or plan your observing runs. Maybe, taken together, we can speed along better than the clumsy approaches either currently offers alone.

---
## Canonical Angle Measure
Unified angle arithmetic based on canonical measure. _[Not necessarily Radians, nor Degrees!]_ User choice, with **SET-ANG-MODE**. But with the input/output variety available here, you really shouldn't care. Possibly handy for debugging, and there could be differences of a few bits in the ULP of various results. (down around the 15th or 16th digit.)

The code here depends on Double-Precision Floating Point arithmetic. Single-Precision will probably fail in some places. Astronomical Epochs require substantial numeric precision, on the order of 40-bits. Sky angles can be reasonably computed and maintained with only 24-bits of precision -- after all, we ran the world's largest telescope _(at the time)_ using a 24-bit fractional integer math system. For amateur equipment you possibly only need 16 bits?

For context, a precision of 1 arcsec requires 21-bits. For 1 arcmin, 15-bits. So the problem isn't in the angular measure. The problem occurs because the integer portion of a typical Epoch takes, currently, 22 bits. And that sits atop the fractional precision needed for good sky angle computations.

You can easily define additional angle measures. All you need to provide is an input function that takes user supplied values and  converts them to Turns. And you should also define a **TO-_your-unit_** function that converts from Turns to your chosen measure. Then, regardless of the system angle mode, you should see consistent and correct results when using your new angle measure.

**set-ang-mode** _mode-kw => num_ 
- Mode must be one of _:RAD_, _:DEG_, _:HRS_, or _:TURNS_. If mode is changed then you really ought to recompile the whole body of this code to ensure that reader-macros and DEFVARs have correct canonical values. The number reported by **set-ang-mode** is the number of mode canonical angle units per Turn.

---
## Angle Conversions To Canonical Measure
Convenient angle entry in a variety of measures. Here, _ang_ represents a real number in internal canonical units.

**deg** _degs => ang_ 
- 360 deg = 1 turn.
- `(to-turns (deg 90)) => 0.25`	

**arcmin** _arcmins => ang_
- 60 arcmin = 1 deg.

**arcsec** _arcsecs => ang_
- 60 arcsec = 1 arcmin.

**mas** _mas => ang_
- 1000 mas = 1 arcsec.

**dms** _ddd &optional mm ss => ang_
- An easy way to specify an angle in deg, arcmin, arcsec.

**d.ms** _DDD.MMSSsss => ang_  
- (remember your old HP Calculator?)

**hrs** _hrs => ang_ 
- 24 hrs = 1 turn.
- 1 hrs = 15 deg.

**mins** _mins => ang_
- 60 mins = 1 hrs.

**secs** _secs => ang_
- 60 secs = 1 mins.
  
**hms** _hh &optional mm ss => ang_
- An easy way to specify a time as hrs, mins, secs

**h.ms** _HH.MMSSsss => ang_


**rad** _rad => ang_ 
- 2π rad = 1 turn.

**mrad** _mrad => ang_
- 1,000 mrad = 1 rad.
  
**μrad** _μrad => ang_
- 1,000 μrad = 1 mrad.

**turns** _turns => ang_
- 1 turns = a full circle.

---
## Angle Conversions from Canonical Measure
View any angle in any measure, e.g., `(to-μrad (arcsec 1)) => 4.848.`

**to-deg** _ang => deg_
  
**to-arcmin** _ang => arcmin_
  
**to-arcsec** _ang => arcsec_

**to-mas** _ang => mas_
  
**to-dms** _ang => (DMS ddd mm ss.sss)_
  
**to-d.ms** _ang => DDD.MMSSsss_

**to-hrs** _ang => hrs_
  
**to-mins** _ang => mins_
  
**to-secs** _ang => secs_
  
**to-hms** _ang => (HMS hh mm ss.sss)_
  
**to-h.ms** _ang => HH.MMSSsss_

**to-rad** _ang => rad_
  
**to-mrad** _ang => mrad_

**to-μrad** _ang => μrad_

**to-turns** _ang => turns_

**unipolar** _ang => ang_
- Normalize angle to principal values in (0 360) deg. The result remains in canonical angle measure. We are simply renormalizing the value to be in the principle domain corresponding to an unsigned range from 0 to 1 Turn. The Riemann surface has a branch cut along the positive Real axis, with angles measured counter-clockwise from the positive Real axis in the complex plane. 
```
;; The following are equivalent:

(unipolar ang)

(turns
   (mod (to-turns ang) 1.0))
```

**bipolar**  _ang => ang_
- Normalize angle to principal values in (-180 180) deg. The result remains in canonical angle measure. We are simply renormalizing the value to be in the principle domain corresponding to a signed range from -1/2 to +1/2 Turn. The Riemann surface has a branch cut along the negative Real axis, with angles measured counter-clockwise from the positive Real axis in the complex plane. 
```
;; The following are equivalent:

(bipolar ang)

(turns
  (- (mod (+ (to-turns ang)
             1/2)
          1.0)
     1/2))
```

At the major observatories we always used integer arithmetic representing 24-bit fractions of a Turn (no floating point!), to represent all of our angles. We did all the Ephemeris calculations using 24-bit fractional integer arithmetic. We wrote all of our trig functions to accept those kinds of arguments. The telescope's axis drive encoders were 24-bits. Heck, in the early days, FP Arithmetic was either simulated (slow!), or took expensive coprocessor boards that weren't easily available. [But, of course, for data entry and visual readouts, we always presented our angles in human readable form. Just like nobody ever writes or reads their computer programs in binary.]

The use of integer arithmetic preserves LSB precision while allowing for unlimited wrapping dynanic range. Floating point, on the other hand, would have gradually eroded LSB precision as numbers grow. They don't wrap, they just grow the exponent while keeping a fixed number of mantissa bits. We don't need huge dynamic range in angular measure. We need precision modular arithmetic.

I remember once, in the Aerospace Industry, dealing with an Electrical Engineer who was trying to control a servo system. He insisted on using single-precision floating-point Radian angle measure for his control system. He probably had that mindset because all the trig functions take radian measure, right? 😁 The crazy thing here is that he was using a Motorola 56K DSP - all 24-bit integer based arithmetic! So obviously, the DSP had to simulate single-precision floating point, and their _"single-precision"_ had to be offering fewer than 24-bits for mantissa. He became so confused by the crappy results he was getting. I finally convinced him to just let me take care of things. 

There is nothing better than integer arithmetic and integer fractions of a Turn. The ancient Greeks had it right! But the once common knowledge of Ancient Greece has now become endangered rare knowledge among today's practicioners. There are many useful tricks to using integers and integer fractions. These have become all but totally forgotten, except among the few of us remaining grey-beard Astronomers.

I did carefully consider my options when I wrote this code. I wrote a 24-bit integer version of this code, but I finally decided that double-precision FP would be okay to use here. Single precision? Definitely not! Simply becuase FP arithmetic seeks to preserve range at the cost of LSB precision. And single precision FP has barely enough bits in the range from 0.0 to 1.0. Anything larger erodes the LSB.

_[To think about it... are there any practical uses for radian measure? Since π is irrational, you can never get to a full circle angular sweep without overshooting. And what do you use to directly measure radians? So why not stop at the intermediate measure you do use to determine them? 😁]_

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

General rotations can be computed in a reference frame agnostic manner. Any vector, _**V**_, can be decomposed into a component, _**X**_, parallel to the unit vector, _**a**_, representing the rotation axis, and a vector, _**Y**_, perpendicular to the axis. Axis unit vector, _**a**_, points toward the pole of the rotation axis. 

We use RHS conventions here. So point your right-hand thumb in the direction toward the pole of rotation, and your fingers curl in the direction of positive rotation angles.

Under rotation, the parallel component, _**X**_, remains unchanged. That parallel component vector _**X** = (**a** • **V**)**a**_, using a vector dot-product. 

Using a vector cross-product, we can define a vector, _**Z** = (**a**  ✕ **V**)_, that is orthogonal to both the rotation axis and the initial vector. And the perpendicular component vector, _**Y** = (**Z** ✕ **a**)_, orthogonal to both the axis and the _**Z**_ vector, must now be the perpendicular component of vector _**V**_. Together, based on the way we defined them, the vectors _**X**_, _**Y**_, and _**Z**_, form a RHS coordinate frame. Rotation of the vector diminishes its _**Y**_ component and adds a component in the _**Z**_ direction. 

But now notice that _**Y**_ must also simply be what is left after subtracting off its parallel component: _**Y** = **V** - **X**_. The length of _**Y**_ is also equal to the length of _**Z**_ since the axis vector, _**a**_, is a unit vector. So we don't need to waste time computing a second vector cross-product. But we do need the first one giving us _**Z**_.

Final result is _**V'** = **X** + **Y** Cos ζ + **Z** Sin ζ_ , for rotation angle ζ.

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
- A synonym for **HMS**.
- `(to-deg (RA 22 30 15.3)) => 337.56375`

**Dec** _dd &optional hh ss => ang_ 
- A synonym for **DMS**.
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

**\*qth-lon\*** _=> ang_
- Longitude (+E, -W)

**\*qth-lat\*** _=> ang_  
- Latitude (+N, -S)

**\*qth-elev\*** _=> num_
- Elevation in meters.
- Not currently used for anything.

**\*qth-tz\*** _=> num_
- Time zone offset, usually measured in hours, from UTC, in the sense of (Local - UTC). (+E, -W)
- In the binding, if (Local-UTC) = -7 hours, then be sure to state `(HRS -7)`. In this code, time is treated as an angular value too!
---
## Epoch Construction

**\+J2000\+** _=> epoch_
- DEFCONSTANT for fast reference to the standard epoch = JDN 2451545.0.

**YMD** _yyyy &optional mm dd => epoch_
- Compute JDN for 0h UT on specified date
- mm and dd default to 1.
- Date analog of **HMS** and **DMS**.
  
**JDN** _yyyy mm dd &key time lcl-ut &allow-other-keys => epoch_
- Compute JDN for specified date & time.
- Defaults to zero hours and local timezone offset.
- `(JDN 2000 01 01 :time (HMS 12) :lcl-ut 0) => 2451545.0  ;; = +J2000+`
- While Lisp is fond of using '-' as a spacer in symbol names, we also mean that quite literally here for keyword _:LCL-UT_. IOW, we mean Local minus UT, usually stated in hours. So use `:LCL-UT (hrs -7)` to mean Mountain Standard Time zone.
- Like UTC time, JDN is universally the same for all observers, regardless of their local clock time. The time zone info is needed to correct your reported clock time to become UTC time, before computing the JDN.

**current-epoch** _=> epoch_
- Get JDN for this very instant.
- `(current-epoch) => 2460451.4686574075`

**date.time** _YYYYMMDD.HHMMSS &key lcl-ut => epoch_ 
- Compute JDN for date & time entry analogous to **d.ms** format.
- `(date.time 2024_05_20.12_30) => 2460451.312962959`
  - [Yes... my Lisp Reader allows '_' anywhere within numbers. Very nice to have.]

**d.t** _YYYYMMDD.HHMMSS &key lcl-ut => epoch_
- Synonym for **date.time**

---
## Mean Siderial Time

**ERA** _epoch => ang_
- Compute Earth Rotation Angle for epoch.
  
**GMST** _epoch => ang_
- Compute siderial time at Greenwich for given epoch.
- To get the LMST anywhere else, add your longitude to this result.
- `(to-hms (gmst +j2000+)) => (HMS 18 41 50.549)`
- Validated to within a few μsec of USNO for 50 years, beginning epoch 2000-05-24T00:00:00Z, incrementing by 365d.
  - mean ≈ 0.47μs, sigma ≈ 28μs
  - validation used UTC instead of the proper UT1 and TT    
![GMST Validation](https://github.com/dbmcclain/Astronomy/assets/3160577/2b84d301-b82c-4469-abd2-185679211598)


**LMST** _&key lon epoch => ang_
- Compute siderial time.
- Default is at your observatory longitude, and now
- I.e., what is on the meridian?
- `(to-ra (lmst)) => (RA 7 59 36.19)` 

---
## Hour Angles

**ra-to-ha** _RA-ang &key lon epoch => Ha-ang_
- Convert RA to HA.
- Default is for now, and your observatory location.
- The result, shown here as _HA-ang_, is an angle in canonical representation.
  - Same holds, in general, for anything else labeled with suffix _-ang_.

**ha-to-ra** _HA-ang &key lon epoch => RA-ang_
- Compute the RA for a given HA
- Defaults to now, and your observatory location

**parallactic-angle** _HA-ang Dec-ang &key lat => ang_
- Compute the parallactic angle for the stated HA and Dec.
- Default is your observatory latitude.
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
Accurate Precession between any two epochs - uses IAU Long-Term Ecliptic and Equatorial polar precession models. Adopted from the IAU/SOFA C routines in the 2023 release.

---
So, what exactly is "accurate" precession? 

In general, most of us don't care about an accuracy more precise than about 10-60 arcsec on the sky. We don't care at all what it tells us to set for the RA, since that gets multiplied by (Cos δ) on the way to the sky. What we care about is getting the telescope pointed so that the target is roughly centered in the field of view.

In the years 2000-2006, the IAU established a new standard, called GCRS (Geocentric Celestial Reference System), for position reckoning. We saw huge progress in the decades from 1980-2000, where VLBI was able to discern sub-mas errors and inconsistencies with our old way of working against the sky, the old FK4 and FK5 system. The IAU came up with a system that divorced our dependence on Earth, and its rotation, from the way we account for star and galaxy positions.

So consequently they suggest a more rigorous way to perform precession and nutation all at once in every transformation of star positions from their catalogued positions, matching the Equinox of J2000.0. Time is reckoned differently now too, based primarily on TAI (atomic clocks), instead of relying on Earth's rotation period.

The IAU methods of today can transform catalog positions to your current epoch to an astonishing sub-mas precision. (1 mas = 0.001 arcsec). This enables researchers to look for miniscule deviations caused by gravity bending light as it traverses the universe and passes near massive objects.

That's really great! But you probably won't care about that kind of accuracy from your backyard observatory.

The new way to do precession on your star catalog positions is quite unlike the way we once did it. Traditionally we cared about the location of the North Celestial Pole *and* the North Ecliptic Pole. Knowing those two things we could then derive where the 0h of RA was (The First Point of Aires), where the Equatorial plane and Ecliptic plane intersect on the ascending node. And we could know the current obliquity, or tilt of the Earth's Equatorial plane relative to the Ecliptic plane. Hence knowing what those features were when the star was catalogued, and what those features are today, allows us to compute the shifted apparent position of the star in todays coordinate frame.

Today, the IAU just has us transform from one fixed system, forever unchanging, to your local equinox, and there is no need to know anything at all about the Ecliptic system. The two approaches are completely equivalent, but the GCRS to CIRS transform is the modern way, based on a fixed reference frame. In the past we always had to know which Equinox was used to report star position. Now those positions are firmly fixed, except for visible proper motion and parallax. Distant Quasars are so far away that they will never change their positions from our viewpoint. This is the system that our orbiting observatories can utilize - they aren't on the ground and aren't subject to the erratic rotation of the Earth. We on Earth can also use the system.

So, in that regard, I have included 4 different ways to perform stellar precession. One of them is a "back-of-the-envelope" approximation that we had on our programmable calculators. It performed crude precession, but ignored nutation. The other 3 methods offer varying degrees of approximation to a more complete precession + nutation correction. One of the other 3 still utilizes the Ecliptic pole position in relation to the Equatorial pole position. Both poles move slowly over time due to precession and nutation. The other 2 methods ignore the Ecliptic entirely, and work within the new IAU GCRS and CIRS system.

Which method is the quickest to use? Probably the one that we always used in our calculators. Which method is the most precise? Probably the one using both the precessed and nutated Ecliptic and Equatorial pole positions. But even the other two, based on GCRS to CIRS transforms, claim to be sub-arcsec in precision. Who knows? As long as it is better than 1 arcmin, I personally don't have a dog in that fight.

---

**prec** _RA-ang Dec-ang from-epoch &optional to-epoch => RA_ang, Dec_ang_
- Implements the IAU Long-Term Precession models for Ecliptic and Equatorial polar precession.
- to-epoch defaults to now.
- As good as it gets, in this library of code - incorporates the most complete, long-term, precession and nutation.
- Claims to be within 100 arcsec for 200,000 years on either side of J2000.0. (Who would know? if it isn't.)
```
(map-mult (#'to-ra #'to-dec)
  (prec (ra 9 20) (dec 80 15) +j2000+ (d.t 2024_01_01)))
=>
(RA 9 23 12.106)
(DEC 80 8 49.398)
```
---
Okay, in the past, we used a grubby little routine on our calculators to precess our targets for the night's observing. It goes like this:
```
Δα = 3.07496 + 1.33621 Tan(δ)Sin(α)   - secs/yr
Δδ = 20.0431 Cos(α)                   - αrcsec/yr
```
You see the _Tan(δ)_ in there? It means you shouldn't try to use this near the North/South poles. And that tangent can make a huge change in RA. But so what? You are near the pole. How bad on the sky is this really? 

Well, a quick and dirty analysis over the whole sky, precessing for 50 years, either side from J2000, shows at most 2.8 arcmin error on the sky, even if you operate as close as 1 deg from the poles, and even as the maximum change in apparent position due to precession is 43 arcmins. So the grubby algorithm appears to be within 7%. 

The camera in my little 2-inch telescope has 1080 pixel width, spanning 43 arcmin across. Without any corrections for precession, the star could wind up off-frame. But, correcting for precession, our star will still end up pretty darn close to the image center, even with these errors.

The little pic here shows the relative amount of precession correction, measured on the sky. (Black = None, White = Max) Despite the _Tan(δ)_, Max correction is not at the Equatorial poles.

![Precession vs Position](https://github.com/dbmcclain/Astronomy/assets/3160577/30386066-38b3-4384-9014-014c36c22f25)

Precession corrections on the sky, actual displacements seen, as opposed to increments in RA, are greatest in a broad band surrounding the Ecliptic plane. Those dark regions, which show little precession correction, are near the Ecliptic poles.

So here I provide the grubby version too. It allows you to precess approximately, without needing to specify a full Epoch. Just state the number of years.

**precN** _RA-ang Dec-ang nyrs => RA-ang, Dec-ang_
- Does the grubby precession for you.

---
**preca** and **prec-aa** both use the modern IAU GCRS to CIRS transforms. (GCRS = Geocentric Celestial Reference System, CIRS = Celestial Intermediate Reference System). GCRS corresponds to a catalog of star positions reported as J2000.0. CIRS refers to any other epoch, like the one you are using during an observing run.

**preca** _RA-ang Dec-ang &optional from-epoch to-epoch => Ra-ang, Dec-ang
- Does the precession using a cheap approximation to the most rigorous GCRS-CIRS transform. This one comes from the authors who guided the new IAU system we use today.
- Claims better than 1 arcsec accuracy for the next century.
- From-epoch defaults to +J2000+, and to-epoch defaults to `(CURRENT-EPOCH)`

**prec-aa** _RA-ang Dec-ang &optional from-epoch to-epoch => Ra-ang, Dec-ang_
- Does the precession using another cheap approximation, coming out of the Explanatory Supplement to the American Almanac.
- Claims better than 1 arcsec accurcy for the next century.
- From-epoch defaults to +J2000+, and to-epoch defaults to `(CURRENT-EPOCH)`

---
## Az/El and Equatorial Coordinates
Az/El and Equatorial coords, and Airmass: Azimuth measured from North toward East. No singularities at NCP or Zenith. And, by now, you should realize that we eschew Euler angles arithmetic.

**azel-to-hadec** _Az-ang El-ang &key lat = HA-ang, Dec-ang_
- Compute HA & Dec from Az/El.
- Default is your observatory latitude.

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
- Compute Az/El from HA & Dec.
- Default is your observatory latitude.
  
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
