;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10.;  -*- 
;;; Sun Oct 21 18:49:59 1990 by Mark Kantrowitz <mkant@GLINDA.OZ.CS.CMU.EDU>
;;; sunset.lisp

;;; ****************************************************************
;;; Sunset and Sunrise Times ***************************************
;;; ****************************************************************
;;; 
;;; This program calculates the time of sunrise and sunset given the
;;; date, longitude and latitude. 
;;;
;;; It is based upon a pascal program sent to me by Ed Reingold
;;; <reingold@cs.uiuc.edu>. The formula is taken from the
;;; Almanac for Computers 1984, prepared by the Nautical Almanac Office,
;;; United States Naval Observatory, Washington, 1984.
;;;
;;; Portions of lisp code from "Calendrical Calculations" by Nachum
;;; Dershowitz and Edward M. Reingold, Software -- Practice & Experience,
;;; vol. 20, no. 9 (September, 1990), pp. 899-928.
;;;
;;; This code is in the public domain, but any use of it should acknowledge
;;; the source.
;;; 
;;; Written by Mark Kantrowitz, mkant@cs.cmu.edu, October 21, 1990.

;;; ****************************************************************
;;; Documentation **************************************************
;;; ****************************************************************
;;;
;;; WARNING: the calculations will be accurate only to +/- 2 min, locations
;;; should be between 65deg north and 65deg south, for dates in the latter
;;; half of the 20th century!
;;;
;;; does not currently handle precession of the axes and refraction effects
;;; of the atmosphere (the latter affects the time at which the sun crosses
;;; the apparent horizon).
;;;

(defpackage :sunset (:export #:time-of-phenomenon))

(in-package :sunset)

;;; RANDOM NOTES:
;;;
;;; Sidereal time = time required for a body to return to the equivalent
;;;                 position as defined by the stars.
;;;
;;; Correction for refraction, parallax, size of sun =  0.835608 degrees
;;;
;;; Spherical Polar Coordinates: 
;;;    Reference plane through the equator with origin at center of earth
;;;    (a great circle of the celestial sphere).
;;;    A great circle passing through a star and the celestial poles is called
;;;    an hour circle.
;;;
;;; An observer's local meridian is the great circle passing through the
;;; observer's zenith and the celestial poles.  The angle measured westward
;;; from this meridian to the hour circle is called the hour angle (HA) of the
;;; star. The hour angle of the vernal equinox is defined as the local 
;;; sidereal time of the observer; therefore,
;;;       right ascension + hour angle = sidereal time.

;;; Since the vernal equinox and equator are not fixed, because of precession,
;;; one must specify at what date (epoch) the coordinates were measured.


;;; ********************************
;;; Latitude, Longitude, Time-Zone *
;;; ********************************

;;; Time-zone differences are negative, since we are behind greenwich.
(defvar *location-table*
  ;; CITY, LATITUDE, LONGITUDE, TIME-ZONE
  '((("Boston, MA" "MIT") 42.3   -71.1   -5) 
    ("New York, NY"       40.7   -74.0   -5)
    ("San Francisco"      37.8  -122.4   -8)
    ("Los Angeles (LA)"   34.1  -118.3   -8)
    ("Philadelphia"       39.6   -75.1   -5)
    ("Chicago"            41.8   -87.7   -6)
    (("Pittsburgh" "CMU") 40.4   -80.0   -5) 
    ("Providence, RI"     41.8   -71.4   -5)
    ("Washington, DC"     38.8   -77.0   -5)
    ("Seattle, WA"        47.6  -122.3   -8)
    ("Beaverton, OR"      45.5  -122.8   -8)
    ("UIUC"               40.1   -88.2   -6)))

(defvar *latitude* 40.4
  "Latitude in degrees, + north, - south.")
(defvar *longitude* -80
  "Longitude in degrees, + east, - west.")
(defvar *time-zone* -5
  "Number of hours difference between local time and universal time.")

(defun setup-latitude-longitude (city)
  "This function sets the default *latitude* and *longitude* based on that
   of city, if the city name is listed in the location table."
  (let ((entry (assoc city *location-table*
		      :test #'(lambda (city1 city2)
				(if (listp city2)
				    (member city1 city2 :test #'search)
				  (search city1 city2))))))
    (when entry
      (setq *latitude* (second entry)
	    *longitude* (third entry)
	    *time-zone* (fourth entry)))
    (values)))

;;; We're in Pittsburgh, so let's make it the default.
(setup-latitude-longitude "CMU")

;;; ********************************
;;; Trigonometry *******************
;;; ********************************
(defconstant *pi* (if (boundp 'pi) pi 3.141592653589793))

(defun deg-to-rad (deg)
  "Converts degrees to radians, since Common Lisp trig functions use radians."
  (* deg (/ *pi* 180)))

(defun rad-to-deg (rad)
  "Converts radians to degrees."
  (* rad (/ 180 *pi*)))

(defun sin-deg (x)
  (sin (deg-to-rad x)))

(defun cos-deg (x)
  (cos (deg-to-rad x)))

(defun tan-deg (x)
  (tan (deg-to-rad x)))

(defun xy->quadrant (x y)
  "Determines which quadrant the point (x, y) is in."
  (if (plusp x)
      (if (plusp y) 1 4)
      (if (plusp y) 2 3)))

(defun angle->quadrant (angle)
  ;; Force angle to lie between 0 and 360.
  (setq angle (mod angle 360))
  ;; Check quadrant based on angle
  (1+ (truncate angle 90)))

(defun arctan (x quad)
  "Computes the arctangent of a real value. Quad indicates which quadrant
   the value was taken from. Since Lisp's standard function has a range of
   pi/2 to -pi/2, angles in quadrant 2 or 3 will be returned in quadrants
   1 and 4. By adding or subtracting pi, an angle in the correct quadrant
   is returned."
  (let ((temp (rad-to-deg (atan x))))
    (case quad
      (2   (+ temp 180))
      (3   (+ temp 180))
      (4   (+ temp 360))
      (1   temp))))

(defun square (x)
  (* x x))

(defun arccos (x)
  (let ((y (sqrt (- 1 (square x)))))
    (arctan (/ y x)
	    (xy->quadrant x y))))

(defun arcsin (y)
  (let ((x (sqrt (- 1 (square y)))))
    (arctan (/ y x)
	    (xy->quadrant x y))))

;;; ********************************
;;; Constants **********************
;;; ********************************
;;; Inclination of earth equator to orbit (epsilon) = ~23.45 degrees
(defvar *earth-inclination* 23.441884 
  "Inclination of earth's equator to its solar orbit in degrees.")

(defvar *cos-inclination* (cos-deg *earth-inclination*) ; was 0.91746
  "Cosine of earth's inclination.")

(defvar *sin-inclination* (sin-deg *earth-inclination*) ; 0.39782 
  "Sine of earth's inclination.")

;;; Ellipse:
;;;    eccentricity (e) = c/a where c^2 = a^2 - b^2, and
;;;    x = a Cos(theta), y = b Sin(theta) 
;;; For the Earth's orbit, we have:
;;;    a = 149457000 and e = 0.016718
;;;    aphelion = a+x =1.5207x10^8, perhelion = a-x = 1.4707x10^8
;;;    longitude of perhelion = 101.983 degrees.
;;;    r(perhelion) = r(pi) = 0.983298
;;;    r(aphelion) = r(alpha) = 1.016744
(defvar *earth-orbit-eccentricity* 0.016718
  "Eccentricity of orbit of the earth around the sun.")

(defvar *earth-precession* (/ 5025.64 3600.0)
  "Precession (P) of the earth's axes in degrees per tropical century.")

;;; Radius of Earth:
;;;  average 6371.315 km +/- 0.437
;;;  equatorial 6378.533
;;;  polar 6356.912
;;;  ellipticity 0.003393
;;;

;;; ********************************
;;; Primitives *********************
;;; ********************************
(defun deg-to-hour (deg)
  ;; 360 degrees is a full day, 24 hours per day, 
  ;; so there are 15 degrees per hour.
  (/ deg 15))

(defun hour-to-day (hour)
  (/ hour 24))

;;; ********************************
;;; Day Number *********************
;;; ********************************
(defmacro sum (expression index initial condition)
  "Sum $expression$ for $index$ = $initial$ and successive integers,
   as long as $condition$ holds."
  (let* ((temp (gensym)))
    `(do ((,temp 0 (+ ,temp ,expression))
          (,index ,initial (1+ ,index)))
         ((not ,condition) ,temp))))

(defun last-day-of-gregorian-month (month year)
  "Last day in Gregorian $month$ during $year."
  ;; If February is a leap year, return 29,
  ;; otherwise the number of days in the month.
  (if (and (= month 2)
           (zerop (mod year 4))
           (not (member (mod year 400) '(100 200 300))))
      29
    (nth (1- month)
         (list 31 28 31 30 31 30 31 31 30 31 30 31))))

(defun daynumber (month day year)
  (+ day
     (sum                  ;; Days in prior months this year.
      (last-day-of-gregorian-month m year) m 1 (< m month))))

;;; ********************************
;;; Solar Longitude ****************
;;; ********************************
;;; I believe that the 282... figure is what needs to be modified to
;;; take precession into account. Then, instead of just playing with
;;; the day-number, we'll need to have the year as well. I need to
;;; have the value for a fixed date, say 1900, and then add based
;;; on the precession. 

(defun longitude-of-sun (approx)
  "Given an approximate time for the phenomenon, in terms of day of the year,
   returns the longitude of the sun."
  ;; Also known as lambda
  (let ((mean-anomaly
	 ;; mean anomaly of sun
	 (- (* 0.9856 approx) 3.289)))
    ;; longitude of the sun in degrees
    (mod (+ mean-anomaly 
	    (* 1.916 (sin-deg mean-anomaly))
	    (* 0.020 (sin-deg (* 2 mean-anomaly)))
	    282.634)
	 360)))    

;;; This seems to be slightly more accurate. 
;;; Do (defun longitude-of-sun (x) (solar-longitude x)) to use.
(defun solar-longitude (ed)
  ;; Also known as lambda
  (let* ((n (mod (* 360.0 (/ ed 365.2422)) 360.0)) ; degrees
	 (m (deg-to-rad (mod (+ n (- 278.83354 282.596403)) 360.0)))
	 (e m)
	 errt)
    (loop
     (when (<= (setq errt (- e (+ (* *earth-orbit-eccentricity*
				     (sin e))
				  m)))
	       0.0000001)
       (return))
     (decf e (/ errt (- 1 (* *earth-orbit-eccentricity* (cos e))))))
    (mod (+ (rad-to-deg (* 2 (atan (* 1.0168601 (tan (/ e 2))))))
	    282.596403)
	 360)))

;;; ********************************
;;; Declination & Right Ascension **
;;; ********************************
;;; The declination and right ascension are used together to give the
;;; position of a star with reference to the celestial equator and vernal
;;; equinox respectively.
;;;
;;; declination = angular distance between a celestial object and the
;;;               celestial equator, measured along an hour circle, with
;;;               north positive and south negative. (lowercase greek delta)
;;; right ascension = angle measured from the vernal equinox in the west to
;;;               east direction, which is opposite to the apparent rotation
;;;               of the celestial sphere, measured in hours, such that
;;;               360 degrees is equivalent to 24 hours. (lowercase alpha)
;;;                
(defun right-ascension (lambda)
  "Returns the right-ascension (alpha) of the sun, 
   given its longitude (lambda)."
  (deg-to-hour (arctan (* *cos-inclination* (tan-deg lambda))
		       (angle->quadrant lambda))))

(defun declination (lambda)
  "Returns the declination (delta) of the sun, 
   given its longitude (lambda)."
  (arcsin (* *sin-inclination* (sin-deg lambda))))

;;; ********************************
;;; Time of Phenomenon *************
;;; ********************************
(defun time-of-phenomenon (month day year &optional (occurrence :sunset)
				 (latitude *latitude*) (longitude *longitude*)
				 (time-zone *time-zone*))
  "Calculates the time of either sunrise or sunset for the given time and
   location. For daylight savings time, add one hour.
   Occurrence may be either :sunset, :sunrise, or :candle-lighting. 
   (Candle-lighting is 18 minutes before sunset, except in Jerusalem.)"
  (let* ((dayofyear (daynumber month day year)) 
	 (approx 
	  ;; approximate time of phenomenon
	  (case occurrence
	    ((:sunset :candle-lighting)
	     (+ dayofyear (hour-to-day (- 18 (deg-to-hour longitude)))))
	    (:sunrise  
	     (+ dayofyear (hour-to-day (-  6 (deg-to-hour longitude)))))))
	 (longitude-of-sun (longitude-of-sun approx))
	 (right-ascension (right-ascension longitude-of-sun))
	 (declination (declination longitude-of-sun))
	 (coslocaltime (/ (- (cos-deg (+ 90 (/ 50 60)))
			     (* (sin-deg declination) (sin-deg latitude)))
			  (* (cos-deg declination) (cos-deg latitude)))))
    (if (> (abs coslocaltime) 1)
	(format t "~&No sunset that day.")
      (let ((localtime
	     ;; local time of the phenomenon
	     (arccos coslocaltime))
	    local-mean-time		; T: Local Mean time of phenomenon
	    universal-time		; Local time of phenomenon
	    stdtime			; actual time of phenomenon
	    )
	(case occurrence
	  (:sunrise (setq localtime (deg-to-hour (- 360 localtime))))
	  ((:sunset :candle-lighting) (setq localtime (deg-to-hour localtime))))
	(setq local-mean-time
	      (mod (- (+ localtime right-ascension)
		      (+ (* 0.0657098 approx)
			 6.622))
		   24))
	(setq universal-time (- local-mean-time (deg-to-hour longitude))
	      ;; Since the time-zone lines only approximately follow 
	      ;; the 15 degree longitude lines, we can't simply use
	      ;; (floor (deg-to-hour longitude)) for the time-zone. 
	      stdtime (+ universal-time time-zone))
	(when (eq occurrence :candle-lighting)
	  (setq stdtime (- stdtime (/ 18 60))))
	(multiple-value-bind (hour minute) (floor stdtime)
	  (format t "~&~A ~D/~D at ~2,'0D:~2,'0D"
		  occurrence month day hour (round (* 60 minute))))	
	(values)))))

;;; ********************************
;;; Examples ***********************
;;; ********************************
#|
* (setup-latitude-longitude "CMU")

* (time-of-phenomenon 10 19 1990 :candle-lighting)

CANDLE-LIGHTING 10/19 at 17:16
* (time-of-phenomenon 10 19 1990 :sunset)

SUNSET 10/19 at 17:34
* (time-of-phenomenon 10 19 1990 :sunrise)

SUNRISE 10/19 at 06:35
* (setup-latitude-longitude "Boston")

* (time-of-phenomenon 10 19 1990 :candle-lighting)

CANDLE-LIGHTING 10/19 at 16:39
* (setup-latitude-longitude "UIUC")

* (time-of-phenomenon 10 19 1990 :candle-lighting)

CANDLE-LIGHTING 10/19 at 16:50
|#

;;; need to make adjustment for daylight davings time. i.e., add 1 hour then.
