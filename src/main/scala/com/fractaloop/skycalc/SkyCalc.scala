package com.fractaloop.skycalc

import scala.collection.mutable

import Math._
import com.github.nscala_time.time.Imports._
import org.joda.time.DateTime

object SkyCalc {
  /**
   * Convert degrees to radians
   * @param degrees
   * @return value in radians
   */
  protected def R(degrees: Double) = degrees * PI / 180

  type Julian = Double
  type Angle = Double


  // Simple constants
  val dayMs: Double = 1.day.toStandardDuration.getMillis

  // Julian Constants
  val Julian1970: Julian = 2440588
  val Julian2000: Julian = 2451545

  // perihelion of the Earth
  var Parhelion: Angle = R(102.9372)


  def dateTimeToJulian(date: DateTime): Julian = date.getMillis / dayMs - 0.5 + Julian1970
  def julianToDateTime(j: Julian): DateTime = new DateTime(((j + 0.5 - Julian1970) * dayMs).toLong)
  def toDays(date: DateTime): Julian = dateTimeToJulian(date) - Julian2000

  // general calculations for position

  // obliquity of the Earth
  var e = R(23.4397)

  protected def getRightAscension(l: Double, b: Double) = atan2(sin(l) * cos(e) - tan(b) * sin(e), cos(l))
  protected def getDeclination(l: Double, b: Double) = asin(sin(b) * cos(e) + cos(b) * sin(e) * sin(l))
  protected def getAzimuth(H: Double, phi: Double, dec: Double) = PI + atan2(sin(H), cos(H) * sin(phi) - tan(dec) * cos(phi))
  protected def getAltitude(H: Double, phi: Double, dec: Double) = asin(sin(phi) * sin(dec) + cos(phi) * cos(dec) * cos(H))
  protected def getSiderealTime(d: Double, lw: Double) = R(280.16 + 360.9856235 * d) - lw


  // general sun calculations

  protected def getSolarMeanAnomaly(d: Double) = R(357.5291 + 0.98560028 * d)
  protected def getEquationOfCenter(M: Double) = R(1.9148 * sin(M) + 0.02 * sin(2 * M) + 0.0003 * sin(3 * M))
  protected def getEclipticLongitude(M: Double, C: Double) = M + C + Parhelion + PI
  protected def getSunCoords(d: Double) = {
    val M = getSolarMeanAnomaly(d)
    val C = getEquationOfCenter(M)
    val L = getEclipticLongitude(M, C)

    Map(
      'dec -> getDeclination(L, 0),
      'ra -> getRightAscension(L, 0))
  }

  /**
   * Find the north-based azimuth and altitude of the sun based on a given time
   * and location
   * @param date when the position happened
   * @param lat latitude
   * @param lng longitude
   * @return
   */
  def getPosition(date: DateTime, lat: Double, lng: Double) = {

    val lw  = R(-lng)
    val phi = R(lat)
    val d   = toDays(date)

    val c  = getSunCoords(d)
    val H  = getSiderealTime(d, lw) - c('ra);

    Map(
      'azimuth -> getAzimuth(H, phi, c('dec)),
      'altitude -> getAltitude(H, phi, c('dec)))
  }

  // sun times configuration (angle, morning name, evening name)

  val times = mutable.Buffer(
    (-0.83, 'sunrise,       'sunset      ),
    ( -0.3, 'sunriseEnd,    'sunsetStart ),
    ( -6.0, 'dawn,          'dusk        ),
    (-12.0, 'nauticalDawn,  'nauticalDusk),
    (-18.0, 'nightEnd,      'night       ),
    (  6.0, 'goldenHourEnd, 'goldenHour  ))

  // adds a custom time to the times config
  def addTime(angle: Double, riseName: Symbol, setName: Symbol) = times ++= List((angle, riseName, setName))

  // calculations for sun times
  var J0 = 0.0009;

  def getJulianCycle(d: Double, lw: Double) = round(d - J0 - lw / (2 * PI))
  def getApproxTransit(Ht: Double, lw: Double, n: Double) = J0 + (Ht + lw) / (2 * PI) + n
  def getSolarTransit(ds: Double, M: Double, L: Double): Julian = Julian2000 + ds + 0.0053 * sin(M) - 0.0069 * sin(2 * L)
  def getHourAngle(h: Double, phi: Double, d: Double) = acos((sin(h) - sin(phi) * sin(d)) / (cos(phi) * cos(d)))


  // calculates sun times for a given date and latitude/longitude
  def getTimes(date: DateTime, lat: Double, lng: Double) = {
    val lw  = R(-lng)
    val phi = R(lat)
    val d   = toDays(date)

    val n  = getJulianCycle(d, lw)
    val ds = getApproxTransit(0, lw, n)

    val M = getSolarMeanAnomaly(ds)
    val C = getEquationOfCenter(M)
    val L = getEclipticLongitude(M, C)

    val dec = getDeclination(L, 0)

    val Jnoon = getSolarTransit(ds, M, L)

    // returns set time for the given sun altitude
    def getSetJ(h: Double): Julian = {
      val w = getHourAngle(h, phi, dec)
      val a = getApproxTransit(w, lw, n)

      getSolarTransit(a, M, L)
    }

    val simpleTimes: Map[Symbol, DateTime] = Map(
      'solarNoon -> julianToDateTime(Jnoon),
      'nadir -> julianToDateTime(Jnoon - 0.5))

    (times.map({
      time: (Double, Symbol, Symbol) =>
        val Jset = getSetJ(R(time._1))
        val Jrise = Jnoon - (Jset - Jnoon)

        List(
          time._2 -> julianToDateTime(Jrise),
          time._3 -> julianToDateTime(Jset))
    }).flatten ++ simpleTimes).toMap
  }

  // moon calculations, based on http://aa.quae.nl/en/reken/hemelpositie.html formulas

  // geocentric ecliptic coordinates of the moon
  private def getMoonCoords(d: Double) = {

    val L = R(218.316 + 13.176396 * d)  // ecliptic longitude
    val M = R(134.963 + 13.064993 * d)  // mean anomaly
    val F = R(93.272 + 13.229350 * d)   // mean distance

    val l  = L + R(6.289 * sin(M))      // longitude
    val b  = R(5.128 * sin(F))          // latitude
    val dt = 385001 - 20905 * cos(M)    // distance to the moon in km

    Map(
      'ra -> getRightAscension(l, b),
      'dec -> getDeclination(l, b),
      'dist -> dt)
  }

  def getMoonPosition(date: DateTime, lat: Double, lng: Double) = {

    val lw  = R(-lng)
    val phi = R(lat)
    val d   = toDays(date)

    val c = getMoonCoords(d)
    val H = getSiderealTime(d, lw) - c('ra)
    val h = getAltitude(H, phi, c('dec))

    // altitude correction for refraction
    val h_c = h + R(0.017 / tan(h + R(10.26 / (h + R(5.10)))))

    Map(
      'azimuth -> getAzimuth(H, phi, c('dec)),
      'altitude -> h_c,
      'distance -> c('dist))
  }


  // calculations for illumination parameters of the moon,
  // based on http://idlastro.gsfc.nasa.gov/ftp/pro/astro/mphase.pro formulas and
  // Chapter 48 of "Astronomical Algorithms" 2nd edition by Jean Meeus
  // (Willmann-Bell, Richmond) 1998.

  def getMoonIllumination(date: DateTime) = {
    val d = toDays(date)
    val s = getSunCoords(d)
    val m = getMoonCoords(d)

    // distance from Earth to Sun in km
    val sdist = 149598000

    val phi = acos(sin(s('dec)) * sin(m('dec)) + cos(s('dec)) * cos(m('dec)) * cos(s('ra) - m('ra)))
    val inc = atan2(sdist * sin(phi), m('dist) - sdist * cos(phi))

    Map(
      'fraction -> (1 + cos(inc)) / 2,
      'angle -> atan2(cos(s('dec)) * sin(s('ra) - m('ra)), sin(s('dec)) * cos(m('dec))
        - cos(s('dec)) * sin(m('dec)) * cos(s('ra) - m('ra))))
  }
}
