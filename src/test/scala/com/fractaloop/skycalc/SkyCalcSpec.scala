package com.fractaloop.skycalc

import com.github.nscala_time.time.Imports._
import org.joda.time.format.ISODateTimeFormat
import org.scalatest.FunSpec

class SkyCalcSpec extends FunSpec {
  val date = new DateTime(2013, 03, 05, 0, 0, DateTimeZone.UTC)
  val lat = 50.5
  val lng = 30.5

  var testTimes = Map(
    'solarNoon -> "2013-03-05T10:10:57Z",
    'nadir -> "2013-03-04T22:10:57Z",
    'sunrise -> "2013-03-05T04:34:57Z",
    'sunset -> "2013-03-05T15:46:56Z",
    'sunriseEnd -> "2013-03-05T04:38:19Z",
    'sunsetStart -> "2013-03-05T15:43:34Z",
    'dawn -> "2013-03-05T04:02:17Z",
    'dusk -> "2013-03-05T16:19:36Z",
    'nauticalDawn -> "2013-03-05T03:24:31Z",
    'nauticalDusk -> "2013-03-05T16:57:22Z",
    'nightEnd -> "2013-03-05T02:46:17Z",
    'night -> "2013-03-05T17:35:36Z",
    'goldenHourEnd -> "2013-03-05T05:19:01Z",
    'goldenHour -> "2013-03-05T15:02:52Z")

  def assertNear(val1: Double, val2: Double, margin: Option[Double] = None) = {
    val m = margin.getOrElse(1E-15)
    assert(Math.abs(val1 - val2) < m, "asserted almost equal: " + val1 + ", " + val2)
  }

  def assertNear(date1: DateTime, date2: DateTime, margin: Duration) = {
    val abs: Long = Math.abs(date1.getMillis - date2.getMillis)
    val millis: Long = margin.getMillis
    assert(abs < millis, "asserted almost equal: " + date1 + ", " + date2)
  }

  describe("getPosition") {
    it("should return an object with correct azimuth and altitude for the given time and location") {
      var sunPos = SkyCalc.getPosition(date, lat, lng)
      assertNear(sunPos('azimuth), 0.6412750628729547)
      assertNear(sunPos('altitude), -0.7000406838781611)
    }
  }

  describe("getTimes") {
    it("should return correct sun phases for the given date and location") {
      val calcTimes = SkyCalc.getTimes(date, lat, lng)
      for {
        testTime <- testTimes
        calcTime <- calcTimes if testTime._1 == calcTime._1
      } yield assertNear(ISODateTimeFormat.dateTimeNoMillis().parseDateTime(testTime._2), calcTime._2, 1.second)
    }
  }

  describe("getMoonPosition") {
    it("should return an object with correct moon position data given time and location") {
      var moonPos = SkyCalc.getMoonPosition(date, lat, lng)

      assertNear(moonPos('azimuth), 2.1631927013459706)
      assertNear(moonPos('altitude), 0.006969727754891917)
      assertNear(moonPos('distance), 364121.3725625622)
    }
  }

  describe("getMoonIllumination") {
    it("should return an object with correct fraction and angle of moon\"s illuminated limb given time") {
      var moonIllum = SkyCalc.getMoonIllumination(date)

      assertNear(moonIllum('fraction), 0.4848068202456373)
      assertNear(moonIllum('angle), 1.6732942678578346)
    }
  }
}
