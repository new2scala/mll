package org.ditw.nd4jTests

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by dev on 2017-05-26.
  */
class HyperplaneTests extends FlatSpec with Matchers with TableDrivenPropertyChecks {

//  hyperplane: x1 - x2 + 0 = 0
//  point1: (1, 1) dist: 0
//  point2: (0, 1) dist: 1/(2^1/2)
  private val twoDimW1 = Array[Double](1, -1)
  private val twoDimB11 = 0
  //  hyperplane: x1 - x2 + 1 = 0
  //  point1: (0, 1) dist: 0
  //  point2: (0, 0) dist: 1/(2^1/2)
  private val twoDimB12 = 1

  //  hyperplane: x1 - 2*x2 + 0 = 0
  //  point1: (2, 1) dist: 0
  //  point2: (0, 1) dist: 1/(5^1/2)
  private val twoDimW2 = Array[Double](1, -2)
  private val twoDimB21 = 0

  private val TestTolerance = 1e-6

  def floatEquals(x1:Double, x2:Double):Boolean = math.abs(x1-x2) <= TestTolerance

  def norm2(x:Array[Double]):Double = math.sqrt(x.map(e => e*e).sum)

  val twoDimTestData = Table(
    ("w", "b", "pt", "signedDist"),
    (
      twoDimW2,
      twoDimB21,
      Array[Double](2, 1),
      0.0
    ),
    (
      twoDimW2,
      twoDimB21,
      Array[Double](0, 1),
      -2/math.sqrt(5)
    ),
    (
      twoDimW2,
      twoDimB21,
      Array[Double](1, 0),
      1/math.sqrt(5)
    ),
    (
      twoDimW2,
      twoDimB21,
      Array[Double](2, 0),
      2/math.sqrt(5)
    ),
    (
      twoDimW1,
      twoDimB11,
      Array[Double](1, 1),
      0.0
    ),
    (
      twoDimW1,
      twoDimB11,
      Array[Double](0, 1),
      -1/math.sqrt(2)
    ),
    (
      twoDimW1,
      twoDimB11,
      Array[Double](1, 0),
      1/math.sqrt(2)
    ),
    (
      twoDimW1,
      twoDimB12,
      Array[Double](0, 1),
      0.0
    ),
    (
      twoDimW1,
      twoDimB12,
      Array[Double](0, 0),
      1/math.sqrt(2)
    ),
    (
      twoDimW1,
      twoDimB12,
      Array[Double](1, 0),
      math.sqrt(2)
    )
  )

  "2d point to hyperplane dist tests" should "pass" in {
    forAll(twoDimTestData) { (w, b, pt, signedDist) =>
      val wx_b = (w zip pt).map(p => p._1*p._2).sum + b
      val wl = norm2(w)

      val d = wx_b/wl
      floatEquals(d, signedDist) shouldBe true
    }
  }
}
