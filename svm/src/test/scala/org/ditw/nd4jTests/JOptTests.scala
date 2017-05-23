package org.ditw.nd4jTests

import java.util

import com.joptimizer.optimizers.{LPOptimizationRequest, LPPrimalDualMethod}
import org.nd4j.linalg.factory.Nd4j

/**
  * Created by dev on 2017-05-23.
  */
object JOptTests extends App {

  def test1 = {
    val c = Array[Double](-1, -1)
    val G = Array(
      Array[Double]( 4/3.0, -1 ),
      Array[Double]( -1/2.0, 1 ),
      Array[Double]( -2, -1 ),
      Array[Double]( 1/3.0, 1 )
    )
    val h = Array[Double](2, 1/2.0, 2, 1/2.0)

    val lb = Array[Double](0, 0)
    val ub = Array[Double](10, 10)

    val req = new LPOptimizationRequest();

    req.setC(c)
    req.setG(G)
    req.setH(h)
    req.setLb(lb)
    req.setUb(ub)
    req.setDumpProblem(true)

    val opt = new LPPrimalDualMethod()
    opt.setLPOptimizationRequest(req)
    opt.optimize()

    val s = opt.getOptimizationResponse.getSolution
    println(s.mkString(","))
  }

  def test2(c:Array[Double], ub:Array[Double]) = {
    // -x +  y <= 0
    // -x + 0y <= 1
    // 0x -  y <= 0

    // goal: max(x - y)
    // lb:  -1, 0
    //val c = Array[Double](-1, 1)
    val G = Array(
      Array[Double]( -1, 1 ),
      Array[Double]( -1, 0 ),
      Array[Double]( 0, -1 )
    )
    val h = Array[Double](0, 1, 0)

    //val lb = Array[Double](-1, 0)
    //val ub = Array[Double](1, 0)
    //val ub = Array[Double](0.5, 0)

    val req = new LPOptimizationRequest();

    req.setC(c)
    req.setG(G)
    req.setH(h)
    req.setUb(ub)
    //req.setUb(ub)
    req.setDumpProblem(true)

    val opt = new LPPrimalDualMethod()
    opt.setLPOptimizationRequest(req)
    opt.optimize()

    val s = opt.getOptimizationResponse.getSolution
    println(s.mkString(","))
  }

  /*
  test2(Array[Double](-1, 1), Array[Double](1, 0))
  test2(Array[Double](-1, 1), Array[Double](0.5, 0))
  test2(Array[Double](1, -1), Array[Double](1, 1))
  test2(Array[Double](1, -2), Array[Double](1, 1))
  test2(Array[Double](-1, 2), Array[Double](1, 1))
*/

  def test3(points:Array[(Double, Double)]) = {
    // -x +  y <= 0
    // -x + 0y <= 1
    // 0x -  y <= 0

    // goal: max(x - y)
    // lb:  -1, 0
    //val c = Array[Double](-1, 1)
    val G = points.map(p => Array(p._1, p._2))
    //val h = new Array[Double](G.length)
    //util.Arrays.fill(h, 1)
    val h = points.map { p =>
      val d = math.sqrt(p._1*p._1 + p._2*p._2)
      1/d
    }
    //val lb = Array[Double](-1, 0)
    //val ub = Array[Double](1, 0)
    //val ub = Array[Double](0.5, 0)

    val req = new LPOptimizationRequest()

    req.setC(Array[Double](-1, -1))
    req.setG(G)
    req.setH(h)
    //req.setUb(ub)
    //req.setUb(ub)
    req.setDumpProblem(true)

    val opt = new LPPrimalDualMethod()
    opt.setLPOptimizationRequest(req)
    opt.optimize()

    val s = opt.getOptimizationResponse.getSolution
    println(s.mkString(","))
  }

  test3(Array(
    1.0 -> 2.0,
    2.0 -> 1.0,
    1.0 -> 1.0
  ))
}
