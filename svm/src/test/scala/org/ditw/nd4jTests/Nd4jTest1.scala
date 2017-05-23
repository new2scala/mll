package org.ditw.nd4jTests

import org.nd4j.linalg.factory.Nd4j

/**
  * Created by dev on 2017-05-23.
  */
object Nd4jTest1 extends App {

  val nd = Nd4j.create(Array[Float](1, 2, 3, 4), Array[Int](2, 2))

  println(nd)

  println(nd.transpose())
  println(nd.reshape(4, 1))
  println(nd.reshape(1, 4))

}
