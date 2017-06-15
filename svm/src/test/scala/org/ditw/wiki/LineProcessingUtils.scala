package org.ditw.wiki

import scala.collection.mutable.ListBuffer

/**
  * Created by dev on 2017-06-15.
  */
object LineProcessingUtils {

  case class RecTextStruct(text:String, subStructs:List[RecTextStruct])

  private def findIndices(line:String, substring:String):List[Int] = {
    var startIdx = 0
    val inds = ListBuffer[Int]()
    while (startIdx < line.length) {
      val idx = line.indexOf(substring, startIdx)
      if (idx >= 0) {
        inds += idx
        startIdx = idx + substring.length
      }
      else startIdx = line.length
    }
    inds.toList
  }

  val EmptyRecStructList = List[RecTextStruct]()

  def findRecStruct(line:String, delimiter:(String,String)):List[RecTextStruct] = {
    val start = delimiter._1
    val end = delimiter._2

    val startIndices = findIndices(line, start)
    val endIndices = findIndices(line, end)
    val idxPairs = (startIndices.map(_ -> 1) ++ endIndices.map(_ -> -1)).sortBy(_._1)

    if (idxPairs.isEmpty) EmptyRecStructList
    else {
      val structs = ListBuffer[RecTextStruct]()
      var acc = 0
      var structStartIdx = idxPairs.head._1
      var paired = true
      idxPairs.foreach { p =>
        val idx = p._1
        acc = acc + p._2
        if (paired) structStartIdx = idx // find pair in the previous index
        if (acc == 0) { // found pair
          val t = line.substring(structStartIdx+start.length, idx)
          structs += RecTextStruct(t, findRecStruct(t, delimiter))
          paired = true
        }
        else paired = false
      }
      structs.toList
    }
  }
}
