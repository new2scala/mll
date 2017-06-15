package org.ditw.wiki.tests

import org.ditw.wiki.LineProcessingUtils._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by dev on 2017-06-15.
  */
class LineProcessingUtilsTest extends FlatSpec with Matchers with TableDrivenPropertyChecks {

  val delimiter = ("[[", "]]")
  val recStructTestData = Table(
    ("inText", "outStructs"),
    (
      "[[a[[c]]b]]",
      List(
        RecTextStruct(
          "a[[c]]b",
          List(RecTextStruct("c", EmptyRecStructList))
        )
      )
    ),
    (
      "abc[[d]]efg[[h]]i",
      List(
        RecTextStruct("d", EmptyRecStructList),
        RecTextStruct("h", EmptyRecStructList)
      )
    ),
    (
      "123[[abc[[d]]efg[[h]]ijk]]456[[7]]89",
      List(
        RecTextStruct("abc[[d]]efg[[h]]ijk",
          List(
            RecTextStruct("d", EmptyRecStructList),
            RecTextStruct("h", EmptyRecStructList)
          )
        ),
        RecTextStruct("7", EmptyRecStructList)
      )
    ),
    (
      "abc",
      EmptyRecStructList
    )
  )

  "Recursive Struct tests" should "pass" in {
    forAll(recStructTestData) { (inText, outStructs) =>
      val s = findRecStruct(inText, delimiter)
      s shouldBe outStructs
    }
  }

}
