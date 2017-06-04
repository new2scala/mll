package org.ditw.utils.testData

import java.nio.charset.StandardCharsets

import scala.io.Source
import scala.util.Random

/**
  * Created by dev on 2017-06-04.
  */
object RandGen {
  val charset = StandardCharsets.UTF_8

  val Countries = Set(
    "UK", "USA", "Spain", "Japan", "Norway", "Switzerland", "Australia", "Korea",
    "France", "Iran", "China", "Austria", "Poland"
  )

  def loadTexts(f:String):IndexedSeq[String] = Source.fromFile(f, charset.name()).getLines().toIndexedSeq
  def genUSStates(f:String):(IndexedSeq[String], IndexedSeq[String]) = {

    val states = loadTexts(f)
    // each line contains 2 entries, state name and abbr
    states.map { l =>
      val parts = l.split("\\s+")
      parts.slice(0, parts.length-1).mkString(" ") -> parts.last
    }.unzip
  }

  val Cities = loadTexts("/media/sf_work/us_city_names.txt")
  val (states, stateAbbrs) = genUSStates("/media/sf_work/us_states.txt")
  //val States = genUSState("/media/sf_work/us_states.txt")
  val statesAndAbbrs = states ++ stateAbbrs


  val rand = new Random()
  def genSeq(chars:IndexedSeq[Char], length:Int):String = {
    var s = ""
    (0 until length).map { i =>
      val idx = math.abs(rand.nextInt) % chars.length
      s += chars(idx)
    }
    s
  }

  val numbers = (0 to 9).map(_.toString.charAt(0))
  val capital_letters = ('A' to 'Z')
  val lower_letters = ('a' to 'z')
  val num_cap = numbers ++ capital_letters

  def genNumbers(length:Int):String = {
    genSeq(numbers, length)
  }
  def genCapLetters(length:Int):String = {
    genSeq(capital_letters, length)
  }
  def genLowerLetters(length:Int):String = {
    genSeq(lower_letters, length)
  }
  def genNumCapLetters(length:Int):String = {
    genSeq(num_cap, length)
  }
  def randPick(entries:IndexedSeq[String]):String = {
    val idx = math.abs(rand.nextInt) % entries.size
    entries(idx)
  }
  def randCountry(randSeed:Int = 0):String = {
    randPick(Countries.toIndexedSeq)
  }
  def randCity() = randPick(Cities)
  def randStateAbbr() = randPick(stateAbbrs)
  def randStateOrAbbr() = randPick(statesAndAbbrs)
  def randState() = randPick(states)

  def randUsPostal1() = genNumbers(5)
  def randUsPostal2() = {
    genNumbers(5) + '-' + genNumbers(4)
  }
  def randUkPostal() = {
    genNumCapLetters(3) + ' ' + genNumCapLetters(3)
  }

  def randWord() = {
    val c = genCapLetters(1)
    val x = rand.nextInt
    val l = math.abs(x) % 10 + 4
    val ll = genLowerLetters(l)
    c + ll
  }

  def randIndex(length:Int) = {
    val x = rand.nextInt
    math.abs(x) % length
  }

  def randWords(wordCount:Int):String = {
    (0 until wordCount).map(_ => randWord()).mkString(" ")
  }

  def randWords(maxCount:Int, minCount:Int):String = {
    val diff = maxCount-minCount
    val wc = randIndex(diff) + minCount
    randWords(wc)
  }

  val DiviTemplate = "Division of %s"
  def randDivi() = {
    DiviTemplate.format(randWords(4, 1))
  }


  def randRoad() = {
    randWords(5, 2)
  }

  val CenterWords = IndexedSeq("Center", "Centers", "Centre", "Centres")
  val DeptWords = IndexedSeq("Department", "Dept")
  val DivisionWords = IndexedSeq("Division")
  val InstituteWords = IndexedSeq("Institute")
  val UniversityWords = IndexedSeq("University", "Univ")
  val ForOf = IndexedSeq("for", "of")
  val Of = IndexedSeq("of")

  def AXRand(setA:IndexedSeq[String], setX:IndexedSeq[String], maxCount:Int, minCount:Int):String = {
    val idxA = randIndex(setA.size)
    val idxX = randIndex(setX.size)
    "%s %s %s".format(setA(idxA), setX(idxX), randWords(maxCount, minCount))
  }

  def randCenter() = AXRand(CenterWords, ForOf, 4, 2)
  def randDept() = AXRand(DeptWords, Of, 4, 2)
}
