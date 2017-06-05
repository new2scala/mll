package org.ditw.utils.testData

import java.io.FileOutputStream
import java.nio.charset.StandardCharsets
import javax.print.attribute.standard.OutputDeviceAssigned

import org.apache.commons.io.IOUtils

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

/**
  * Created by dev on 2017-06-02.
  */
object PubAffiliationTools extends App {
  import RandGen._
  object GenData {

    // city_postal, country
    //   Madrid 28049, Spain
    //   SE1 1UL London, UK
    //   Hyogo 650-0047, Japan
    //   4058 Basel, Switzerland
    val Template_3 = "%s, %s"
    // city, country
    //   Iksan, Korea
    val Template_4 = "%s, %s"
    // city, postal, country
    //   Melbourne, 3050, Australia
    val Template_5 = "%s, %s, %s"
    // street, city, postal, country
    //   Easter Bush, Midlothian, EH25 9RG, UK
    val Template_6 = "%s, %s, %s, %s"
    // city, state_country
    //   Boston, MA USA
    val Template_7 = "%s, %s"

    val Usa_Gen1 = (params:Seq[String]) => {
      val city = params(0)
      val state = params(1)
      val postal = params(2)
      val country = params(3)
      "%s, %s %s, %s".format(city, state, postal, country)
    }

    val Uk_Gen1 = (params:Seq[String]) => {
      val road = params(0)
      val city = params(1)
      val postal = params(2)
      val country = params(3)
      "%s, %s %s, %s".format(road, city, postal, country)
    }

    val Tag_Department = "DEPT"
    val Tag_ResearchInstitute = "RES_INST"
    val Tag_University = "UNIV"
    val Tag_College = "COLLEGE"
    val Tag_School = "SCHOOL"
    val Tag_Division = "DIVI"
    val Tag_MedCenter = "MED_CENTER"
    val Tag_LAB = "DIVI"
    val Tag_Country = "COUNTRY"
    val Tag_StatePostal = "STATE_POSTAL"
    val Tag_CityPostal = "CITY_POSTAL"
    val Tag_District = "DISTRICT"
    val Tag_City = "CITY"
    val Tag_State = "STATE"
    val Tag_Street = "STREET"
    val Tag_Building = "BUILDING"

    val Templates = IndexedSeq(
      // city, state_postal, country
      //   Berkeley, CA 94720-1762, USA
      //   New York, NY 10065, USA
      (
        Usa_Gen1,
        () => {
          Seq(randCity(), randState(), randUsPostal1(), "USA")
        },
        List(Tag_City, Tag_StatePostal, Tag_Country)
      ),
      (
        Usa_Gen1,
        () => {
          Seq(randCity(), randState(), randUsPostal2(), "USA")
        },
        List(Tag_City, Tag_StatePostal, Tag_Country)
      ),
      // street, city_postal, country,
      //   Oxford Road, Manchester M13 9PT, UK
      //   Easter Bush, Edinburgh EH25 9RG, UK
      //   Pawinskiego 5a, 02-106 Warsaw, Poland
      (
        Uk_Gen1,
        () => {
          Seq(randRoad(), randCity(), randUkPostal(), "UK")
        },
        List(Tag_Street, Tag_CityPostal, Tag_Country)
      )

    )

    // Division of Parasitic Diseases and Malaria, Centers for Disease Control and Prevention, Atlanta, Georgia
    val AffTags_Divi_MedCenter = List(Tag_Division, Tag_MedCenter)
    val AffTags_School_Univ = List(Tag_School, Tag_University)
    val AffTags_Department_College = List(Tag_Department, Tag_College)
    val AffTags_Department_Univ = List(Tag_Department, Tag_University)
    val AffTags_ResearchInstitute = List(Tag_ResearchInstitute)

    val LocTags_City_State = List(Tag_City, Tag_State)
    val LocTags_City_Country = List(Tag_City, Tag_Country)
    val LocTags_City_StatePostal_Country = List(Tag_City, Tag_StatePostal, Tag_Country)
    val LocTags_District_CityPostal_Country = List(Tag_District, Tag_CityPostal, Tag_Country)

    def Gen_School_Univ() = {
      "%s, %s".format(randSchool(), randUniv())
    }
    def Gen_Dept_College() = {
      "%s, %s".format(randDept(), randCollege())
    }
    def Gen_ResearchInstitute() = {
      "%s %s".format(randWords(3, 1), randInstitute())
    }
    def Gen_Dept_Univ() = {
      "%s, %s".format(randDept(), randUniv())
    }
    def Gen_Divi_MedCenter() = {
      "%s, %s".format(randDivi(), randCenter())
    }
    val Gen_Aff_Loc = (gens:Seq[() => String]) => {
      val affGen = gens(0)
      val locGen = gens(1)
      "%s, %s".format(affGen(), locGen())
    }
    def Gen_City_Country() = {
      "%s, %s".format(randCity(), randCountry())
    }
    def Gen_City_StatePostal_Country() = {
      "%s, %s, %s".format(randCity(), randStatePostal(), randCountry())
    }
    def Gen_District_CityPostal_Country() = {
      "%s, %s, %s".format(randWords(3, 1), randCityPostal(), randCountry())
    }
    def Gen_City_State() = {
      "%s, %s".format(randCity(), randStateOrAbbr())
    }


    val FullTemplates = IndexedSeq(
      (
        Gen_Aff_Loc,
        Seq[() => String](
          Gen_Divi_MedCenter, Gen_City_State
        ),
        AffTags_Divi_MedCenter ::: LocTags_City_State
      ),
      (
        Gen_Aff_Loc,
        Seq[() => String](
          Gen_School_Univ, Gen_City_Country
        ),
        AffTags_School_Univ ::: LocTags_City_Country
      ),
      (
        Gen_Aff_Loc,
        Seq[() => String](
          Gen_School_Univ, Gen_City_StatePostal_Country
        ),
        AffTags_School_Univ ::: LocTags_City_StatePostal_Country
      ),
      (
        Gen_Aff_Loc,
        Seq[() => String](
          Gen_Dept_College, Gen_City_StatePostal_Country
        ),
        AffTags_Department_College ::: LocTags_City_StatePostal_Country
      ),
      (
        Gen_Aff_Loc,
        Seq[() => String](
          Gen_Dept_Univ, Gen_City_StatePostal_Country
        ),
        AffTags_Department_Univ ::: LocTags_City_StatePostal_Country
      ),
      (
        Gen_Aff_Loc,
        Seq[() => String](
          Gen_School_Univ, Gen_District_CityPostal_Country
        ),
        AffTags_School_Univ ::: LocTags_District_CityPostal_Country
      ),
      (
        Gen_Aff_Loc,
        Seq[() => String](
          Gen_ResearchInstitute, Gen_District_CityPostal_Country
        ),
        AffTags_ResearchInstitute ::: LocTags_District_CityPostal_Country
      )

    )

    val Separator = "===="

    def genData(count:Int):IndexedSeq[String] = {
      rand.setSeed(0)

      (0 until count).map { i =>
        val ti = rand.nextInt()
        val templIdx = math.abs(ti) % Templates.size
        val (f1, f2, l) = Templates(templIdx)
        val params:Seq[String] = f2()
        f1(params) + Separator + l.mkString(",")
      }
    }

    def genFullData(count:Int):IndexedSeq[String] = {

      (0 until count).map { i =>
        val ti = rand.nextInt()
        val templIdx = math.abs(ti) % FullTemplates.size
        val (f1, f2, l) = FullTemplates(templIdx)
        val gens:Seq[() => String] = f2
        f1(f2) + Separator + l.mkString(",")
      }
    }

    def genDataAndSave(count:Int, of:String):Unit = {
      val d = genFullData(count)

      IOUtils.write(d.mkString("\n"), new FileOutputStream(of), charset)
    }

    private def makeLine(raw:String, tag:String):String = s"$raw NA $tag"
    private def makeCommaLine:String = s", NA O"

    def buildConll(rawText:String, tags:IndexedSeq[String]):String = {
      val affParts = rawText.split(",")
      if (affParts.length != tags.length)
        throw new IllegalArgumentException(s"Text/Tag length mismatch: [$rawText](${affParts.length})/[$tags](${tags.length})")
      (0 until affParts.length).map { partIdx =>
        val p = affParts(partIdx).trim
        val words = p.split("\\s+")
        val r = ListBuffer[String]()
        r += makeLine(words.head, "B-"+tags(partIdx))
        r ++= words.tail.map(makeLine(_, "I-"+tags(partIdx)))
        r.mkString("\n")
      }.mkString(
        "",
        s"\n$makeCommaLine\n",// add comma line
        "\n\n"
      )
    }

    def conv2ConllFormat(l:String):String = {
      val parts = l.split(Separator)
      val affText = parts(0)
      val affTags = parts(1)
      val affTagParts = affTags.split(",").map(_.trim)
      buildConll(affText, affTagParts)
    }

    val TestTag = "-NA-"
    def conv2ConllTestFormat(l:String):String = {
      val tagCount = l.split(",").length
      val tags = new Array[String](tagCount)
      (0 until tagCount).foreach { i => tags(i) = "NA" }
      buildConll(l, tags)
    }

    def convFile(fin:String, fout:String):Unit = {
      val s = Source.fromFile(fin, charset.name()).getLines().map(conv2ConllFormat).mkString
      IOUtils.write(s, new FileOutputStream(fout), charset)
    }

    def convTestFile(fin:String, fout:String):Unit = {
      val s = Source.fromFile(fin, charset.name()).getLines().map(conv2ConllTestFormat).mkString
      IOUtils.write(s, new FileOutputStream(fout), charset)
    }

  }

//  println(GenData.genNumbers(5))
//  println(GenData.genCapLetters(4))
//  println(GenData.genNumCapLetters(3))
//
//  println(GenData.randCountry(4))
//
//  println(GenData.genFullData(20).mkString("\n"))

//  val f1 = "/media/sf_work/aff-data/train-2.txt"
//  GenData.genDataAndSave(20000, f1)
//  val f2 = "/media/sf_work/aff-data/train-2-converted.txt"
//  GenData.convFile(f1, f2)

  val f1 = "/media/sf_work/aff-data/test-2.txt"
  val f2 = "/media/sf_work/aff-data/test-2-converted.txt"

  GenData.convTestFile(f1, f2)

//  rand.setSeed(1000)
//  List(
//    randCenter(),
//    randDept()
//  ).foreach(println)
}
