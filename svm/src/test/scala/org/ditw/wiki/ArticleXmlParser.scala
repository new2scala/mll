package org.ditw.wiki

import java.io.{FileInputStream, InputStream}

import scala.collection.mutable.ListBuffer

/**
  * Created by dev on 2017-06-09.
  */
object ArticleXmlParser extends App {

  object Consts {
    val RedirectHashtag = "#REDIRECT"
    val PageId_ListOf = List("Lists of", "List of")
    val RedirectPageRefExtractor = """.*\[\[(.*)\]\].*""".r
    val PageRefWithAltTextExtractor = """(.*)\|(.*)""".r

    val PageRefSearchStart = """[["""
    val PageRefSearchEnd = """]]"""
    val PageRefFile = "File:"

    val XmlTag_Page = "page"
    val XmlTag_Revision = "revision"
    val XmlTag_Title = "title"
    val XmlTag_Text = "text"

    val SectionPatterns = IndexedSeq(
      """==(.*)==""".r,
      """===(.*)===""".r,
      """====(.*)====""".r,
      """=====(.*)=====""".r,
      """======(.*)======""".r,
      """=======(.*)=======""".r
    )
  }

  import Consts._

  private def findPageRefs(text:String):IndexedSeq[PageRef] = {
    var startIdx = 0

    val r = ListBuffer[PageRef]()
    while (startIdx < text.length) {
      val si = text.indexOf(PageRefSearchStart, startIdx)
      if (si >= 0) {
        val ei = text.indexOf(PageRefSearchEnd, si)
        if (ei >= 0) {
          val ref = text.substring(si+2, ei)
          ref match {
            case PageRefWithAltTextExtractor(pid, altText) => r += PageRef(pid, Option(altText))
            case _ => r += PageRef(ref)
          }
          startIdx = ei + PageRefSearchEnd.length
        }
        else throw new IllegalArgumentException(s"Cannot find closing ']]' from [$text]")
      }
      else startIdx = text.length
    }
    r.toIndexedSeq
  }
  case class PageRef(pid:String, altText:Option[String] = None) {
    private val isFileRef:Boolean = pid.startsWith(PageRefFile)
  }

  import util.control.Breaks._
  private def parseSectionLine(line:String):Option[(String, Int)] = {
    var r:Option[(String, Int)] = None
    breakable {
      SectionPatterns.indices.reverse.foreach { idx =>
        val m = SectionPatterns(idx).unapplySeq(line)
        if (m.nonEmpty) {
          r = Option(m.get.head, idx+1)
          break
        }
      }
    }
    r
  }

  private def seekTillSectionEnd(linesIt:Iterator[String], currIndent:Int):(Iterator[String], List[String]) = {
    val currLines = ListBuffer[String]()
    val (it1, it2) = linesIt.duplicate
    breakable {
      while (it2.hasNext) {
        val trimmed = it2.next.trim
        val p = parseSectionLine(trimmed)
        if (p.isEmpty) {
          currLines += trimmed
        }
        else {
          val secIndent = p.get._2
          val secTitle = p.get._1

          if (secIndent <= currIndent) {
            break
          }
          else currLines += trimmed
        }
        it1.next
      }
    }
    it1 -> currLines.toList
  }

  private def parseSection(title:String, indent:Int, linesIt:Iterator[String]): (Iterator[String], PageSection) = {
    val (newLinesIt, secLines) = seekTillSectionEnd(linesIt, indent)
    var secLinesIt = secLines.iterator
    val preLines = ListBuffer[String]()
    val postLines = ListBuffer[String]()
    val subSections = ListBuffer[PageSection]()
    breakable {
      while (secLinesIt.hasNext) {
        val trimmed = secLinesIt.next.trim
        val p = parseSectionLine(trimmed)
        if (p.isEmpty) {
          if (subSections.isEmpty) preLines += trimmed
          else postLines += trimmed
        }
        else {
          val subIndent = p.get._2
          val subTitle = p.get._1

          val (newIt, subSection) = parseSection(subTitle, subIndent, secLinesIt)
          subSections += subSection
          secLinesIt = newIt
        }
      }
    }
    newLinesIt -> PageSection(title, indent, subSections.toList, preLines.toList, postLines.toList)
  }

  case class PageSection(title:String, indent:Int, subSections:List[PageSection], preLines:List[String], postLines:List[String]) {

    private val preLineContents:List[LineContent] = {
      preLines.filter(_.trim.nonEmpty).map(LineContent)
    }

    def preLinesWithPageRefs():List[LineContent] = {
      preLineContents.filter(_.getPageRefs.nonEmpty)
    }

    private val postLineContents:List[LineContent] = {
      postLines.filter(_.trim.nonEmpty).map(LineContent)
    }

    def postLinesWithPageRefs():List[LineContent] = {
      postLineContents.filter(_.getPageRefs.nonEmpty)
    }

  }

  case class LineContent(text:String) {
    private val pageRefs:IndexedSeq[PageRef] = findPageRefs(text)
    def getPageRefs = pageRefs
  }

  case class Page(pid:String, text:String) {
    private val isList = PageId_ListOf.exists(lo => pid.startsWith(lo))
    private val redirect:Option[PageRef] = {
      if (text.startsWith(RedirectHashtag)) {
        text match {
          case RedirectPageRefExtractor(pid) => Option(PageRef(pid))
          case _ => None
        }
      }
      else None
    }

    private val sections = parseSection(pid, 0, text.lines)
    def getSections = sections
//    private val lineContents:List[LineContent] = {
//      text.lines.filter(_.trim.nonEmpty).map(LineContent).toList
//    }
//
//    def linesWithPageRefs():List[LineContent] = {
//      lineContents.filter(_.getPageRefs.nonEmpty)
//    }
  }

  import xml.XML

  def readPages(instream:InputStream):Seq[Page] = {
    val doc = XML.load(instream)

    val pages = (doc \ XmlTag_Page).map { p =>
      val title = (p \ XmlTag_Title).text
      val text = (p \ XmlTag_Revision \ XmlTag_Text).text

      Page(title, text)
    }
    pages
  }

  val pages = readPages(new FileInputStream("/media/sf_work/tmp/wiki-articles/articles.part0003.xml"))
  pages.foreach { p =>
    val (_, secs) = p.getSections
    println(secs.title)
  }

}
