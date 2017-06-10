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

    val XmlTag_Page = "page"
    val XmlTag_Revision = "revision"
    val XmlTag_Title = "title"
    val XmlTag_Text = "text"
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
  case class PageRef(pid:String, altText:Option[String] = None)

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
    private val lineContents:List[LineContent] = {
      text.lines.filter(_.trim.nonEmpty).map(LineContent).toList
    }

    def linesWithPageRefs():List[LineContent] = {
      lineContents.filter(_.getPageRefs.nonEmpty)
    }
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
    val ls = p.linesWithPageRefs()
    println(ls.size)
  }

}
