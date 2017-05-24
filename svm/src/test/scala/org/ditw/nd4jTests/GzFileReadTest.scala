package org.ditw.nd4jTests

import java.io.{FileOutputStream, InputStream, InputStreamReader, StringReader}
import java.nio.charset.StandardCharsets

import org.apache.commons.io.IOUtils
import org.ditw.utils.TgzUtils

import scala.collection.mutable.ListBuffer

/**
  * Created by dev on 2017-05-23.
  */
object GzFileReadTest extends App {
  def reader(fn:String, is:InputStream, lineCount:Int):String = {
    //var start = 0
    val bufSize = 1024

    val lines = ListBuffer[String]()

      val buf = new Array[Char](bufSize)
      val reader = new InputStreamReader(is, StandardCharsets.UTF_8)
    var currLine = ListBuffer[String]()
      var l = reader.read(buf)
      while (l > 0 && lines.size < lineCount) {
        val r = buf.slice(0, l).mkString
        val nl = r.indexOf('\n')
        if (nl >= 0) {
          currLine += r.substring(0, nl+1)
          lines += currLine.mkString
          currLine = ListBuffer[String]()
          currLine += r.substring(nl+1)
        }
        else {
          currLine += r
        }
        l = reader.read(buf)
      }

      //start = start + l

    lines.mkString
  }

  def reader20(fn:String, is:InputStream) = reader(fn, is, 20)

  //val path = "/media/sf_work/tmp/latest-all.json.bz2"
  val path = "/media/sf_work/tmp/enwiki-latest-pages-articles1.xml-p10p30302.bz2"
  //val s = TgzUtils.processGzFile(path, reader)
  val s = TgzUtils.processBzip2File(path, reader20)
  IOUtils.write(s, new FileOutputStream("/media/sf_work/tmp/articles1.xml.part"), StandardCharsets.UTF_8)
}
