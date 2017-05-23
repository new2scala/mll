package org.ditw.nd4jTests

import java.io.{InputStream, InputStreamReader, StringReader}
import java.nio.charset.StandardCharsets

import org.apache.commons.io.IOUtils
import org.ditw.utils.TgzUtils

import scala.collection.mutable.ListBuffer

/**
  * Created by dev on 2017-05-23.
  */
object GzFileReadTest extends App {
  val path = "/media/sf_work/tmp/latest-all.json.bz2"

  def reader(fn:String, is:InputStream):String = {
    //var start = 0
    val bufSize = 1024

    val lines = ListBuffer[String]()

      val buf = new Array[Char](bufSize)
      val reader = new InputStreamReader(is, StandardCharsets.UTF_8)
      var l = reader.read(buf)
      while (l > 0 && lines.size < 100) {
        lines += buf.slice(0, l).mkString
        l = reader.read(buf)
      }
    lines.mkString
      //start = start + l


  }

  //val s = TgzUtils.processGzFile(path, reader)
  val s = TgzUtils.processBzip2File(path, reader)
  println(s)
}
