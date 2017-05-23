package org.ditw.utils

import java.io._
import java.nio.charset.StandardCharsets

import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveInputStream, TarArchiveOutputStream}
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import org.apache.commons.compress.compressors.gzip.{GzipCompressorInputStream, GzipCompressorOutputStream}
import org.apache.commons.io.IOUtils

import scala.collection.mutable.ListBuffer

/**
  * Created by dev on 2017-05-23.
  */
object TgzUtils {
  type FileHandler[T] = (String, InputStream) => T

  def processAllFiles[T](path:String, handler:FileHandler[T]):List[T] = {
    processTgz(path, s => true, handler)
  }

  val FHRead2String:FileHandler[String] = (fn:String, is:InputStream) => {
    IOUtils.toString(is, StandardCharsets.UTF_8)
  }

  def processTgz[T](path:String, nameFilter:String => Boolean, handler:FileHandler[T]):List[T] = {
    val bfFileInputStream = new BufferedInputStream(new FileInputStream(path))

    val tarIn = new TarArchiveInputStream(new GzipCompressorInputStream(bfFileInputStream))
    var tarEntry = tarIn.getNextEntry

    var tarEntryIdx = 0
    val resultList = ListBuffer[T]()
    while (tarEntry != null) {
      val fileOrDir = if (tarEntry.isDirectory) "DIR" else "FILE"
      //println(s"Extracting [${tarEntry.getName}]($fileOrDir)")

      if (!tarEntry.isDirectory) {
        if (nameFilter(tarEntry.getName))
          resultList += handler(tarEntry.getName, tarIn)
        /*
        val bfos = new BufferedOutputStream(new FileOutputStream(f"E:\\VMShare\\tmp\\$tarEntryIdx%04d.json"))
        val bufSize = 4096
        val buf = new Array[Byte](bufSize)
        var cnt = tarIn.read(buf, 0, bufSize)
        while (cnt != -1) {
          bfos.write(buf, 0, cnt)
          cnt = tarIn.read(buf, 0, bufSize)
        }
        bfos.close()
        */
      }
      tarEntry = tarIn.getNextEntry
      tarEntryIdx = tarEntryIdx + 1
    }

    tarIn.close()
    resultList.toList
  }

  type FileNameContentHandler[T] = (String, InputStream) => T

  def processFilesWithNames[T](path:String, nameFilter:String => Boolean, handler:FileNameContentHandler[T]):List[T] = {
    val bfFileInputStream = new BufferedInputStream(new FileInputStream(path))

    val tarIn = new TarArchiveInputStream(new GzipCompressorInputStream(bfFileInputStream))
    var tarEntry = tarIn.getNextEntry

    var tarEntryIdx = 0
    val resultList = ListBuffer[T]()
    while (tarEntry != null) {
      val fileOrDir = if (tarEntry.isDirectory) "DIR" else "FILE"
      //println(s"Extracting [${tarEntry.getName}]($fileOrDir)")

      if (!tarEntry.isDirectory) {
        if (nameFilter(tarEntry.getName))
          resultList += handler(tarEntry.getName, tarIn)
        /*
        val bfos = new BufferedOutputStream(new FileOutputStream(f"E:\\VMShare\\tmp\\$tarEntryIdx%04d.json"))
        val bufSize = 4096
        val buf = new Array[Byte](bufSize)
        var cnt = tarIn.read(buf, 0, bufSize)
        while (cnt != -1) {
          bfos.write(buf, 0, cnt)
          cnt = tarIn.read(buf, 0, bufSize)
        }
        bfos.close()
        */
      }
      tarEntry = tarIn.getNextEntry
      tarEntryIdx = tarEntryIdx + 1
    }

    tarIn.close()
    resultList.toList
  }


  def processGzFile[T](path:String, handler:FileHandler[T]):T = {
    val f = new File(path)
    val bfFileInputStream = new BufferedInputStream(new FileInputStream(path))
    val gzIn = new GzipCompressorInputStream(bfFileInputStream)
    val result = handler(f.getName, gzIn)
    gzIn.close()
    bfFileInputStream.close()
    result
  }

  def processBzip2File[T](path:String, handler:FileHandler[T]):T = {
    val f = new File(path)
    val bfFileInputStream = new BufferedInputStream(new FileInputStream(path))
    val gzIn = new BZip2CompressorInputStream(bfFileInputStream)
    val result = handler(f.getName, gzIn)
    gzIn.close()
    bfFileInputStream.close()
    result
  }

  def add2ExistingTgz(path:String, suffix:String, dstFile:String):Int = {
    val tarInFile = new File(dstFile)
    if (!tarInFile.exists) throw new IllegalArgumentException(s"Input Tgz file [$dstFile] not found!")

    val bstr = new ByteArrayOutputStream()
    val bfStream = new BufferedOutputStream(bstr)
    val tgzOut = new TarArchiveOutputStream(new GzipCompressorOutputStream(bfStream))

    val existingNames:List[String] = processFilesWithNames(
      dstFile,
      (str:String) => true, // accept all the existing entries
      (fn:String, is:InputStream) => {
        val tgzEntry = new TarArchiveEntry(new File(fn), fn) // new TarArchiveEntry(f2Add, entryName)
        val bytes = IOUtils.toByteArray(is)
        tgzEntry.setSize(bytes.length.toLong)
        tgzOut.putArchiveEntry(tgzEntry)
        IOUtils.write(bytes, tgzOut)
        tgzOut.closeArchiveEntry()
        fn
      }
    )

    val lowercasedSet = existingNames.map(_.toLowerCase).toSet

    try {
      val files = new File(path).listFiles
      val files2Add:Array[File] = files.filter(_.getName.toLowerCase.endsWith(suffix))
      val newFiles = files2Add.filter(f => !lowercasedSet.contains(f.getName.toLowerCase))

      newFiles.foreach{ fAdd =>
        addFile2Tgz(tgzOut, path+fAdd.getName, "")
      }
      tgzOut.finish()
      tgzOut.close()
      if (newFiles.nonEmpty) {
        IOUtils.write(bstr.toByteArray, new FileOutputStream(dstFile))
      }
      newFiles.length
    }
    catch {
      case t:Throwable => {
        println(s"Error creating tgz archive: ${t.getMessage}")
        if (tgzOut != null) {
          tgzOut.close()
        }
        throw t
      }
    }
  }

  def createTgz(path:String, suffix:String, dstFile:String):Unit = {
    var tgzOut:TarArchiveOutputStream = null
    try {
      val bfStream = new BufferedOutputStream(new FileOutputStream(dstFile))
      tgzOut = new TarArchiveOutputStream(new GzipCompressorOutputStream(bfStream))
      val files = new File(path).listFiles
      val files2Add = files.filter(_.getName.toLowerCase.endsWith(suffix))
      files2Add.foreach{ fAdd =>
        addFile2Tgz(tgzOut, path+fAdd.getName, "")
      }
    }
    catch {
      case t:Throwable => {
        println(s"Error creating tgz archive: ${t.getMessage}")
      }
    }
    finally {
      if (tgzOut != null) {
        tgzOut.finish()
        tgzOut.close()
      }
    }
  }
  private def addFile2Tgz(tarOut:TarArchiveOutputStream, toAdd:String, entryBase:String):Unit = {
    val f2Add = new File(toAdd)
    if (f2Add.exists) {
      val entryName = entryBase+f2Add.getName
      val tarEntry = new TarArchiveEntry(f2Add, entryName)
      tarOut.putArchiveEntry(tarEntry)
      val fs = new FileInputStream(f2Add)
      IOUtils.copy(fs, tarOut)
      fs.close
      tarOut.closeArchiveEntry()
    }
  }
}