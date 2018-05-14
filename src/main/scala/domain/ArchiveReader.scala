package domain

import java.io._
import java.nio.file._

import domain.PoolResource.Filenames
import monix.eval.Task
import monix.execution.Scheduler
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.apache.commons.io.IOUtils
import org.slf4j.LoggerFactory

import scala.concurrent.duration.Duration
import scala.util.{Success, Try}


object ArchiveReader {

  private val logger = LoggerFactory.getLogger(getClass)

  def extractArchive(sourceFile: File, destDir: File)(implicit scheduler: Scheduler): Task[File] = {
    Task {
        val tsStart = System.nanoTime()
        val r = extractImpl(sourceFile, destDir)
        val duration = Duration.fromNanos(System.nanoTime() - tsStart)
        logger.info(s"extractArchive()..duration: ${duration.toMillis} ms")
        r.failed.foreach { t => throw new Exception(s"extractArchive($sourceFile, ..) failed: ${t.getMessage}", t)}
        r.get
    }
  }

  private def extractImpl(sourceFile: File, destDir: File): Try[File] = {
    val destDirPath = destDir.toPath
    val tarIn: TarArchiveInputStream = {
      val gzipIn = new GzipCompressorInputStream(new BufferedInputStream(new FileInputStream(sourceFile)))
      new TarArchiveInputStream(gzipIn)
    }
    var fileCounter = 0
    var ordersExtractedCount = 0
    Iterator.continually(tarIn.getNextTarEntry).takeWhile(_ != null ).foreach { entry =>
      if (entry.isDirectory) {
        Files.createDirectory(destDirPath.resolve(entry.getName))
      } else {
        fileCounter += 1
        val data = IOUtils.toByteArray(tarIn, entry.getSize.toInt)
        val destFilePath = destDirPath.resolve(entry.getName)
        Files.write(destFilePath, data, StandardOpenOption.CREATE, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
        if(destFilePath.endsWith(Filenames.Order)){
          ordersExtractedCount += 1
        }
      }
    }
    tarIn.close()
    Success(destDir)
  }
}
