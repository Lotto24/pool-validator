package domain

import java.io._
import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.nio.file._
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.{CancellationException, TimeUnit}

import _root_.util.Task.CancellableSupplierAI
import _root_.util.{Task, TaskImpl}
import domain.ArchiveReader.ProgressInfo
import domain.PoolResource.Filenames
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.apache.commons.io.IOUtils
import org.slf4j.LoggerFactory

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Promise}
import scala.util.{Failure, Success, Try}

trait ArchiveReader {

  def extractArchive(sourceFile: File, destDir: File, progressInfoCb: ProgressInfo => Unit): Task[Try[File]]

  def setTempDir(path: Path): Unit

}

object ArchiveReader {

  case class ProgressInfo(orderDirPath: Path, totalOrdersCount: Long, extractedOrdersCount: Long)

}

/**
  * `ArchiveReader` implementation using "org.apache.commons" "commons-compress".
  **/
class ArchiveReaderImpl(implicit ec: ExecutionContext) extends ArchiveReader {

  private val logger = LoggerFactory.getLogger(getClass)

  var tempDir: Path = _

  override def setTempDir(path: Path): Unit = {
    logger.info(s"setTempDir($path)")
    tempDir = path
  }

  /**
    * @return Future with the destination-directory.
    **/
  override def extractArchive(sourceFile: File, destDir: File, progressInfoCb: ProgressInfo => Unit): Task[Try[File]] = {

    new TaskImpl[Try[File]](info="extractArchive", new CancellableSupplierAI[Try[File]] {

      override def supply(): Try[File] = {
          val tsStart = System.nanoTime()
          //val r = extract_simpleImpl(sourceFile, destDir, progressInfoCb, fIsCancelled = () => this.isCancelled)
          //INFO extract_simpleImpl() runs ~ 30% longer compared to extract_asynChannelImpl()
          val r = extract_asyncChannelImpl(sourceFile, destDir, progressInfoCb, fIsCancelled = () => this.isCancelled)
          val duration = Duration.fromNanos(System.nanoTime() - tsStart)
          logger.info(s"extractArchive()..duration: ${duration.toMillis} ms")
          r.map(_ => destDir)
      }
    })
  }

  private def extract_simpleImpl(sourceFile: File, destDir: File,
                                 progressInfoCb: ProgressInfo => Unit,
                                 fIsCancelled: () => Boolean): Try[Unit] = {
    val destDirPath = destDir.toPath
    val tarIn: TarArchiveInputStream = {
      val gzipIn = new GzipCompressorInputStream(new BufferedInputStream(new FileInputStream(sourceFile)))
      new TarArchiveInputStream(gzipIn)
    }

    // get the count of order-directories (needed for progress-info)
    val countOrderDirs = {
      val gzip = new GzipCompressorInputStream(new BufferedInputStream(new FileInputStream(sourceFile)), true)
      val tar = new TarArchiveInputStream(gzip)
      Iterator.continually(tar.getNextTarEntry).takeWhile(_ != null).count(_.isDirectory)
    }

    var fileCounter = 0
    var ordersExtractedCount = 0

    Iterator.continually(tarIn.getNextTarEntry).takeWhile(_ != null && !fIsCancelled()).foreach { entry =>
      if (entry.isDirectory) {
        Files.createDirectory(destDirPath.resolve(entry.getName))
      } else {
        fileCounter += 1
        val data = IOUtils.toByteArray(tarIn, entry.getSize.toInt)
        val destFilePath = destDirPath.resolve(entry.getName)
        Files.write(destFilePath, data, StandardOpenOption.CREATE, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
        if(destFilePath.endsWith(Filenames.Order)){
          ordersExtractedCount += 1
          progressInfoCb(ProgressInfo(destFilePath, totalOrdersCount = countOrderDirs, extractedOrdersCount = ordersExtractedCount))
        }
      }
    }
    tarIn.close()
    if(fIsCancelled()) Failure(new CancellationException()) else Success()
  }


  private def extract_asyncChannelImpl(sourceFile: File,
                                       destDir: File,
                                       progressInfoCb: ProgressInfo => Unit,
                                       fIsCancelled: () => Boolean): Try[Unit] = {
    val destDirPath = destDir.toPath
    var fileCounter = 0L


    def timed[T](info: String)(aBlock: => T): T = {
      val tsBefore = System.nanoTime()
      val r = aBlock
      logger.info(s"$info lasted ${Duration.fromNanos(System.nanoTime() - tsBefore).toMillis} ms")
      r
    }

    val tarIn: TarArchiveInputStream = {
      val gzip = new GzipCompressorInputStream(new BufferedInputStream(new FileInputStream(sourceFile)), true)
      val tar = new TarArchiveInputStream(gzip)
      tar
    }


    // get the count of order-directories (needed for progress-info)
    val countOrderDirs = timed("count tar-file root-level dirs") {
      val gzip = new GzipCompressorInputStream(new BufferedInputStream(new FileInputStream(sourceFile)), true)
      val tar = new TarArchiveInputStream(gzip)
      Iterator.continually(tar.getNextTarEntry).takeWhile(_ != null).count(_.isDirectory)
    }


    val promise = Promise[Unit]
    val completionHandler = new ExtractArchiveCompletionHandler(totalOrdersCountHint = countOrderDirs, progressInfoCb)
    completionHandler.onReadyHandler = { () =>
      promise.complete(
        if (fIsCancelled()) Failure(new CancellationException()) else Success[Unit]()
      )
    }

    Iterator.continually(tarIn.getNextTarEntry).takeWhile(_ != null && !fIsCancelled()).foreach { entry =>
      if (entry.isDirectory()) {
        Files.createDirectory(destDirPath.resolve(entry.getName))
      } else {
        fileCounter += 1
        val destFilePath = destDirPath.resolve(entry.getName())
        val data = IOUtils.toByteArray(tarIn, entry.getSize)
        val asyCh = AsynchronousFileChannel.open(destFilePath, StandardOpenOption.CREATE, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
        val attachment = new AsynChannelAttachment(destFilePath, fileIndex = fileCounter, channel = asyCh)
        asyCh.write(ByteBuffer.wrap(data), 0, attachment, completionHandler)
      }
    }
    tarIn.close()
    completionHandler.setAllTasksSubmitted(expectedFileCount = fileCounter)
    Await.ready(promise.future, Duration.apply(60, TimeUnit.MINUTES))
    if(fIsCancelled()) Failure(new CancellationException()) else Success()
  }

  private class AsynChannelAttachment(val orderDirPath: Path, val fileIndex: Long, val channel: AsynchronousFileChannel)

  private class ExtractArchiveCompletionHandler(totalOrdersCountHint: Long, progressInfoCb: (ProgressInfo) => Unit) extends CompletionHandler[Integer, AsynChannelAttachment]() {
    private var allTasksSubmitted: Boolean = false
    private var onReadyInvoked = false
    private var expectedFileCount: Long = 0
    private val completedFilesCount = new AtomicLong(0)
    private val completedOrdersCount = new AtomicLong(0)
    var onReadyHandler: () => Unit = _

    def setAllTasksSubmitted(expectedFileCount: Long): Unit = {
      allTasksSubmitted = true
      this.expectedFileCount = expectedFileCount
      checkIsReady()
    }

    private def checkIsReady(): Unit = {
      if (allTasksSubmitted && !onReadyInvoked && completedFilesCount.get() == expectedFileCount) {
        this.synchronized(onReadyInvoked = true)
        onReadyHandler()
      }
    }

    override def completed(result: Integer, attachment: AsynChannelAttachment): Unit = onFileCompleted(attachment)

    override def failed(exc: Throwable, attachment: AsynChannelAttachment): Unit = onFileCompleted(attachment)

    private def onFileCompleted(attachment: AsynChannelAttachment): Unit = {
      if (attachment.orderDirPath.endsWith(Filenames.Order)) {
        completedOrdersCount.incrementAndGet()
        submitProgressInfo(attachment)
      }
      completedFilesCount.incrementAndGet()
      attachment.channel.close()
      checkIsReady()
    }

    private def submitProgressInfo(attachment: AsynChannelAttachment): Unit = {
      progressInfoCb.apply(ProgressInfo(orderDirPath = attachment.orderDirPath, totalOrdersCount = totalOrdersCountHint,
        extractedOrdersCount = completedOrdersCount.get()))
    }
  }

}
