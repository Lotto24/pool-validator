package domain

import java.io._
import java.nio.file.{Path, Paths}

import _root_.util.Utils
import com.google.common.cache.{Cache, CacheBuilder}
import domain.PoolResource.Filenames
import monix.reactive.Observable
import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveInputStream}
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.apache.commons.io.IOUtils
import org.slf4j.LoggerFactory

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}


class PoolResourceProviderTarGzImpl(
  sourceFilePath: Path,
  orderDocsParser: OrderDocumentsParser,
  orderDocsCacheSize: Int
) extends PoolResourceProviderAI {

  private val logger = LoggerFactory.getLogger(getClass)

  private var cachedPoolMetadata = Option.empty[Try[PoolMetadata]]
  private var cachedPoolDigestTimestamp: Option[Try[PoolDigestTimestamp]] = None
  
  private val orderDocsCache: Cache[String, Try[OrderDocs]] = CacheBuilder.newBuilder()
    .maximumSize(orderDocsCacheSize).build().asInstanceOf[Cache[String, Try[OrderDocs]]]

  override def init(): Future[Unit] = Future.successful()

  override def dispose(): Future[Unit] = Future.successful()

  override def getOrdersCount(): Try[Int] = Try {
    val gzip = new GzipCompressorInputStream(new BufferedInputStream(new FileInputStream(sourceFilePath.toFile)), true)
    val tarIn = new TarArchiveInputStream(gzip)
    val result = Iterator.continually(tarIn.getNextTarEntry).takeWhile(_ != null).count(_.isDirectory)
    tarIn.close()
    result
  }

  private def newTarInputStream(): Try[TarArchiveInputStream] = {
    Utils.tryFileReadable(sourceFilePath.toFile, isDirectoryHint = false).map { file =>
      val gzipIn = new GzipCompressorInputStream(new BufferedInputStream(new FileInputStream(file)))
      new TarArchiveInputStream(gzipIn)
    }
  }

  override def getOrderDocsObservable(): Observable[Try[OrderDocs]] = {
    newTarInputStream() match {
      case Success(tarIn) =>
        val orderDocsBuilder = new OrderDocsBuilder(tarIn)
        Observable
          .repeatEval(tarIn.getNextTarEntry)
          .takeWhile(_ != null || orderDocsBuilder.hasPartialOrderDocs)
          .doOnTerminate { _ =>
            cachedPoolMetadata = Some(orderDocsBuilder.cachedPoolMetadata
              .getOrElse(Failure(new FileNotFoundException(s"not found: ${Paths.get(Filenames.Metadata)}"))))
            cachedPoolDigestTimestamp = Some(orderDocsBuilder.cachedPoolDigestTimestamp
              .getOrElse(Failure(new FileNotFoundException(s"not found: ${Paths.get(Filenames.PoolDigestTimestamp)}"))))
            tarIn.close()
          }
          .doOnSubscriptionCancel(() => tarIn.close())
          .map(entry => orderDocsBuilder.processEntry(entry))
          .collect { case Some(orderDocs) => orderDocs }
      case Failure(t) =>
        Observable.raiseError(t)
    }
  }

  override def getOrderDocs(orderId: OrderId): Try[OrderDocs] = {
    orderDocsCache.get(orderId, Utils.toCallable {
      newTarInputStream().flatMap { tarIn =>
        val orderDocsBuilder = new OrderDocsBuilder(tarIn, filter = Some(
          entry => Paths.get(entry.getName).startsWith(orderId)
        ))
        val iter = Iterator.continually(tarIn.getNextTarEntry)
          .takeWhile(_ != null || orderDocsBuilder.hasPartialOrderDocs)
          .map(entry => orderDocsBuilder.processEntry(entry))
        val result = iter.find(_.isDefined).map(_.get).getOrElse(Failure(new Exception(s"an order with id='$orderId' does not exist")))
        tarIn.close()
        result
      }
    })
  }

  override def getPoolMetadata(): Try[PoolMetadata] = {
    cachedPoolMetadata match {
      case Some(result) => 
        result
      case _ =>
        val filePath = Paths.get(Filenames.Metadata)
        val tmp = getFileContent(filePath).transform(
          data => orderDocsParser.parsePoolData(data, filePath),
          throwable => Failure(throwable)
        )
        cachedPoolMetadata = Some(tmp)
        tmp
    }
  }

  override protected def getFileContent(path: Path): Try[IndexedSeq[Byte]] = {
    newTarInputStream().flatMap { tarIn =>
      val result = Iterator.continually(tarIn.getNextTarEntry)
        .takeWhile(_ != null)
        .find(entry => Paths.get(entry.getName) == path)
        .map { entry =>
          if (entry.isDirectory) {
            Failure(new Exception(s"path $path is a directory"))
          } else {
            Try(IOUtils.toByteArray(tarIn, entry.getSize.toInt).toIndexedSeq)
          }
        }.getOrElse(Failure(new FileNotFoundException(s"not found: $path")))
      tarIn.close()
      result
    }
  }

  override def getOrder(orderId: OrderId): Try[Order] = getOrderDocs(orderId).flatMap(_.order)

  override def getOrderResult(orderId: OrderId): Try[OrderResult] = getOrderDocs(orderId).flatMap(_.orderResult)

  override def getOrderResultSignature(orderId: OrderId): Try[OrderResultSignature] = getOrderDocs(orderId).flatMap(_.orderResultSignature)

  override def getOrderResultSignatureTimestamp(orderId: OrderId): Try[OrderResultSignatureTimestamp] = {
    getOrderDocs(orderId).flatMap(_.orderResultSignatureTimestamp)
  }

  override def getOrderSignature(orderId: OrderId): Try[OrderSignature] = getOrderDocs(orderId).flatMap(_.orderSignature)

  override def getOrderMetadata(orderId: OrderId): Try[OrderMetadata] = {
    getOrderDocs(orderId).flatMap(_.orderMetadata)
  }

  override def getPoolDigestTimestamp(): Try[PoolDigestTimestamp] = cachedPoolDigestTimestamp match {
    case Some(result) => 
      result
    case _ =>
      val tmp = readFromFile[PoolDigestTimestamp](Paths.get(Filenames.PoolDigestTimestamp)) { (data, filePath) =>
        orderDocsParser.parsePoolDigestTimestamp(data, filePath)
      }
      cachedPoolDigestTimestamp = Some(tmp)
      tmp
  }

  private class OrderDocsBuilder(
    tarIn: TarArchiveInputStream,
    filter: Option[TarArchiveEntry => Boolean] = None,
    var cachedPoolMetadata: Option[Try[PoolMetadata]] = None,
    var cachedPoolDigestTimestamp: Option[Try[PoolDigestTimestamp]] = None,
    private var currentPartialDocs: Option[PartialOrderDocs] = None
  ) {

    def hasPartialOrderDocs: Boolean = currentPartialDocs.nonEmpty

    def processEntry(entry: TarArchiveEntry): Option[Try[OrderDocs]] = {
      if (entry == null) {
        val tmp = currentPartialDocs.map(x => Success(x.toOrderDocs))
        currentPartialDocs = None
        tmp
      } else {
        val considered = filter.map(_.apply(entry)).getOrElse(true)
        if (considered) {
          if (entry.isDirectory) {
            val tmp = currentPartialDocs.map(x => Success(x.toOrderDocs))
            currentPartialDocs = Some(PartialOrderDocs(orderId = Paths.get(entry.getName).getFileName.toString))
            tmp
          } else {
            if(currentPartialDocs.isEmpty) {
              //read & cache some top-level documents if so
              entry.getName match {
                case Filenames.PoolDigestTimestamp =>
                  val data = IOUtils.toByteArray(tarIn, entry.getSize.toInt)
                  cachedPoolDigestTimestamp = Some(orderDocsParser.parsePoolDigestTimestamp(data, Paths.get(Filenames.PoolDigestTimestamp)))
                case Filenames.Metadata =>
                  val data = IOUtils.toByteArray(tarIn, entry.getSize.toInt)
                  cachedPoolMetadata = Some(orderDocsParser.parsePoolData(data, Paths.get(Filenames.Metadata)))
                case other => 
                  logger.info(s"unexpected file: $other")
              }              
            }
            currentPartialDocs = currentPartialDocs.map(partialDocs => amendedPartialOrderDocs(partialDocs, entry))
            None
          }
        } else {
          None
        }
      }
    }

    def result(): Option[OrderDocs] = currentPartialDocs.map(_.toOrderDocs)

    private def amendedPartialOrderDocs(partialDocs: PartialOrderDocs, entry: TarArchiveEntry): PartialOrderDocs = {
      val data = IOUtils.toByteArray(tarIn, entry.getSize.toInt)
      val entryPath = Paths.get(entry.getName)
      entryPath.getFileName.toString match {
        case Filenames.Order =>
          partialDocs.copy(order = Some(orderDocsParser.parseOrder(data, Paths.get(entry.getName))))
        case Filenames.OrderResult =>
          partialDocs.copy(orderResult = Some(orderDocsParser.parseOrderResult(data, Paths.get(entry.getName))))
        case Filenames.OrderResultSignature =>
          partialDocs.copy(orderResultSignature = Some(orderDocsParser.parseOrderResultSignature(data, Paths.get(entry.getName))))
        case Filenames.OrderResultSignatureTimestamp =>
          partialDocs.copy(
            orderResultSignatureTimestamp = Some(orderDocsParser.parseOrderResultSignatureTimestamp(data, Paths.get(entry.getName))))
        case Filenames.OrderSignature =>
          partialDocs.copy(
            orderSignature = Some(orderDocsParser.parseOrderSignature(data, Paths.get(entry.getName))))
        case Filenames.OrderMetadata =>
          partialDocs.copy(
            orderMetadata = Some(orderDocsParser.parseOrderMetadata(data, Paths.get(entry.getName))))
        case other =>
          logger.info(s"unexpected OrderDoc: $other (orderId: ${partialDocs.orderId})")
          partialDocs
      }
    }
  }

  private case class PartialOrderDocs(
    orderId: OrderId,
    order: Option[Try[Order]] = None,
    orderResult: Option[Try[OrderResult]] = None,
    orderResultSignature: Option[Try[OrderResultSignature]] = None,
    orderResultSignatureTimestamp: Option[Try[OrderResultSignatureTimestamp]] = None,
    orderSignature: Option[Try[OrderSignature]] = None,
    orderMetadata: Option[Try[OrderMetadata]] = None
  ) {

    def toOrderDocs: OrderDocs = {
      OrderDocs(
        orderId,
        order.getOrElse(missingDoc(Filenames.Order)),
        orderResult.getOrElse(missingDoc(Filenames.OrderResult)),
        orderResultSignature.getOrElse(missingDoc(Filenames.OrderResultSignature)),
        orderResultSignatureTimestamp.getOrElse(missingDoc(Filenames.OrderResultSignatureTimestamp)),
        orderSignature.getOrElse(missingDoc(Filenames.OrderSignature)),
        orderMetadata.getOrElse(missingDoc(Filenames.OrderMetadata))
      )
    }

    private def missingDoc[T](fileName: String): Failure[T] = new Failure(new Exception(s"missing: $fileName"))
  }

}
