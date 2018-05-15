package domain

import java.nio.file.Path

import _root_.util.Utils
import domain.PoolResource.Filenames
import monix.reactive.Observable
import util.Utils.DirectoryFilter

import scala.concurrent.Future
import scala.util.{Success, Try}


class PoolResourceProviderDirectoryImpl(
  protected val poolDirPath: Path, protected val orderDocsParser: OrderDocumentsParser
) extends PoolResourceProviderAI {

  override def init(): Future[Unit] = Future.successful()

  override def dispose(): Future[Unit] = Future.successful()

  override def getOrdersCount(): Try[Int] = Utils.tryFileReadable(poolDirPath.toFile, isDirectoryHint = true).map { baseDir =>
    baseDir.listFiles(DirectoryFilter).size
  }

  override def getOrderDocsObservable(): Observable[Try[OrderDocs]] = {
    if(poolDirPath.toFile.isFile) Observable.raiseError(new IllegalArgumentException(s"$poolDirPath is a File (directory expected)")) else {
      Observable.fromIterator(poolDirPath.toFile.listFiles(DirectoryFilter).toIterator).map(orderDir => getOrderDocs(orderDir.getName))
    }
  }

  override def getPoolMetadata(): Try[PoolMetadata] = {
    readFromFile[PoolMetadata](poolDirPath.resolve(Filenames.Metadata)) { (data, filePath) =>
      orderDocsParser.parsePoolData(data, filePath)
    }
  }

  override def getOrder(orderId: OrderId): Try[Order] = {
    readFromFile[Order](poolDirPath.resolve(orderId).resolve(Filenames.Order)) { (data, filePath) =>
      orderDocsParser.parseOrder(data, filePath)
    }
  }

  override def getOrderResult(orderId: OrderId): Try[OrderResult] = {
    readFromFile[OrderResult](poolDirPath.resolve(orderId).resolve(Filenames.OrderResult)) { (data, filePath) =>
      orderDocsParser.parseOrderResult(data, filePath)
    }
  }

  override def getOrderResultSignature(orderId: OrderId): Try[OrderResultSignature] = {
    readFromFile[OrderResultSignature](poolDirPath.resolve(orderId).resolve(Filenames.OrderResultSignature)) { (data, filePath) =>
      orderDocsParser.parseOrderResultSignature(data, filePath)
    }
  }

  override def getOrderResultSignatureTimestamp(orderId: OrderId): Try[OrderResultSignatureTimestamp] = {
    readFromFile[OrderResultSignatureTimestamp](poolDirPath.resolve(orderId).resolve(Filenames.OrderResultSignatureTimestamp)) { (data, filePath) =>
      orderDocsParser.parseOrderResultSignatureTimestamp(data, filePath)
    }
  }

  override def getOrderSignature(orderId: OrderId): Try[OrderSignature] = {
    readFromFile[OrderSignature](poolDirPath.resolve(orderId).resolve(Filenames.OrderSignature)) { (data, filePath) =>
      orderDocsParser.parseOrderSignature(data, filePath)
    }
  }

  override def getOrderMetadata(orderId: OrderId): Try[OrderMetadata] = {
    readFromFile[OrderMetadata](poolDirPath.resolve(orderId).resolve(Filenames.OrderMetadata)) { (data, filePath) =>
      orderDocsParser.parseOrderMetadata(data, filePath)
    }
  }

  override def getPoolDigestTimestamp(): Try[PoolDigestTimestamp] = {
    readFromFile[PoolDigestTimestamp](poolDirPath.resolve(Filenames.PoolDigestTimestamp)) { (data, filePath) =>
      orderDocsParser.parsePoolDigestTimestamp(data, filePath)
    }
  }

  override def getOrderDocs(orderId: OrderId): Try[OrderDocs] = Success(OrderDocs(
    orderId = orderId,
    order = getOrder(orderId),
    orderResult = getOrderResult(orderId),
    orderResultSignature = getOrderResultSignature(orderId),
    orderResultSignatureTimestamp = getOrderResultSignatureTimestamp(orderId),
    orderSignature = getOrderSignature(orderId),
    orderMetadata = getOrderMetadata(orderId)
  ))

  override protected def getFileContent(filePath: Path): Try[IndexedSeq[Byte]] = {
    Utils.getInputStream(filePath).flatMap(is => Utils.getAsSeq(is, closeStream = true))
  }
}
