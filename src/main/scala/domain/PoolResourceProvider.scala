package domain

import java.nio.file.Path

import monix.reactive.Observable

import scala.concurrent.Future
import scala.util.{Failure, Try}


/**
  * `PoolResourceProvider` provides order-documents and other resources related to a participation pool archive.
  **/
trait PoolResourceProvider {

  def init(): Future[Unit]

  def dispose(): Future[Unit]

  def getOrdersCount(): Try[Int]

  def getOrderDocsObservable(): Observable[Try[OrderDocs]]

  def getOrderDocs(orderId: OrderId): Try[OrderDocs]

  def getPoolMetadata(): Try[PoolMetadata]

  def getOrder(orderId: OrderId): Try[Order]

  def getOrderResult(orderId: OrderId): Try[OrderResult]

  def getOrderResultSignature(orderId: OrderId): Try[OrderResultSignature]

  def getOrderResultSignatureTimestamp(orderId: OrderId): Try[OrderResultSignatureTimestamp]

  def getOrderSignature(orderId: OrderId): Try[OrderSignature]

  def getOrderMetadata(orderId: OrderId): Try[OrderMetadata]

  def getPoolDigestTimestamp(): Try[PoolDigestTimestamp]

}


trait PoolResourceProviderAI extends PoolResourceProvider {

  protected def getFileContent(filePath: Path): Try[IndexedSeq[Byte]]// = getInputStream(filePath).flatMap(is => Utils.getAsSeq(is, closeStream = true))

  protected def readFromFile[T <: PoolResource](filePath: Path)(parseFunction: (IndexedSeq[Byte], Path) => Try[T]): Try[T] = {
    getFileContent(filePath).transform(
      data => parseFunction(data, filePath),
      throwable => Failure(throwable)
    )
  }

}
