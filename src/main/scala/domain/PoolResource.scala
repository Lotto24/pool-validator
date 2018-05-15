package domain

import java.net.URI
import java.nio.file.Path
import java.time.{LocalDate, ZonedDateTime}
import java.time.format.DateTimeFormatter

import domain.Order._
import domain.OrderMetadata.OrderHedgingData
import domain.PoolMetadata.PoolDigest
import domain.products.GamingProduct.GamingProductId
import domain.products.GamingProductOrder
import domain.products.ParticipationPools.ParticipationPoolId
import org.bouncycastle.tsp.TimeStampResponse

import scala.util.Try

/**
  * Base trait for classes representing pool archive resources (e.g. the archive, an order-directory, order-documents)
  **/
sealed trait PoolResource {

  def docPath: Path

  def rawData: IndexedSeq[Byte]

}


object PoolResource {

  /** Enumeration for `PoolResource`-types. */
  object PRType extends Enumeration {
    val PoolDirectory, MetaData, OrderDirectory, Order, OrderResult, OrderResultSignature,
    OrderResultSignatureTimestamp, OrderSignature, OrderMetadata, PoolDigestTimestamp = Value
  }

  object Filenames {
    val Metadata: String = "metadata.json"
    val Order: String = "order"
    val OrderResult: String = "order.result"
    val OrderResultSignature: String = "order.result.signature"
    val OrderResultSignatureTimestamp: String = "order.result.signature.timestamp"
    val OrderSignature: String = "order.signature"
    val OrderMetadata: String = "order.metadata"
    val PoolDigestTimestamp: String = "participation-pool-digest.timestamp"
  }

  private val orderDocFileNames = Set(Filenames.Order, Filenames.OrderResult, Filenames.OrderResultSignature,
    Filenames.OrderResultSignatureTimestamp, Filenames.OrderSignature)

  def isOrderDoc(filename: String): Boolean = orderDocFileNames.contains(filename)

  def docTypeToFileName(docType: PRType.Value): String = {
    docType match {
      case PRType.MetaData => Filenames.Metadata
      case PRType.Order => Filenames.Order
      case PRType.OrderResult => Filenames.OrderResult
      case PRType.OrderResultSignature => Filenames.OrderResultSignature
      case PRType.OrderResultSignatureTimestamp => Filenames.OrderResultSignatureTimestamp
      case PRType.OrderSignature => Filenames.OrderSignature
      case PRType.OrderMetadata => Filenames.OrderMetadata
      case PRType.PoolDigestTimestamp => Filenames.PoolDigestTimestamp
      case x => sys.error(s"unexpected arg: $x")
    }
  }
}


/**
  * Base trait for classes representing the (json-) documents which are created (or are involved) in the order processing.
  **/
trait OrderDoc extends PoolResource


object PoolMetadata {

  case class PoolDigest(base64: IndexedSeq[Byte], algorithm: String)

}


case class PoolMetadata(override val docPath: Path,
                        gamingProduct: URI,
                        participationPoolId: ParticipationPoolId,
                        productId: GamingProductId,
                        drawDate: ZonedDateTime,
                        poolDigest: Option[PoolDigest],
                        override val rawData: IndexedSeq[Byte]) extends PoolResource {
}


case class Order(metaData: Metadata,
                 gamingProductOrders: Map[URI, GamingProductOrder],
                 orderId: OrderId,
                 override val docPath: Path,
                 override val rawData: IndexedSeq[Byte]) extends OrderDoc

object Order {

  case class Metadata(retailerHref: String,
                      retailCustomer: String,
                      retailerOrderReference: String,
                      creationDate: ZonedDateTime)

  object Metadata {
    val CreationDateFormat: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
  }

}


case class OrderResult(retailerOrderReference: String,
                       creationTime: ZonedDateTime,
                       orderDigest: IndexedSeq[Byte],
                       retailerHref: String,
                       orderProcessingResult: String,
                       override val docPath: Path,
                       override val rawData: IndexedSeq[Byte]) extends OrderDoc

object OrderResult {
  val Accepted: String = "accepted"
  val dateTimeFormat: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
}


/**
  * @param signature base64 encoded signature
  * */
case class OrderResultSignature(keyId: String,
                                algorithm: String,
                                signature: IndexedSeq[Byte],
                                override val docPath: Path,
                                override val rawData: IndexedSeq[Byte]) extends OrderDoc


/**
  * @param rawData base64 encoded ASN.1 timestamp response
  * */
case class OrderResultSignatureTimestamp(value: String,
                                         timeStampResponse: Try[TimeStampResponse],
                                         override val docPath: Path,
                                         override val rawData: IndexedSeq[Byte]) extends OrderDoc


/**
  * @param signature base64 encoded signature
  * */
case class OrderSignature(keyId: String,
                          algorithm: String,
                          signature: IndexedSeq[Byte],
                          override val docPath: Path,
                          override val rawData: IndexedSeq[Byte]) extends OrderDoc

/**
  * `OrderMetadata` is a container for data that is not required for the chain of proof, but nevertheless may be
  * of interest (e.g. hedging data).
  * */
case class OrderMetadata(hedgingData: OrderHedgingData,
                         override val docPath: Path,
                         override val rawData: IndexedSeq[Byte]) extends OrderDoc

object OrderMetadata{

  case class OrderHedgingData(hedgingDataPerProduct: Map[GamingProductId, Seq[DrawHedgingData]])

  /**Hedging data for single draw / participation-pool*/
  case class DrawHedgingData(poolId: ParticipationPoolId, drawDate: LocalDate, hedgingChannel: Option[String])

}

/**
  * @param rawData base64 encoded ASN.1 timestamp response
  * */
case class PoolDigestTimestamp(value: String,
                                         override val docPath: Path,
                                         override val rawData: IndexedSeq[Byte]) extends OrderDoc


/** Container for all `OrderDoc`s for a single order.*/
case class OrderDocs(
  orderId: OrderId,
  order: Try[Order],
  orderResult: Try[OrderResult],
  orderResultSignature: Try[OrderResultSignature],
  orderResultSignatureTimestamp: Try[OrderResultSignatureTimestamp],
  orderSignature: Try[OrderSignature],
  orderMetadata: Try[OrderMetadata]
) {
  def getAll: IndexedSeq[Try[OrderDoc]] = Vector(order, orderResult, orderResultSignature, orderResultSignatureTimestamp, orderSignature, orderMetadata)
}
