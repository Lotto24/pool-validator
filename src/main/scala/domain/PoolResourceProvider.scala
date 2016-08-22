package domain

import java.io._
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.time.{LocalDate, ZonedDateTime}
import java.util.Base64

import _root_.util.Utils
import domain.Order._
import domain.OrderMetadata.{DrawHedgingData, OrderHedgingData}
import domain.PoolMetadata.PoolDigest
import domain.PoolResource.Filenames
import domain.PoolResourceProvider.ProductOrderFactory
import domain.products.GamingProduct._
import domain.products.ejs.EjsProductOrderFactory
import domain.products.ems.EmsProductOrderFactory
import domain.products.gls.GlsProductOrderFactory
import domain.products.glss.GlsSProductOrderFactory
import domain.products.s6.S6ProductOrderFactory
import domain.products.s77.S77ProductOrderFactory
import domain.products.{GamingProductOrder, ML24GamingProduct, ParticipationPools}
import org.bouncycastle.tsp.TimeStampResponse
import util.Utils.DirectoryFilter
import play.api.libs.json._

import scala.util.{Failure, Success, Try}


/**
  * `PoolResourceProvider` provides order-documents and other resources related to a participation pool archive.
  **/
trait PoolResourceProvider {

  def getPoolMetadata(poolDirPath: Path): Try[PoolMetadata]

  def getOrder(orderDirPath: Path): Try[Order]

  def getOrderResult(orderDirPath: Path): Try[OrderResult]

  def getOrderResultSignature(orderDirPath: Path): Try[OrderResultSignature]

  def getOrderResultSignatureTimestamp(orderDirPath: Path): Try[OrderResultSignatureTimestamp]

  def getOrderSignature(orderDirPath: Path): Try[OrderSignature]

  def getOrderMetadata(getParent: Path): Try[OrderMetadata]

  def getPoolDigestTimestamp(poolDirPath: Path): Try[PoolDigestTimestamp]

  /**
    * Delivers the `Path`s of the order directories for the order specified by `poolLocation`.
    **/
  def getOrderDirPaths(poolLocation: Path): Try[IndexedSeq[Path]]

  /**
    * Delivers the `Path`s of the order documents for the order specified by `orderDirPath`.
    **/
  def getOrderDocPaths(orderDirPath: Path): Try[IndexedSeq[Path]]
}

object PoolResourceProvider {

  /**
    * Trait for factories that create `GamingProductOrder`-instances.
    * `Order`-documents may contain orders for different products which may also have different json-representations.
    * An `OrderDocReader` needs an appropriate `ProductOrderFactory` to instantiate a concrete `GamingProductOrder`.
    **/
  trait ProductOrderFactory {

    def isApplicableFor(productURI: URI): Boolean

    def create(productURI: URI, orderData: JsObject, docPath: Path): Try[GamingProductOrder]

  }

  object ProductOrderFactory {

    /**
      * Delivers a `ProductOrderFactory` which is capable to create `ProductOrder`s for all known `GamingProductOrder`s.
      **/
    def allProductsOrderFactory: ProductOrderFactory = new CompositeProductOrderFactory(
      Seq(
        new EjsProductOrderFactory,
        new EmsProductOrderFactory,
        new GlsProductOrderFactory,
        new GlsSProductOrderFactory,
        new S6ProductOrderFactory,
        new S77ProductOrderFactory
      ))
  }

}


class PoolResourceProviderImpl(productOrderFactory: ProductOrderFactory = ProductOrderFactory.allProductsOrderFactory) extends PoolResourceProvider {

  /**
    * Intended to be overridden in tests (e.g. to simulate missing files)
    * */
  protected def getInputStream(filePath: Path): Try[InputStream] = Utils.getInputStream(filePath)

  override def getPoolMetadata(poolDirPath: Path): Try[PoolMetadata] = {
    val filePath = poolDirPath.resolve(Filenames.Metadata)
    getInputStream(filePath).flatMap(is => Utils.getAsSeq(is, closeStream = true)).map { data =>
      val node = Json.parse(data.toArray)
      val productUri = new URI((node \ "gaming-product").as[String])
      val poolIdStr = (node \ "participation-pool-id").as[String]
      val (productId: GamingProductId, drawDate: LocalDate) = ParticipationPools.parseParticipationPoolId(poolIdStr)
      if (!ML24GamingProduct.All.exists(_.id == productId))
        throw new Exception(s"unexpected productId: $productId")

      val digestData: Option[PoolDigest] = {
        (node \ "participation-pool-digest").toOption match {
          case None | Some(play.api.libs.json.JsNull) =>
            None
          case _ =>
            val data = (node \ "participation-pool-digest" \ "base64").as[String].getBytes(StandardCharsets.UTF_8)
            val algorithm = (node \ "participation-pool-digest" \ "algorithm").as[String]
            Some(PoolMetadata.PoolDigest(base64 = data, algorithm = algorithm))
        }
      }

      PoolMetadata(docPath = poolDirPath.resolve(Filenames.Metadata),
        gamingProduct = productUri,
        participationPoolId = poolIdStr,
        productId = productId,
        drawDate = ZonedDateTime.parse((node \ "draw-time").as[String]),
        poolDigest = digestData,
        rawData = data.toIndexedSeq
      )
    }
  }

  override def getOrder(orderDirPath: Path): Try[Order] = getOrderForFilePath(orderDirPath.resolve(Filenames.Order))

  def getOrderForFilePath(filePath: Path): Try[Order] = {
    getInputStream(filePath).flatMap(is => Utils.getAsSeq(is, closeStream = true)).map { data =>
      val node = Json.parse(data.toArray)
      val metaData = Metadata(
        retailerHref = (node \ "metadata" \ "retailer" \ "href").as[String],
        retailCustomer = (node \ "metadata" \ "retail-customer").as[String],
        retailerOrderReference = (node \ "metadata" \ "retailer-order-reference").as[String],
        creationDate = {
          ZonedDateTime.parse((node \ "metadata" \ "creation-date").as[String], Metadata.CreationDateFormat)
        }
      )
      val orders = (node \ "gaming-product-orders").as[JsObject]
      val gamingProductOrders: Map[URI, GamingProductOrder] = orders.fields.map { field =>
        val productURI: URI = new URI(field._1)
        val lotteryProductOrder = productOrderFactory.create(productURI, field._2.as[JsObject], filePath).get
        (productURI, lotteryProductOrder)
      }.toMap

      assert(filePath.getNameCount >= 2, s"p.getNameCount[${filePath.getNameCount}] should be >= 2")
      val directoryName = filePath.getName(filePath.getNameCount - 2).toString
      Order(metaData = metaData, gamingProductOrders = gamingProductOrders,
        docPath = filePath, directoryName = directoryName, rawData = data)
    }
  }

  override def getOrderResult(orderDirPath: Path): Try[OrderResult] = {
    val filePath = orderDirPath.resolve(Filenames.OrderResult)
    getInputStream(filePath).flatMap(is => Utils.getAsSeq(is, closeStream = true)).map { data =>
      val node = Json.parse(data.toArray)
      val creationTime: ZonedDateTime = {
        (node \ "creation-time").toOption.map(_.as[String]).fold(Option.empty[ZonedDateTime]) { s =>
          Option(ZonedDateTime.parse(s, OrderResult.dateTimeFormat))
        }.orNull
      }
      val retailerHref: String = (node \ "retailer" \ "href").as[String]
      OrderResult(
        retailerOrderReference = (node \ "retailer-order-reference").as[String],
        creationTime = creationTime,
        orderDigest = (node \ "order-digest").as[String].getBytes(StandardCharsets.UTF_8),
        retailerHref = retailerHref,
        orderProcessingResult = (node \ "order-processing-result").as[String],
        docPath = orderDirPath.resolve(Filenames.OrderResult),
        rawData = data
      )
    }
  }

  override def getOrderResultSignature(orderDirPath: Path): Try[OrderResultSignature] = {
    val filePath = orderDirPath.resolve(Filenames.OrderResultSignature)
    getInputStream(filePath).flatMap(is => Utils.getAsSeq(is, closeStream = true)).map { data =>
      val node = Json.parse(data.toArray)
      OrderResultSignature(
        keyId = (node \ "keyId").as[String],
        algorithm = (node \ "algorithm").as[String],
        signature = (node \ "signature").as[String].getBytes(StandardCharsets.UTF_8),
        orderDirPath.resolve(Filenames.OrderResultSignature),
        rawData = data
      )
    }
  }

  override def getOrderResultSignatureTimestamp(orderDirPath: Path): Try[OrderResultSignatureTimestamp] = {
    val filePath = orderDirPath.resolve(Filenames.OrderResultSignatureTimestamp)
    getInputStream(filePath).flatMap(is => Utils.getAsSeq(is, closeStream = true)).map { base64data =>
      val data = Base64.getDecoder.decode(base64data.toArray)
      val tsResponse = Try {
        new TimeStampResponse(data)
      }

      OrderResultSignatureTimestamp(value = new String(base64data.toArray, StandardCharsets.UTF_8),
        timeStampResponse = tsResponse,
        docPath = orderDirPath.resolve(Filenames.OrderResultSignatureTimestamp),
        rawData = base64data
      )
    }
  }

  override def getOrderSignature(orderDirPath: Path): Try[OrderSignature] = {
    val filePath = orderDirPath.resolve(Filenames.OrderSignature)
    getInputStream(filePath).flatMap(is => Utils.getAsSeq(is, closeStream = true)).map { data =>
      val node = Json.parse(data.toArray)
      OrderSignature(keyId = (node \ "keyId").as[String],
        algorithm = (node \ "algorithm").as[String],
        signature = (node \ "signature").as[String].getBytes(StandardCharsets.UTF_8),
        docPath = orderDirPath.resolve(Filenames.OrderSignature),
        rawData = data
      )
    }
  }

  override def getOrderMetadata(orderDirPath: Path): Try[OrderMetadata] = {
    getOrderMetadataForFilePath(orderDirPath.resolve(Filenames.OrderMetadata))
  }

  def getOrderMetadataForFilePath(filePath: Path): Try[OrderMetadata] = {
    getInputStream(filePath).flatMap(is => Utils.getAsSeq(is, closeStream = true)).map { data =>
      val node = Json.parse(data.toArray)
      val jsonHedgingData = (node \ "hedging-data").as[JsObject]

      val hedgingDataPerProduct: Map[GamingProductId, Seq[DrawHedgingData]] = jsonHedgingData.value.iterator.map {
        case (productURI, jsonProductOrderHedgingData) =>
          val productId = gamingProductIdFromURI(new URI(productURI))
          val drawHedgingData = jsonProductOrderHedgingData.as[JsObject].fields.map { case (poolId, drawHedgingData) =>
            val hedgingChannel: Option[String] = (drawHedgingData \ "hedging-channel").toOption.map(_.as[String])
            val (_, drawDate) = ParticipationPools.parseParticipationPoolId(poolId)
            DrawHedgingData(poolId = poolId, drawDate, hedgingChannel = hedgingChannel)
          }
          (productId, drawHedgingData)
      }.toMap
      OrderMetadata(OrderHedgingData(hedgingDataPerProduct), filePath, data)
    }
  }

  override def getPoolDigestTimestamp(poolDirPath: Path): Try[PoolDigestTimestamp] = {
    val filePath = poolDirPath.resolve(Filenames.PoolDigestTimestamp)
    getInputStream(filePath).flatMap(is => Utils.getAsSeq(is, closeStream = true)).map { data =>
      PoolDigestTimestamp(value = new String(data.toArray, StandardCharsets.UTF_8),
        docPath = poolDirPath.resolve(Filenames.PoolDigestTimestamp), rawData = data)
    }
  }

  override def getOrderDirPaths(poolLocation: Path): Try[IndexedSeq[Path]] = {
    val poolDirectory = poolLocation.toFile
    Utils.tryFileReadable(poolDirectory, isDirectoryHint = true).map { baseDir =>
      baseDir.listFiles(DirectoryFilter).toIndexedSeq.map(_.toPath)
    }
  }

  override def getOrderDocPaths(orderDirPath: Path): Try[IndexedSeq[Path]] = {
    Try {
      orderDirPath.toFile.listFiles().map(_.toPath)
    }
  }
}


/**
  * Combines several `ProductOrderFactory` instances to allow the processing of multi-product orders.
  **/
class CompositeProductOrderFactory(productOrderFactories: Seq[ProductOrderFactory]) extends ProductOrderFactory {

  override def isApplicableFor(productURI: URI): Boolean = productOrderFactories.exists(_.isApplicableFor(productURI))

  def create(productURI: URI, orderData: JsObject, docPath: Path): Try[GamingProductOrder] = {
    productOrderFactories.find(factory => factory.isApplicableFor(productURI)) match {
      case Some(factory) =>
        Try {
          factory.create(productURI, orderData, docPath).get
        }
      case _ =>
        Failure(new Exception(s"no ProductOrderFactory found for $productURI"))
    }
  }
}
