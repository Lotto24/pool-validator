package domain

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}
import java.time.{DayOfWeek, LocalDate, ZonedDateTime}
import java.util.Base64

import _root_.util.Utils
import domain.Order.Metadata
import domain.OrderDocumentsParser.ProductOrderFactory
import domain.OrderMetadata.{DrawHedgingData, OrderHedgingData}
import domain.PoolMetadata.PoolDigest
import domain.products.GamingProduct.{GamingProductId, gamingProductIdFromURI}
import domain.products.amls.{AmlsGamingProductOrder, AmlsProductOrderFactory}
import domain.products.aols.{AolsGamingProductOrder, AolsProductOrderFactory}
import domain.products.apls.{AplsGamingProductOrder, AplsProductOrderFactory}
import domain.products.asls.{AslsGamingProductOrder, AslsProductOrderFactory}
import domain.products.awls.{AwlsGamingProductOrder, AwlsProductOrderFactory}
import domain.products.c4ls.{C4lsGamingProductOrder, C4lsProductOrderFactory}
import domain.products.ejs.{EjsGamingProductOrder, EjsProductOrderFactory}
import domain.products.ems.{EmsGamingProductOrder, EmsProductOrderFactory}
import domain.products.emsplus.{EmsPlusGamingProductOrder, EmsPlusProductOrderFactory}
import domain.products.fls.{FlsGamingProductOrder, FlsProductOrderFactory}
import domain.products.gls.{GlsGamingProductOrder, GlsProductOrderFactory}
import domain.products.glss.{GlsSGamingProductOrder, GlsSProductOrderFactory}
import domain.products.irishraffle.{IrishRaffleGamingProductOrder, IrishRaffleProductOrderFactory}
import domain.products.irls.p1.{IrlsP1GamingProductOrder, IrlsP1ProductOrderFactory}
import domain.products.irls.p2.{IrlsP2GamingProductOrder, IrlsP2ProductOrderFactory}
import domain.products.irls.{IrlsGamingProductOrder, IrlsProductOrderFactory}
import domain.products.keno.{KenoGamingProductOrder, KenoProductOrderFactory}
import domain.products.mmls.{MmlsGamingProductOrder, MmlsProductOrderFactory}
import domain.products.pls.{PlsGamingProductOrder, PlsProductOrderFactory}
import domain.products.plus5.{Plus5GamingProductOrder, Plus5ProductOrderFactory}
import domain.products.s6.{S6GamingProductOrder, S6ProductOrderFactory}
import domain.products.s77.{S77GamingProductOrder, S77ProductOrderFactory}
import domain.products.sls.{SlsGamingProductOrder, SlsProductOrderFactory}
import domain.products.ukls.{UklsGamingProductOrder, UklsProductOrderFactory}
import domain.products.uktbls.{UktblsGamingProductOrder, UktblsProductOrderFactory}
import domain.products.uspbls.{UspblsGamingProductOrder, UspblsProductOrderFactory}
import domain.products.xmasl.{XmaslGamingProductOrder, XmaslProductOrderFactory}
import domain.products.{GamingProductOrder, ML24GamingProduct, ParticipationPools}
import org.bouncycastle.tsp.TimeStampResponse
import play.api.libs.json._

import scala.collection.immutable.HashMap
import scala.util.{Failure, Success, Try}

trait OrderDocumentsParser {

  def parseOrder(jsonBytes: scala.IndexedSeq[Byte], filePath: Path): Try[Order]

  def parseOrderResult(jsonBytes: scala.IndexedSeq[Byte], filePath: Path): Try[OrderResult]

  def parseOrderResultSignature(jsonBytes: scala.IndexedSeq[Byte], filePath: Path): Try[OrderResultSignature]

  def parseOrderResultSignatureTimestamp(base64Bytes: scala.IndexedSeq[Byte], filePath: Path): Try[OrderResultSignatureTimestamp]

  def parseOrderMetadata(jsonBytes: scala.IndexedSeq[Byte], filePath: Path): Try[OrderMetadata]

  def parseOrderSignature(jsonBytes: scala.IndexedSeq[Byte], filePath: Path): Try[OrderSignature]

  def parsePoolData(jsonBytes: scala.IndexedSeq[Byte], filePath: Path): Try[PoolMetadata]

  def parsePoolDigestTimestamp(data: scala.IndexedSeq[Byte], filePath: Path): Try[PoolDigestTimestamp]

}

object OrderDocumentsParser {

  /**
    * Trait for factories that create `GamingProductOrder`-instances.
    * `Order`-documents may contain orders for different products which may also have different json-representations.
    * An `OrderDocReader` needs an appropriate `ProductOrderFactory` to instantiate a concrete `GamingProductOrder`.
    **/
  trait ProductOrderFactory {

    def create(productURI: URI, orderData: JsObject, docPath: Path): Try[GamingProductOrder]

  }

}


class OrderDocumentsParserPlayImpl(productOrderFactory: ProductOrderFactory) extends OrderDocumentsParser {

  override def parseOrder(jsonBytes: scala.IndexedSeq[Byte], filePath: Path): Try[Order] = Try {
    assert(filePath.getNameCount >= 2, s"parseOrder(): provided path must have a length >= 2 (path: '${filePath.getFileName}')")
    val node = Json.parse(jsonBytes.toArray)
    val retailerHref = (node \ "metadata" \ "retailer" \ "href").as[String]
    val retailer = Symbol(Try(Paths.get(retailerHref).getFileName.toString).toOption.getOrElse("unknown retailer"))
    val metaData = Metadata(
      retailerHref = retailerHref,
      retailer = retailer,
      retailCustomerId = (node \ "metadata" \ "retail-customer").as[String],
      origin = (node \ "metadata" \ "origin").asOpt[String].map(Symbol(_)),
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

    val directoryName = filePath.getParent.getFileName.toString
    Order(metaData = metaData, gamingProductOrders = gamingProductOrders,
      docPath = filePath, orderId = directoryName, rawData = jsonBytes)
  }

  override def parseOrderResultSignature(jsonBytes: scala.IndexedSeq[Byte], filePath: Path): Try[OrderResultSignature] = Try {
    val node = Json.parse(jsonBytes.toArray)
    OrderResultSignature(
      keyId = (node \ "keyId").as[String],
      algorithm = (node \ "algorithm").as[String],
      signature = (node \ "signature").as[String].getBytes(StandardCharsets.UTF_8),
      filePath,
      rawData = jsonBytes
    )
  }

  override def parseOrderResultSignatureTimestamp(base64Bytes: scala.IndexedSeq[Byte], filePath: Path): Try[OrderResultSignatureTimestamp] = Try {
    val data = Base64.getDecoder.decode(base64Bytes.toArray)
    val tsResponse = Try {
      new TimeStampResponse(data)
    }
    OrderResultSignatureTimestamp(value = new String(base64Bytes.toArray, StandardCharsets.UTF_8),
      timeStampResponse = tsResponse,
      docPath = filePath,
      rawData = base64Bytes
    )
  }

  override def parseOrderResult(jsonBytes: scala.IndexedSeq[Byte], filePath: Path): Try[OrderResult] = Try {
    val node = Json.parse(jsonBytes.toArray)
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
      docPath = filePath,
      rawData = jsonBytes
    )
  }

  override def parseOrderMetadata(jsonBytes: scala.IndexedSeq[Byte], filePath: Path): Try[OrderMetadata] = Try {
    val node = Json.parse(jsonBytes.toArray)
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
    OrderMetadata(OrderHedgingData(hedgingDataPerProduct), filePath, jsonBytes)
  }

  override def parseOrderSignature(jsonBytes: scala.IndexedSeq[Byte], filePath: Path): Try[OrderSignature] = Try {
    val node = Json.parse(jsonBytes.toArray)
    OrderSignature(keyId = (node \ "keyId").as[String],
      algorithm = (node \ "algorithm").as[String],
      signature = (node \ "signature").as[String].getBytes(StandardCharsets.UTF_8),
      docPath = filePath,
      rawData = jsonBytes
    )
  }

  override def parsePoolData(jsonBytes: scala.IndexedSeq[Byte], filePath: Path): Try[PoolMetadata] = Try {
    val node = Json.parse(jsonBytes.toArray)
    val productUri = new URI((node \ "gaming-product").as[String])
    val poolIdStr = (node \ "participation-pool-id").as[String]
    val (productId: GamingProductId, drawDate: LocalDate) = ParticipationPools.parseParticipationPoolId(poolIdStr)
    if (!ML24GamingProduct.knownProductIds.contains(productId))
      throw new Exception(s"unexpected productId: $productId")

    val digestData: Option[PoolDigest] = {
      (node \ "participation-pool-digest").toOption match {
        case None | Some(play.api.libs.json.JsNull) => None
        case _ =>
          val data = (node \ "participation-pool-digest" \ "base64").as[String].getBytes(StandardCharsets.UTF_8)
          val algorithm = (node \ "participation-pool-digest" \ "algorithm").as[String]
          Some(PoolMetadata.PoolDigest(base64 = data, algorithm = algorithm))
      }
    }
    PoolMetadata(docPath = filePath,
      gamingProduct = productUri,
      participationPoolId = poolIdStr,
      productId = productId,
      drawDate = ZonedDateTime.parse((node \ "draw-time").as[String]),
      poolDigest = digestData,
      rawData = jsonBytes
    )
  }

  override def parsePoolDigestTimestamp(data: scala.IndexedSeq[Byte], filePath: Path): Try[PoolDigestTimestamp] = Try {
    PoolDigestTimestamp(value = new String(data.toArray, StandardCharsets.UTF_8),
      docPath = filePath, rawData = data)
  }

}



abstract class ProductOrderFactoryAI[B, P, O <: GamingProductOrder] extends ProductOrderFactory {

  /** Intermediate object for reading values of a `ParticipationPoolsMultiplyDays` */
  protected case class ParticipationPoolsMultipleDaysData(firstDate: LocalDate, drawCount: Int, drawDays: Set[DayOfWeek])

  /** Intermediate object for reading values of a `ParticipationPools` */
  protected case class ParticipationPoolsSingleDayData(firstDate: LocalDate, drawCount: Int)

  implicit val weekdaysReads = new Reads[Set[DayOfWeek]]{
    override def reads(json: JsValue): JsResult[Set[DayOfWeek]] = {
      Try(json.as[JsArray].value.map(_.as[String]).map(Utils.dayOfWeekFromString(_).get).toSet) match {
        case Success(daysOfWeek) => JsSuccess(daysOfWeek)
        case Failure(t) => JsError(t.getMessage)
      }
    }
  }

  def create(productURI: URI, orderData: JsObject, docPath: Path): Try[GamingProductOrder] = {
    Try {
      val bets = parseBets((orderData \ "bets").as[JsArray].value.asInstanceOf[Seq[JsObject]])
      val partPools = parseParticipationPools((orderData \ "participation-pools").as[JsObject])
      val variant: Option[String] = (orderData \ "variant").toOption.flatMap {
        case JsNull => None
        case x => Some(x.as[String])
      }
      createOrder(bets, partPools, variant, orderData).asInstanceOf[GamingProductOrder]
    }
  }

  protected def createOrder(bets: Seq[B], pools: P, variant: Option[String], json: JsObject) : O

  protected def parseBets(bets: Seq[JsObject]): Seq[B]

  protected def parseParticipationPools(pools: JsObject): P

  /** Facilitates creation of concrete `ParticipationPoolsMultiplyDaysValues`-instances by using an intermediate data object.*/
  protected def fromIntermediateMultiDayPoolsData(pools: JsObject)( factoryFnct: ParticipationPoolsMultipleDaysData => P) : P = {
    val intermediate = ParticipationPoolsMultipleDaysData(
      firstDate = (pools \ "first-date").as[LocalDate],
      drawDays = (pools \ "draw-days").as[Set[DayOfWeek]],
      drawCount = (pools \ "draw-count").as[Int]
    )
    factoryFnct(intermediate)
  }

  /** Facilitates creation of concrete `ParticipationPools`-instances by using an intermediate data object.*/
  protected def fromIntermediateSingleDayPoolsData(pools: JsObject)( factoryFnct: ParticipationPoolsSingleDayData => P) : P = {
    val intermediate = ParticipationPoolsSingleDayData(
      firstDate = (pools \ "first-date").as[LocalDate],
      drawCount = (pools \ "draw-count").as[Int]
    )
    factoryFnct(intermediate)
  }

}


/**
  * Combines several `ProductOrderFactory` instances to allow the processing of multi-product orders.
  **/
class CompositeProductOrderFactory(productOrderFactories: Map[URI, ProductOrderFactory]) extends ProductOrderFactory {

  def create(productURI: URI, orderData: JsObject, docPath: Path): Try[GamingProductOrder] = {
    productOrderFactories.get(productURI) match {
      case Some(factory) =>
        Try {
          factory.create(productURI, orderData, docPath).get
        }
      case _ =>
        Failure(new Exception(s"no ProductOrderFactory found for $productURI"))
    }
  }
}


object CompositeProductOrderFactory {

  /**
    * Delivers a `ProductOrderFactory` which is capable to create `ProductOrder`s for all known `GamingProductOrder`s.
    **/
  val allProductsOrderFactory: ProductOrderFactory = new CompositeProductOrderFactory(
    HashMap(
      AmlsGamingProductOrder.productURI -> new AmlsProductOrderFactory,
      AolsGamingProductOrder.productURI -> new AolsProductOrderFactory,
      AplsGamingProductOrder.productURI -> new AplsProductOrderFactory,
      AslsGamingProductOrder.productURI -> new AslsProductOrderFactory,
      AwlsGamingProductOrder.productURI -> new AwlsProductOrderFactory,
      C4lsGamingProductOrder.productURI -> new C4lsProductOrderFactory,
      EjsGamingProductOrder.productURI -> new EjsProductOrderFactory,
      EmsGamingProductOrder.productURI -> new EmsProductOrderFactory,
      EmsPlusGamingProductOrder.productURI -> new EmsPlusProductOrderFactory,
      FlsGamingProductOrder.productURI -> new FlsProductOrderFactory,
      GlsGamingProductOrder.productURI -> new GlsProductOrderFactory,
      GlsSGamingProductOrder.productURI -> new GlsSProductOrderFactory,
      IrishRaffleGamingProductOrder.productURI -> new IrishRaffleProductOrderFactory,
      IrlsGamingProductOrder.productURI -> new IrlsProductOrderFactory,
      IrlsP1GamingProductOrder.productURI -> new IrlsP1ProductOrderFactory,
      IrlsP2GamingProductOrder.productURI -> new IrlsP2ProductOrderFactory,
      KenoGamingProductOrder.productURI -> new KenoProductOrderFactory,
      MmlsGamingProductOrder.productURI -> new MmlsProductOrderFactory,
      PlsGamingProductOrder.productURI -> new PlsProductOrderFactory,
      Plus5GamingProductOrder.productURI -> new Plus5ProductOrderFactory,
      S6GamingProductOrder.productURI -> new S6ProductOrderFactory,
      S77GamingProductOrder.productURI -> new S77ProductOrderFactory,
      SlsGamingProductOrder.productURI -> new SlsProductOrderFactory,
      UklsGamingProductOrder.productURI -> new UklsProductOrderFactory,
      UktblsGamingProductOrder.productURI -> new UktblsProductOrderFactory,
      UspblsGamingProductOrder.productURI -> new UspblsProductOrderFactory,
      XmaslGamingProductOrder.productURI -> new XmaslProductOrderFactory
    ))
}
