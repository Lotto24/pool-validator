package model

import java.nio.file.Path
import java.time.LocalDate

import domain.OrderMetadata.OrderHedgingData
import domain.PoolValidator.CheckResult
import domain.products.{Bet, ParticipationPools}
import domain.products.GamingProduct._
import domain.products.ParticipationPools.ParticipationPoolId
import domain._
import model.ApplicationModel.{PoolSource, ValidationState}
import model.ArchiveDetailData.{BetBreakdownRowItem, OrderStats}
import model.OrderDirNavigatorItem.ProductInfos
import model.OrderHedgingDetailData.RowData
import util.Utils.ErrorMsg


/**
  * Basic trait for data to be viewed in the `DetailView`.
  * */
trait DetailData

case class OrderDocDetailData[T <: PoolResource](doc: T) extends DetailData

case class OrderHedgingDetailData(order: Order,
  orderHedgingData: OrderHedgingData,
  expandedBetListPerProduct: Map[GamingProductId, Vector[RowData]]
) extends DetailData


object OrderHedgingDetailData{

  /**
    * `RowData` for hedging-data table in `HedgingDetailPane`.
    * */
  case class RowData(productId : GamingProductId,
    poolId: ParticipationPoolId,
    variant: Option[String],
    drawDate: LocalDate,
    bet : Bet,
    hedgingChannel: String
  )

  object RowData {

    implicit val dateTimeOrdering: Ordering[LocalDate] = Ordering.fromLessThan(_ isBefore _)

    /**
      * Creates `RowData` by combining data from the order and the orderHedgingData.
      * */
    def create(order: Order, orderHedgingData: OrderHedgingData): Map[GamingProductId, Vector[RowData]] = {

      // create a list with all played bets on every draw
      val expandedBetListPerProduct: Seq[(GamingProductId, Vector[RowData])] = order.gamingProductOrders.toSeq.map { case (uri, gpo) =>

        val productId = gamingProductIdFromURI(uri)

        val singleGpoTableRows: Vector[RowData] = for {
          poolId <- gpo.participationPools.ids.toVector.sorted;
          bet <- gpo.bets
        } yield {

          val hedgingChannel: Option[String] = orderHedgingData.hedgingDataPerProduct.get(productId) match {
            case Some(drawHedgingData) =>
              drawHedgingData.find(_.poolId == poolId).flatMap(_.hedgingChannel)
            case _ => None
          }

          val (_, drawDate) = ParticipationPools.parseParticipationPoolId(poolId)

          RowData(
            productId = gamingProductIdFromURI(uri),
            poolId = poolId,
            variant = gpo.variant, 
            drawDate,
            bet,
            hedgingChannel = hedgingChannel.getOrElse("")
          )
        }
        (productId, singleGpoTableRows.sortBy(_.drawDate))
      }
      expandedBetListPerProduct.toMap
    }
  }
}



case class OrderDirectoryDetailData(path: Path,
                                    orderId: OrderId,
                                    validationState: ValidationState.Value,
                                    hasInvalidOrderDocs: Boolean,
                                    validationResults: Seq[CheckResult],
                                    productInfos: Option[ProductInfos]) extends DetailData


case class ArchiveDetailData(poolSource: PoolSource,
                             poolMetadata: PoolMetadata,
                             poolDigestTimestamp: Option[IndexedSeq[Byte]],
                             orderStats: OrderStats,
                             betBreakdownRowData: Vector[BetBreakdownRowItem],
                             extractedToDir: Path,
                             validationState: ValidationState.Value,
                             totalOrdersCount: Int,
                             validatedOrdersCount: Int,
                             validOrdersCount: Int,
                             invalidOrdersCount: Int
) extends DetailData {
  def productIds: Set[GamingProductId] = orderStats.betCountBreakdown.keySet
}



object ArchiveDetailData {

  case class OrderStats(
    totalOrdersCount: Int, 
    betCountBreakdown: Map[GamingProductId, Map[Retailer, Map[Option[Origin], Int]]],
    ordersCountBreakdown: Map[Retailer, Map[Option[Origin], Int]]
  ) {
    
    def allRetailers: Set[Retailer] = betCountBreakdown.values.flatMap(_.keys).toSet
    
    def allOrigins: Set[Option[Origin]] = betCountBreakdown.values.flatMap(x => x.get('mylotto24).map(_.keys)).toSet.flatten
    
    def originsFor(retailer: Retailer): Set[Option[Origin]] = betCountBreakdown.values.flatMap(x => x.get(retailer).map(_.keys)).toSet.flatten

    def betsCountPerProduct: Map[GamingProductId, Int] = betCountBreakdown.mapValues(_.values.map(_.values.sum).sum)

    def getOrdersCountFor(retailer: Retailer, origin: Option[Origin]): Int = {
      ordersCountBreakdown.get(retailer).flatMap(_.get(origin)).getOrElse(0)
    }
  }

  /** Row data for the breakdown `TableView`. */
  case class BetBreakdownRowItem(
    retailer: Retailer, 
    origin: Option[Origin], 
    ordersCount: Int, 
    betCountPerProduct: Map[GamingProductId, Int]
  )
}


/**
  * `DetailDataLoadingError` is a special `DetailData`-type used to propagate errors that occurred while loading the 
  *  actually requested `DetailData`.
  * */
case class DetailDataLoadingError(error: ErrorMsg) extends DetailData
