package model

import java.nio.file.Path

import domain.PoolValidator.CheckResult
import domain.products.GamingProduct._
import domain.{PoolMetadata, PoolResource}
import model.ApplicationModel.{PoolSource, ValidationState}
import model.ArchiveDetailData.OrderStats
import model.OrderDirNavigatorItem.ProductInfos


/**
  * Basic trait for data to be viewed in the `DetailView`.
  * */
trait DetailData

case class OrderDocDetailData[T <: PoolResource](doc: T) extends DetailData

case class OrderDirectoryDetailData(path: Path,
                                    orderId: String,
                                    validationState: ValidationState.Value,
                                    hasInvalidOrderDocs: Boolean,
                                    validationResults: Seq[CheckResult],
                                    productInfos: ProductInfos) extends DetailData

case class ArchiveDetailData(poolSource: PoolSource,
                             poolMetadata: PoolMetadata,
                             poolDigestTimestamp: Option[IndexedSeq[Byte]],
                             orderStats: OrderStats,
                             extractedToDir: Path,
                             validationState: ValidationState.Value,
                             totalOrdersCount: Int,
                             validatedOrdersCount: Int,
                             validOrdersCount: Int,
                             invalidOrdersCount: Int) extends DetailData

object ArchiveDetailData {

  case class OrderStats(totalOrdersCount: Int, betsCountPerProduct: Option[Map[GamingProductId, Int]])

}
