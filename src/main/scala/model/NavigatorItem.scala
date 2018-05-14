package model

import java.nio.file.Path

import domain.OrderId
import domain.PoolValidator.CheckResult
import domain.products.GamingProduct._
import model.ApplicationModel.{PoolSource, ValidationState}
import model.OrderDirNavigatorItem.ProductInfos

/**
  * Basic trait for `NavigatorItem` in the Pool Navigator's `TreeView`.
  */
trait NavigatorItem {

  def relativePath: Path

  def displayName: String

  def validationResults: IndexedSeq[CheckResult]

  /** @return None incidates that validation has not yet been applied. */
  def isValid: Option[Boolean]

  /** Creates a copy with updated `validationResults` */
  def withValidationResults(validationResults: IndexedSeq[CheckResult]): NavigatorItem

  def withoutValidationResults: NavigatorItem
}


case class ArchiveNavigatorItem(
  relativePath: Path,
  displayName: String,
  poolSource: PoolSource,
  isValid: Option[Boolean],
  validationState: ValidationState.Value,
  validationResults: IndexedSeq[CheckResult] = IndexedSeq.empty) extends NavigatorItem {
  
  override def withValidationResults(validationResults: IndexedSeq[CheckResult]): ArchiveNavigatorItem = {
    this.copy(validationResults = validationResults)
  }

  override def withoutValidationResults: ArchiveNavigatorItem = copy(isValid = None,
    validationState = ValidationState.NotValidated,
    validationResults = IndexedSeq.empty)
}

case class OrderDirNavigatorItem(
  relativePath: Path,
  retailerOrderReference: String,
  validationState: ValidationState.Value,
  hasInvalidOrderDocs: Boolean,
  validationResults: IndexedSeq[CheckResult] = IndexedSeq.empty,
  productInfos: Option[ProductInfos]) extends NavigatorItem {

  override def displayName: String = orderId
  
  def orderId: OrderId = relativePath.getFileName.toString
  
  override def withValidationResults(validationResults: IndexedSeq[CheckResult]): OrderDirNavigatorItem = {
    this.copy(validationResults = validationResults)
  }

  override def withoutValidationResults: OrderDirNavigatorItem = copy(validationState = ValidationState.NotValidated,
    validationResults = IndexedSeq.empty, hasInvalidOrderDocs = false)

  override def isValid: Option[Boolean] = {
    if (validationState == ValidationState.NotValidated) None
    else {
      Some(!hasInvalidOrderDocs && validationResults.forall(_.isOk))
    }
  }
}

object OrderDirNavigatorItem {

  case class ProductInfos(betCountPerProduct: Map[GamingProductId, Int])

}


case class OrderDocNavigatorItem(relativePath: Path, validationResults: IndexedSeq[CheckResult] = IndexedSeq.empty) extends NavigatorItem {

  override def displayName: String = relativePath.getFileName.toString

  override def withValidationResults(validationResults: IndexedSeq[CheckResult]): OrderDocNavigatorItem = {
    this.copy(validationResults = validationResults)
  }

  override def withoutValidationResults: OrderDocNavigatorItem = copy(validationResults = IndexedSeq.empty)

  override def isValid: Option[Boolean] = if (validationResults.isEmpty) None else Some(validationResults.forall(_.isOk))
}


/**
  * Options used to filter the content of the `NavigatorView`.
  **/
case class NavigatorFilterOptions(showValidOrders: Boolean,
  showInvalidOrders: Boolean,
  showUnvalidatedOrders: Boolean,
  textFilter: Option[String])