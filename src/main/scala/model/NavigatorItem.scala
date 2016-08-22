package model

import java.nio.file.Path

import domain.PoolValidator.CheckResult
import domain.products.GamingProduct._
import model.ApplicationModel.{PoolSource, ValidationState}
import model.OrderDirNavigatorItem.ProductInfos

/**
  * Basic trait for `NavigatorItem` in the Pool Navigator's `TreeView`.
  */
trait NavigatorItem {

  def path: Path

  def displayName: String

  def validationResults: IndexedSeq[CheckResult]

  /** @return None incidates that validation has not yet been applied. */
  def isValid: Option[Boolean]

  /** Creates a copy with updated `validationResults` */
  def withValidationResults(validationResults: IndexedSeq[CheckResult]): NavigatorItem

  def withoutValidationResults: NavigatorItem
}


/** @param path The directory where the `poolSource` is physically located (e.g. where a `PoolSourceArchive`
  *             has been extracted to)
  * */
case class ArchiveNavigatorItem(path: Path,
                                poolSource: PoolSource,
                                displayName: String,
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

case class OrderDirNavigatorItem(path: Path,
                                 displayName: String,
                                 retailerOrderReference: String,
                                 validationState: ValidationState.Value,
                                 hasInvalidOrderDocs: Boolean,
                                 validationResults: IndexedSeq[CheckResult] = IndexedSeq.empty,
                                 productInfos: ProductInfos) extends NavigatorItem {
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



case class OrderDocNavigatorItem(path: Path,
                                 displayName: String,
                                 validationResults: IndexedSeq[CheckResult] = IndexedSeq.empty) extends NavigatorItem {

  override def withValidationResults(validationResults: IndexedSeq[CheckResult]): OrderDocNavigatorItem = {
    this.copy(validationResults = validationResults)
  }

  override def withoutValidationResults: OrderDocNavigatorItem = copy(validationResults = IndexedSeq.empty)

  override def isValid: Option[Boolean] = if (validationResults.isEmpty) None else Some(validationResults.forall(_.isOk))
}


/**
  * Options used to filter the content of the `NavigatorView`.
  * */
case class NavigatorFilterOptions(showValidOrders: Boolean,
                                  showInvalidOrders: Boolean,
                                  showUnvalidatedOrders: Boolean,
                                  textFilter: Option[String])