package model

import domain.PoolResource
import domain.PoolValidator.CheckResult

/**
  * Basic trait for Items to be displayed in the `ValidationView`.
  **/
sealed trait ValidationViewItem {

  /** Indicates the structural level of the item (e.g. used for indentation). */
  def level: Int
}

case class ValidationViewStateItem(result: CheckResult, level: Int) extends ValidationViewItem

case class ValidationViewStructuralItem(result: Option[CheckResult],
                                        resource: PoolResource.PRType.Value,
                                        name: String,
                                        level: Int) extends ValidationViewItem

case class ValidationViewFilterOptions(showValidOrders: Boolean, showInvalidOrders: Boolean)


/** Enum describing the state of the `ValidationView`. */
object ValidationViewState extends Enumeration {
  val NoArchiveLoaded,
  LoadingArchive,
  ItemNotYetValidated,
  ValidationInProgress,
  PreparingData,
  ValidationResultsAvailable,
  NoItemSelected,
  NoResultsDueToFiltering = Value
}
