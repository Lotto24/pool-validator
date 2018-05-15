package domain

import domain.PoolResource.{Filenames, PRType}
import enumeratum.{Enum, EnumEntry}

import scala.collection.immutable

/** Base trait for descriptions of checks that can be executed to validate a participation pool archive.
  * There are sub-traits `OrderCheck` and `PoolSealCheck`. */
sealed trait PoolArchiveCheck {

  def description: String

  def affectedResources: Set[PRType.Value]

}

object Check {
  val allChecks: IndexedSeq[PoolArchiveCheck] = OrderCheck.values ++ PoolSealCheck.values
}

/**
  * Description class for checks concerned to a single order.
  **/
sealed trait OrderCheck extends PoolArchiveCheck with EnumEntry

object OrderCheck extends Enum[OrderCheck] {

  case object AllOrderDocumentsMustExistCheck extends OrderCheck {
    override val description: String = "All order documents must exist"
    override val affectedResources: Set[PRType.Value] = Set(PRType.OrderDirectory, PRType.Order, PRType.OrderSignature,
      PRType.OrderResult, PRType.OrderResultSignature, PRType.OrderResultSignatureTimestamp)
  }

  case object RetailerOrderSignatureCheck extends OrderCheck {
    override val description: String = "Retailer order signature must be valid"
    override val affectedResources: Set[PRType.Value] = Set(PRType.Order, PRType.OrderSignature)
  }

  case object PoolParticipationCheck extends OrderCheck {
    override val description: String = "Order must participate in pool"
    override val affectedResources: Set[PRType.Value] = Set(PRType.Order, PRType.MetaData)
  }

  case object CheckOrderResultOperatorSignature extends OrderCheck {
    override val description: String = "Order document must match retailer order"
    override val affectedResources: Set[PRType.Value] = Set(PRType.Order, PRType.OrderResult)
  }

  case object OrderAcceptedCheck extends OrderCheck {
    override val description: String = "Order must be accepted"
    override val affectedResources: Set[PRType.Value] = Set(PRType.OrderResult)
  }

  case object OrderResultSignatureValidCheck extends OrderCheck {
    override val description: String = "Order document signature must be valid"
    override val affectedResources: Set[PRType.Value] = Set(PRType.OrderResult, PRType.OrderResultSignature)
  }

  case object OrderResultSignatureTimestampAuthCheck extends OrderCheck {
    override val description: String = "Order document signature timestamp must be authentic"
    override val affectedResources: Set[PRType.Value] = Set(PRType.OrderResultSignatureTimestamp)
  }

  case object OrderResultSignatureTimestampResultSignatureCheck extends OrderCheck {
    override val description: String = "Order document signature timestamp must match order document signature"
    override val affectedResources: Set[PRType.Value] = Set(PRType.OrderResultSignature, PRType.OrderResultSignatureTimestamp)
  }

  case object OrderResultSignatureTimestampBeforeDrawTimeCheck extends OrderCheck {
    override val description: String = "Order document signature timestamp must be created before draw time"
    override val affectedResources: Set[PRType.Value] = Set(PRType.OrderResultSignatureTimestamp)
  }

  override lazy val values: immutable.IndexedSeq[OrderCheck] = findValues

}


/**
  * Description class for checks concerned to the participation pool.
  **/
sealed trait PoolSealCheck extends PoolArchiveCheck with EnumEntry

object PoolSealCheck extends Enum[PoolSealCheck] {

  // pool validation checks
  case object PoolDigestCheck extends PoolSealCheck {
    override val description: String = "The participation pool digest must match the included orders"
    override val affectedResources: Set[PRType.Value] = Set(PRType.MetaData, PRType.PoolDirectory)
  }

  case object PoolDigestTimestampExistsCheck extends PoolSealCheck {
    override val description: String = s"Document ${Filenames.PoolDigestTimestamp} must exist"
    override val affectedResources: Set[PRType.Value] = Set(PRType.PoolDigestTimestamp)
  }

  case object PoolDigestTimestampAuthCheck extends PoolSealCheck {
    override val description: String = "The participation pool digest's timestamp must be authentic"
    override val affectedResources: Set[PRType.Value] = Set(PRType.PoolDigestTimestamp)
  }

  case object PoolDigestTimestampResultSignatureCheck extends PoolSealCheck {
    override val description: String = "The participation pool digest's timestamp must match the pool digest"
    override val affectedResources: Set[PRType.Value] = Set(PRType.PoolDigestTimestamp)
  }

  case object PoolDigestTimestampBeforeDrawTimeCheck extends PoolSealCheck {
    override val description: String = "The participation pool digest's timestamp must be created before draw time"
    override val affectedResources: Set[PRType.Value] = Set(PRType.PoolDigestTimestamp)
  }

  override lazy val values: immutable.IndexedSeq[PoolSealCheck] = findValues

}
