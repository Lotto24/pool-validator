package domain.products.ems

import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.EMS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products._
import play.api.libs.json.JsObject


case class EmsBet(numbers: Seq[Int], starnumbers: Seq[Int]) extends Bet


case class EmsParticipationPools(
  firstDate: LocalDate,
  drawDays: Set[DayOfWeek],
  drawCount: Int
) extends ParticipationPoolsMultiplyDays {

  override def ids: Set[ParticipationPoolId] = {
    Iterator.iterate(firstDate)(_.plusDays(1))
      .filter(drawDate => drawDays(drawDate.getDayOfWeek))
      .take(drawCount)
      .map(formatParticipationPoolId(EMS.id, _))
      .toSet
  }
}


case class EmsGamingProductOrder(
  bets: Seq[EmsBet],
  participationPools: EmsParticipationPools,
  variant: Option[String],
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = EmsGamingProductOrder.productURI
  override def withEmptyJson(): EmsGamingProductOrder = copy(json = JsObject(Seq.empty))
}


object EmsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(EMS.id)
}
