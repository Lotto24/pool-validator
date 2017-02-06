package domain.products.emsplus


import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.EMSPLUS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, _}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class EmsPlusGamingProductOrder(
  bets: Seq[EmsPlusBet],
  participationPools: EmsPlusParticipationPools,
  variant: Option[String],
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = EmsPlusGamingProductOrder.productURI
  override def withEmptyJson(): EmsPlusGamingProductOrder = copy(json = JsObject(Seq.empty))
}


object EmsPlusGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(EMSPLUS.id)
}


case class EmsPlusBet(numbers: Seq[Int]) extends Bet

case class EmsPlusParticipationPools(
  firstDate: LocalDate,
  drawDays: Set[DayOfWeek],
  drawCount: Int
) extends ParticipationPoolsMultiplyDays {

  def drawDates: Seq[LocalDate] = {
    Iterator.iterate(firstDate)(_.plusDays(1))
      .filter(drawDate => drawDays(drawDate.getDayOfWeek))
      .take(drawCount)
      .toVector
  }

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(EMSPLUS.id, _))(breakOut)
}
