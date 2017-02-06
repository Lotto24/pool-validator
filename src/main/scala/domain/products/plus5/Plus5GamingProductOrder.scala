package domain.products.plus5


import java.net.URI
import java.time.LocalDate

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.PLUS5
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, ParticipationPools}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class Plus5GamingProductOrder(
  bets: Seq[Plus5Bet],
  participationPools: Plus5ParticipationPools,
  variant: Option[String],
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = Plus5GamingProductOrder.productURI
  override def withEmptyJson(): Plus5GamingProductOrder = copy(json = JsObject(Seq.empty))
}


object Plus5GamingProductOrder {
  val productURI: URI = gamingProductIdToURI(PLUS5.id)
}


case class Plus5Bet(numbers: Seq[Int]) extends Bet


case class Plus5ParticipationPools(
  firstDate: LocalDate,
  drawCount: Int
) extends ParticipationPools {

  def drawDates: Seq[LocalDate] = Iterable.iterate(firstDate, drawCount)(_.plusDays(1)).toVector

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(PLUS5.id, _))(breakOut)
}
