package domain.products.aols

import java.net.URI
import java.time.LocalDate

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.AOLS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, ParticipationPools}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class AolsGamingProductOrder(
  bets: Seq[AolsBet],
  participationPools: AolsParticipationPools,
  variant: Option[String] = None,
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = AolsGamingProductOrder.productURI
  override def withEmptyJson(): AolsGamingProductOrder = copy(json = JsObject(Seq.empty))
}


object AolsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(AOLS.id)
}


case class AolsBet(numbers: Seq[Int]) extends Bet


case class AolsParticipationPools(
  firstDate: LocalDate,
  drawCount: Int
) extends ParticipationPools {

  def drawDates: Seq[LocalDate] = Iterable.iterate(firstDate, drawCount)(_.plusWeeks(1)).toVector

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(AOLS.id, _))(breakOut)

}
