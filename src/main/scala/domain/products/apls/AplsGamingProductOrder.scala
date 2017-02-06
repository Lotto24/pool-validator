package domain.products.apls

import java.net.URI
import java.time.LocalDate

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.APLS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, ParticipationPools}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class AplsGamingProductOrder(
  bets: Seq[AplsBet],
  participationPools: AplsParticipationPools,
  variant: Option[String] = None,
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = AplsGamingProductOrder.productURI
  override def withEmptyJson(): AplsGamingProductOrder = copy(json = JsObject(Seq.empty))
}

object AplsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(APLS.id)
}


case class AplsBet(numbers: Seq[Int], powerball: Int) extends Bet


case class AplsParticipationPools(
  firstDate: LocalDate,
  drawCount: Int
) extends ParticipationPools {

  def drawDates: Seq[LocalDate] = Iterable.iterate(firstDate, drawCount)(_.plusWeeks(1)).toVector

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(APLS.id, _))(breakOut)

}
