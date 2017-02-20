package domain.products.amls

import java.net.URI
import java.time.LocalDate

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.AMLS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, ParticipationPools}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class AmlsGamingProductOrder(
  bets: Seq[AmlsBet],
  participationPools: AmlsParticipationPools,
  variant: Option[String] = None,
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = AmlsGamingProductOrder.productURI
  override def withEmptyJson(): AmlsGamingProductOrder = copy(json = JsObject(Seq.empty))
}

object AmlsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(AMLS.id)
}

case class AmlsBet(numbers: Seq[Int]) extends Bet

case class AmlsParticipationPools(
  firstDate: LocalDate,
  drawCount: Int
) extends ParticipationPools {

  def drawDates: Seq[LocalDate] = Iterable.iterate(firstDate, drawCount)(_.plusWeeks(1)).toVector

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(AMLS.id, _))(breakOut)

}