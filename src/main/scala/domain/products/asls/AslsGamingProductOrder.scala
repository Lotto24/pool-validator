package domain.products.asls

import java.net.URI
import java.time.LocalDate

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.ASLS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, ParticipationPools, _}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class AslsGamingProductOrder(
  bets: Seq[AslsBet],
  participationPools: AslsParticipationPools,
  variant: Option[String] = None,
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = AslsGamingProductOrder.productURI
  override def withEmptyJson(): AslsGamingProductOrder = copy(json = JsObject(Seq.empty))
}

object AslsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(ASLS.id)
}


case class AslsBet(numbers: Seq[Int]) extends Bet


case class AslsParticipationPools(
  firstDate: LocalDate,
  drawCount: Int
) extends ParticipationPools {

  def drawDates: Seq[LocalDate] = Iterable.iterate(firstDate, drawCount)(_.plusWeeks(1)).toVector

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(ASLS.id, _))(breakOut)

}
