package domain.products.awls

import java.net.URI
import java.time.LocalDate

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.AWLS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, ParticipationPools, _}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class AwlsGamingProductOrder(
  bets: Seq[AwlsBet],
  participationPools: AwlsParticipationPools,
  variant: Option[String] = None,
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = AwlsGamingProductOrder.productURI
  override def withEmptyJson(): AwlsGamingProductOrder = copy(json = JsObject(Seq.empty))
}

object AwlsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(AWLS.id)
}

case class AwlsBet(numbers: Seq[Int]) extends Bet

case class AwlsParticipationPools(
  firstDate: LocalDate,
  drawCount: Int
) extends ParticipationPools {

  def drawDates: Seq[LocalDate] = Iterable.iterate(firstDate, drawCount)(_.plusWeeks(1)).toVector

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(AWLS.id, _))(breakOut)

}
