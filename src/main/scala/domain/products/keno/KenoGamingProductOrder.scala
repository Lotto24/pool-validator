package domain.products.keno

import java.net.URI
import java.time.LocalDate

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.KENO
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, ParticipationPools}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class KenoGamingProductOrder(
  bets: Seq[KenoBet],
  participationPools: KenoParticipationPools,
  variant: Option[String] = None,
  json: JsObject
) extends GamingProductOrder {
  override def productURI: URI = KenoGamingProductOrder.productURI

  override def withEmptyJson(): KenoGamingProductOrder = copy(json = JsObject(Seq.empty))
}

object KenoGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(KENO.id)
}

case class KenoBet(numbers: Seq[Int], stake: Int) extends Bet

case class KenoParticipationPools(
  firstDate: LocalDate,
  drawCount: Int
) extends ParticipationPools {

  def drawDates: Seq[LocalDate] = {
    Iterator.iterate(firstDate)(_.plusDays(1))
      .take(drawCount)
      .toVector
  }

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(KENO.id, _))(breakOut)

}
