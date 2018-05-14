package domain.products.mmls

import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.MMLS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, ParticipationPools}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class MmlsGamingProductOrder(
  bets: Seq[MmlsBet],
  participationPools: MmlsParticipationPools,
  variant: Option[String] = None,
  json: JsObject
) extends GamingProductOrder {
  override def productURI: URI = MmlsGamingProductOrder.productURI

  override def withEmptyJson(): MmlsGamingProductOrder = copy(json = JsObject(Seq.empty))
}

object MmlsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(MMLS.id)
}

case class MmlsBet(numbers: Seq[Int], megaBall: Int) extends Bet

case class MmlsParticipationPools(
  firstDate: LocalDate,
  drawDays: Set[DayOfWeek],
  drawCount: Int
) extends ParticipationPools {

  def drawDates: Seq[LocalDate] = {
    Iterator.iterate(firstDate)(_.plusDays(1))
      .filter(drawDate => drawDays(drawDate.getDayOfWeek))
      .take(drawCount)
      .toVector
  }

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(MMLS.id, _))(breakOut)

}
