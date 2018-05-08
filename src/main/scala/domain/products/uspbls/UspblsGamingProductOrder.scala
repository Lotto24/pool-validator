package domain.products.uspbls

import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.USPBLS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, ParticipationPools}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class UspblsGamingProductOrder(
  bets: Seq[UspblsBet],
  participationPools: UspblsParticipationPools,
  variant: Option[String] = None,
  json: JsObject
) extends GamingProductOrder {
  override def productURI: URI = UspblsGamingProductOrder.productURI

  override def withEmptyJson(): UspblsGamingProductOrder = copy(json = JsObject(Seq.empty))
}

object UspblsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(USPBLS.id)
}

case class UspblsBet(numbers: Seq[Int], powerBall: Int) extends Bet

case class UspblsParticipationPools(
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

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(USPBLS.id, _))(breakOut)

}
