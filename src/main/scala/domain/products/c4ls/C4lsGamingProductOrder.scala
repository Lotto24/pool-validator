package domain.products.c4ls

import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.C4LS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, ParticipationPools}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class C4lsGamingProductOrder(
  bets: Seq[C4lsBet],
  participationPools: C4lsParticipationPools,
  variant: Option[String] = None,
  json: JsObject
) extends GamingProductOrder {
  override def productURI: URI = C4lsGamingProductOrder.productURI

  override def withEmptyJson(): C4lsGamingProductOrder = copy(json = JsObject(Seq.empty))
}

object C4lsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(C4LS.id)
}

case class C4lsBet(numbers: Seq[Int], cashBall: Int) extends Bet

case class C4lsParticipationPools(
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

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(C4LS.id, _))(breakOut)

}
