package domain.products.uktbls


import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.UKTBLS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, _}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class UktblsGamingProductOrder(
  bets: Seq[UktblsBet],
  participationPools: UktblsParticipationPools,
  variant: Option[String],
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = UktblsGamingProductOrder.productURI
  override def withEmptyJson(): UktblsGamingProductOrder = copy(json = JsObject(Seq.empty))
}


object UktblsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(UKTBLS.id)
}

case class UktblsBet(numbers: Seq[Int], thunderball: Int) extends Bet

case class UktblsParticipationPools(
  firstDate: LocalDate,
  drawDays: Set[DayOfWeek],
  drawCount: Int
) extends ParticipationPoolsMultiplyDays {

  def drawDates: Seq[LocalDate] = {
    Iterator.iterate(firstDate)(_.plusDays(1))
      .filter(drawDate => drawDays(drawDate.getDayOfWeek))
      .take(drawCount)
      .toVector
  }

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(UKTBLS.id, _))(breakOut)

}
