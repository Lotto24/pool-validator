package domain.products.pls


import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.PLS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, _}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class PlsGamingProductOrder(
  bets: Seq[PlsBet],
  participationPools: PlsParticipationPools,
  variant: Option[String],
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = PlsGamingProductOrder.productURI
  override def withEmptyJson(): PlsGamingProductOrder = copy(json = JsObject(Seq.empty))
}


object PlsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(PLS.id)
}


case class PlsBet(numbers: Seq[Int]) extends Bet


case class PlsParticipationPools(
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

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(PLS.id, _))(breakOut)

}
