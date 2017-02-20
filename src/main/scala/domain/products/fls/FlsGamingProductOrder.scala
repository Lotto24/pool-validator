package domain.products.fls

import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.FLS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, _}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class FlsGamingProductOrder(
  bets: Seq[FlsBet],
  participationPools: FlsParticipationPools,
  variant: Option[String] = None,
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = FlsGamingProductOrder.productURI
  override def withEmptyJson(): FlsGamingProductOrder = copy(json = JsObject(Seq.empty))
}


object FlsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(FLS.id)
}


case class FlsBet(numbers: Seq[Int], chancenumber: Int) extends Bet


case class FlsParticipationPools(
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

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(FLS.id, _))(breakOut)

}
