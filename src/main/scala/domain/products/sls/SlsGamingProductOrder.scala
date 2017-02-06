package domain.products.sls

import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct._
import domain.products.ML24GamingProduct.SLS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, ParticipationPoolsMultiplyDays}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class SlsGamingProductOrder(
  bets: Seq[SlsBet],
  participationPools: SlsParticipationPools,
  variant: Option[String],
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = SlsGamingProductOrder.productURI
  override def withEmptyJson(): SlsGamingProductOrder = copy(json = JsObject(Seq.empty))
}


object SlsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(SLS.id)
}


case class SlsBet(numbers: Seq[Int]) extends Bet


case class SlsParticipationPools(
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

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(SLS.id, _))(breakOut)

}
