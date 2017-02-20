package domain.products.ukls

import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.UKLS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, _}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class UklsGamingProductOrder(
  bets: Seq[UklsBet],
  participationPools: UklsParticipationPools,
  variant: Option[String],
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = UklsGamingProductOrder.productURI
  override def withEmptyJson(): UklsGamingProductOrder = copy(json = JsObject(Seq.empty))
}


object UklsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(UKLS.id)
}


case class UklsBet(numbers: Seq[Int]) extends Bet


case class UklsParticipationPools(
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

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(UKLS.id, _))(breakOut)

}
