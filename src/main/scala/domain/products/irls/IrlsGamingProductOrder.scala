package domain.products.irls


import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.IRLS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, _}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class IrlsGamingProductOrder(
  bets: Seq[IrlsBet],
  participationPools: IrlsParticipationPools,
  variant: Option[String],
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = IrlsGamingProductOrder.productURI
  override def withEmptyJson(): IrlsGamingProductOrder = copy(json = JsObject(Seq.empty))
}


object IrlsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(IRLS.id)
}


case class IrlsBet(numbers: Seq[Int]) extends Bet


case class IrlsParticipationPools(
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

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(IRLS.id, _))(breakOut)

}

