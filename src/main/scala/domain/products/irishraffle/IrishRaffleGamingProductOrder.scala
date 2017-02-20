package domain.products.irishraffle


import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.IRISHRAFFLE
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, _}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class IrishRaffleGamingProductOrder(
  bets: Seq[IrishRaffleBet],
  participationPools: IrishRaffleParticipationPools,
  variant: Option[String],
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = IrishRaffleGamingProductOrder.productURI
  override def withEmptyJson(): IrishRaffleGamingProductOrder = copy(json = JsObject(Seq.empty))
}


object IrishRaffleGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(IRISHRAFFLE.id)
}


case class IrishRaffleBet(numbers: Seq[Int]) extends Bet


case class IrishRaffleParticipationPools(
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

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(IRISHRAFFLE.id, _))(breakOut)

}
