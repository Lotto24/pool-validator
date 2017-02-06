package domain.products.irls.p1


import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.IRLSP1
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.irls.IrlsBet
import domain.products.{GamingProductOrder, _}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class IrlsP1GamingProductOrder(
  bets: Seq[IrlsBet],
  participationPools: IrlsP1ParticipationPools,
  variant: Option[String],
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = IrlsP1GamingProductOrder.productURI
  override def withEmptyJson(): IrlsP1GamingProductOrder = copy(json = JsObject(Seq.empty))
}


object IrlsP1GamingProductOrder {
  val productURI: URI = gamingProductIdToURI(IRLSP1.id)
}


case class IrlsP1ParticipationPools(
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

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(IRLSP1.id, _))(breakOut)

}
