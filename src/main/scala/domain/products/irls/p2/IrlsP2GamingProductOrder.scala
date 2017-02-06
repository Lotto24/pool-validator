package domain.products.irls.p2

import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.IRLSP2
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.irls.IrlsBet
import domain.products.{GamingProductOrder, _}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class IrlsP2GamingProductOrder(
  bets: Seq[IrlsBet],
  participationPools: IrlsP2ParticipationPools,
  variant: Option[String],
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = IrlsP2GamingProductOrder.productURI
  override def withEmptyJson(): IrlsP2GamingProductOrder = copy(json = JsObject(Seq.empty))
}


object IrlsP2GamingProductOrder {
  val productURI: URI = gamingProductIdToURI(IRLSP2.id)
}


case class IrlsP2ParticipationPools(
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

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(IRLSP2.id, _))(breakOut)

}
