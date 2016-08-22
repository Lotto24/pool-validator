package domain.products.s77

import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct._
import domain.products.ML24GamingProduct.S77
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, ParticipationPools}
import play.api.libs.json.JsObject


case class S77Bet(
  numbers: Set[Int]
) extends Bet


case class S77GamingProductOrder(
  bets: Seq[S77Bet],
  participationPools: S77ParticipationPools,
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = S77GamingProductOrder.productURI
  override def withEmptyJson(): S77GamingProductOrder = copy(json = JsObject(Seq.empty))
}


object S77GamingProductOrder {
  val productURI: URI = gamingProductIdToURI(S77.id)
}


case class S77ParticipationPools(
  firstDate: LocalDate,
  drawDays: Set[DayOfWeek],
  drawCount: Int
) extends ParticipationPools {

  override def ids: Set[ParticipationPoolId] = {
    Iterator.iterate(firstDate)(_.plusDays(1))
      .filter(drawDate => drawDays(drawDate.getDayOfWeek))
      .take(drawCount)
      .map(formatParticipationPoolId(S77.id, _))
      .toSet
  }

}