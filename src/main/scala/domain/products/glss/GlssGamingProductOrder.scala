package domain.products.glss

import java.net.URI
import java.time.LocalDate

import domain.products.GamingProduct._
import domain.products.ML24GamingProduct.GLSS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, ParticipationPools}
import play.api.libs.json.JsObject


case class GlsSBet(numbers: Seq[Int]) extends Bet


case class GlsSGamingProductOrder(
  bets: Seq[GlsSBet],
  participationPools: GlsSParticipationPools,
  variant: Option[String],
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = GlsSGamingProductOrder.productURI
  override def withEmptyJson(): GlsSGamingProductOrder = copy(json = JsObject(Seq.empty))
}


object GlsSGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(GLSS.id)
}


case class GlsSParticipationPools(
  firstDate: LocalDate,
  drawCount: Int
) extends ParticipationPools {

  override def ids: Set[ParticipationPoolId] = {
    Iterable.iterate(firstDate, drawCount)(_.plusWeeks(1)).map(formatParticipationPoolId(GLSS.id, _)).toSet
  }

}

