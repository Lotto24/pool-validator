package domain.products.ejs

import java.net.URI
import java.time.LocalDate

import domain.products.ParticipationPools.ParticipationPoolId
import domain.products._
import ML24GamingProduct.EJS
import GamingProduct.gamingProductIdToURI
import domain.products.ParticipationPools.formatParticipationPoolId
import play.api.libs.json.JsObject

/**
  * Represents an Eurojackpot secondary lottery order.
  * */
case class EjsGamingProductOrder(bets: Seq[EjsBet], participationPools: EjsParticipationPools, json : JsObject) extends GamingProductOrder {
  override def productURI: URI = EjsGamingProductOrder.productURI
  override def withEmptyJson(): EjsGamingProductOrder = copy(json = JsObject(Seq.empty))
}

object EjsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(EJS.id)
}

case class EjsBet(numbers: Set[Int], euroNumbers: Set[Int]) extends Bet


case class EjsParticipationPools(firstDate: LocalDate, drawCount: Int) extends ParticipationPools {

  def ids: Set[ParticipationPoolId] = {
    Iterable.iterate(firstDate, drawCount)(_.plusWeeks(1)).map(formatParticipationPoolId(EJS.id, _)).toSet
  }

}
