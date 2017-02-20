package domain.products.ejs

import java.net.URI
import java.time.LocalDate

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.EJS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products._
import play.api.libs.json.JsObject

/**
  * Represents an Eurojackpot secondary lottery order.
  * */
case class EjsGamingProductOrder(
  bets: Seq[EjsBet], 
  participationPools: EjsParticipationPools, 
  variant: Option[String], 
  json : JsObject=null) extends GamingProductOrder {
  override def productURI: URI = EjsGamingProductOrder.productURI
  override def withEmptyJson(): EjsGamingProductOrder = copy(json = JsObject(Seq.empty))
}

object EjsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(EJS.id)
}

case class EjsBet(numbers: Seq[Int], euroNumbers: Seq[Int]) extends Bet


case class EjsParticipationPools(firstDate: LocalDate, drawCount: Int) extends ParticipationPools {

  def ids: Set[ParticipationPoolId] = {
    Iterable.iterate(firstDate, drawCount)(_.plusWeeks(1)).map(formatParticipationPoolId(EJS.id, _)).toSet
  }

}
