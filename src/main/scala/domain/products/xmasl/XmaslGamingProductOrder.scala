package domain.products.xmasl


import java.net.URI
import java.time.LocalDate

import domain.products.GamingProduct.gamingProductIdToURI
import domain.products.ML24GamingProduct.XMASL
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, ParticipationPools}
import play.api.libs.json.JsObject

import scala.collection.breakOut


case class XmaslGamingProductOrder(
  bets: Seq[XmaslBet],
  participationPools: XmaslParticipationPools,
  variant: Option[String],
  json : JsObject
) extends GamingProductOrder {
  override def productURI: URI = XmaslGamingProductOrder.productURI
  override def withEmptyJson(): XmaslGamingProductOrder = copy(json = JsObject(Seq.empty))
}


object XmaslGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(XMASL.id)
}


case class XmaslBet(numbers: Seq[Int]) extends Bet



case class XmaslParticipationPools(firstDate: LocalDate) extends ParticipationPools {

  def drawDates: Seq[LocalDate] = Vector(firstDate)
  
  def drawCount : Int = 1

  override def ids: Set[ParticipationPoolId] = drawDates.map(formatParticipationPoolId(XMASL.id, _))(breakOut)
}
