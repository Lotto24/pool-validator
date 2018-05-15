package domain.products.xmasl

import java.time.LocalDate

import domain.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class XmaslProductOrderFactory extends ProductOrderFactoryAI[XmaslBet, XmaslParticipationPools, XmaslGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[XmaslBet] = bets.map { bet => XmaslBet(
    numbers = (bet \ "numbers").as[Seq[Int]]
  )}

  protected def parseParticipationPools(pools: JsObject): XmaslParticipationPools = XmaslParticipationPools(
    firstDate = (pools \ "draw-date").as[LocalDate]
  )

  override protected def createOrder(bets: Seq[XmaslBet], pools: XmaslParticipationPools, variant: Option[String], json: JsObject): XmaslGamingProductOrder = {
    XmaslGamingProductOrder(bets, pools, variant, json)
  }

}