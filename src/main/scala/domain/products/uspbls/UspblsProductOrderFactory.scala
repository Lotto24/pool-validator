package domain.products.uspbls

import domain.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class UspblsProductOrderFactory extends ProductOrderFactoryAI[UspblsBet, UspblsParticipationPools, UspblsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[UspblsBet] = bets.map { bet => UspblsBet(
    numbers = (bet \ "numbers").as[Seq[Int]], powerBall = (bet \ "power-ball").as[Int]
  )}

  protected def parseParticipationPools(pools: JsObject): UspblsParticipationPools = fromIntermediateMultiDayPoolsData(pools) { data =>
    UspblsParticipationPools(data.firstDate, data.drawDays, data.drawCount)
  }

  override protected def createOrder(bets: Seq[UspblsBet], pools: UspblsParticipationPools, variant: Option[String], json: JsObject): UspblsGamingProductOrder = {
    UspblsGamingProductOrder(bets, pools, variant, json)
  }

}
