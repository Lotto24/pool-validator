package domain.products.apls

import domain.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class AplsProductOrderFactory extends ProductOrderFactoryAI[AplsBet, AplsParticipationPools, AplsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[AplsBet] = bets.map { bet => AplsBet(
    numbers = (bet \ "numbers").as[Seq[Int]],
    powerball = (bet \ "powerball").get.as[Int]
  )}

  protected def parseParticipationPools(pools: JsObject): AplsParticipationPools = fromIntermediateSingleDayPoolsData(pools) { data =>
    AplsParticipationPools(data.firstDate, data.drawCount)
  }

  override protected def createOrder(bets: Seq[AplsBet], pools: AplsParticipationPools, variant: Option[String], json: JsObject): AplsGamingProductOrder = {
    AplsGamingProductOrder(bets, pools, variant, json)
  }

}
