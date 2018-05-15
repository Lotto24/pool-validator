package domain.products.fls

import domain.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class FlsProductOrderFactory extends ProductOrderFactoryAI[FlsBet, FlsParticipationPools, FlsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[FlsBet] = bets.map { bet => FlsBet(
    numbers = (bet \ "numbers").as[Seq[Int]],
    chancenumber = (bet \ "chancenumber").as[Int]
  )}

  override protected def parseParticipationPools(pools: JsObject): FlsParticipationPools = fromIntermediateMultiDayPoolsData(pools){ data =>
    FlsParticipationPools(data.firstDate, data.drawDays, data.drawCount)
  }

  override protected def createOrder(bets: Seq[FlsBet], pools: FlsParticipationPools, variant: Option[String], json: JsObject): FlsGamingProductOrder = {
    FlsGamingProductOrder(bets, pools, variant, json)
  }

}
