package domain.products.pls

import domain.PoolResourceProvider.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class PlsProductOrderFactory extends ProductOrderFactoryAI[PlsBet, PlsParticipationPools, PlsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[PlsBet] = bets.map { bet => PlsBet(
    numbers = (bet \ "numbers").as[Seq[Int]]
  )}

  override protected def parseParticipationPools(pools: JsObject): PlsParticipationPools = fromIntermediateMultiDayPoolsData(pools){ values =>
    PlsParticipationPools(values.firstDate, values.drawDays, values.drawCount)
  }

  override protected def createOrder(bets: Seq[PlsBet], pools: PlsParticipationPools, variant: Option[String], json: JsObject): PlsGamingProductOrder = {
    PlsGamingProductOrder(bets, pools, variant, json)
  }

}
