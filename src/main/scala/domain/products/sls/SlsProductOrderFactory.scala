package domain.products.sls

import domain.PoolResourceProvider.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class SlsProductOrderFactory extends ProductOrderFactoryAI[SlsBet, SlsParticipationPools, SlsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[SlsBet] = bets.map { bet => SlsBet(
    numbers = (bet \ "numbers").as[Seq[Int]]
  )}

  override protected def parseParticipationPools(pools: JsObject): SlsParticipationPools = fromIntermediateMultiDayPoolsData(pools){ values =>
    SlsParticipationPools(values.firstDate, values.drawDays, values.drawCount)
  }

  override protected def createOrder(bets: Seq[SlsBet], pools: SlsParticipationPools, variant: Option[String], json: JsObject): SlsGamingProductOrder = {
    SlsGamingProductOrder(bets, pools, variant, json)
  }

}