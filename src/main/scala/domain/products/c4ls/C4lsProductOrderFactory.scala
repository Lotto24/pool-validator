package domain.products.c4ls

import domain.PoolResourceProvider.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class C4lsProductOrderFactory extends ProductOrderFactoryAI[C4lsBet, C4lsParticipationPools, C4lsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[C4lsBet] = bets.map { bet => C4lsBet(
    numbers = (bet \ "numbers").as[Seq[Int]], cashBall = (bet \ "cash-ball").as[Int]
  )}

  protected def parseParticipationPools(pools: JsObject): C4lsParticipationPools = fromIntermediateMultiDayPoolsData(pools) { data =>
    C4lsParticipationPools(data.firstDate, data.drawDays, data.drawCount)
  }

  override protected def createOrder(bets: Seq[C4lsBet], pools: C4lsParticipationPools, variant: Option[String], json: JsObject): C4lsGamingProductOrder = {
    C4lsGamingProductOrder(bets, pools, variant, json)
  }

}
