package domain.products.emsplus

import domain.PoolResourceProvider.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class EmsPlusProductOrderFactory extends ProductOrderFactoryAI[EmsPlusBet, EmsPlusParticipationPools, EmsPlusGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[EmsPlusBet] = bets.map { bet => EmsPlusBet(
    numbers = (bet \ "numbers").as[Seq[Int]]
  )}

  override protected def parseParticipationPools(pools: JsObject): EmsPlusParticipationPools = fromIntermediateMultiDayPoolsData(pools){ data =>
    EmsPlusParticipationPools(data.firstDate, data.drawDays, data.drawCount)
  }

  override protected def createOrder(bets: Seq[EmsPlusBet], pools: EmsPlusParticipationPools, variant: Option[String], json: JsObject): EmsPlusGamingProductOrder = {
    EmsPlusGamingProductOrder(bets, pools, variant, json)
  }

}
