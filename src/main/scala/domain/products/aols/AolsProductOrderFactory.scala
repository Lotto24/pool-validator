package domain.products.aols

import domain.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class AolsProductOrderFactory extends ProductOrderFactoryAI[AolsBet, AolsParticipationPools, AolsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[AolsBet] = bets.map { bet =>
    AolsBet(numbers = (bet \ "numbers").as[Seq[Int]])
  }

  protected def parseParticipationPools(pools: JsObject): AolsParticipationPools = fromIntermediateSingleDayPoolsData(pools) { data =>
    AolsParticipationPools(data.firstDate, data.drawCount)
  }

  override protected def createOrder(bets: Seq[AolsBet], pools: AolsParticipationPools, variant: Option[String], json: JsObject): AolsGamingProductOrder = {
    AolsGamingProductOrder(bets, pools, variant, json)
  }

}
