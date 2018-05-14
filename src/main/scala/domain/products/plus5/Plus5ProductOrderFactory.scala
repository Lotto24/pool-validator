package domain.products.plus5

import domain.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class Plus5ProductOrderFactory extends ProductOrderFactoryAI[Plus5Bet, Plus5ParticipationPools, Plus5GamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[Plus5Bet] = bets.map { bet => Plus5Bet(
    numbers = (bet \ "numbers").as[Seq[Int]]
  )}

  protected def parseParticipationPools(pools: JsObject): Plus5ParticipationPools = fromIntermediateSingleDayPoolsData(pools) { data =>
    Plus5ParticipationPools(data.firstDate, data.drawCount)
  }

  override protected def createOrder(bets: Seq[Plus5Bet], pools: Plus5ParticipationPools, variant: Option[String], json: JsObject): Plus5GamingProductOrder = {
    Plus5GamingProductOrder(bets, pools, variant, json)
  }

}
