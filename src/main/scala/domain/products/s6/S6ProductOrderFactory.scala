package domain.products.s6

import domain.PoolResourceProvider.ProductOrderFactoryAI
import play.api.libs.json.JsObject


class S6ProductOrderFactory extends ProductOrderFactoryAI[S6Bet, S6ParticipationPools, S6GamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[S6Bet] = bets.map { bet => 
    S6Bet(numbers = (bet \ "numbers").get.as[Seq[Int]])
  }

  override protected def parseParticipationPools(pools: JsObject): S6ParticipationPools = fromIntermediateMultiDayPoolsData(pools){ values =>
    S6ParticipationPools(values.firstDate, values.drawDays, values.drawCount)
  }
  
  override protected def createOrder(bets: Seq[S6Bet], pools: S6ParticipationPools, variant: Option[String], json: JsObject): S6GamingProductOrder = {
    S6GamingProductOrder(bets, pools, variant, json)
  }

}
