package domain.products.s77

import domain.PoolResourceProvider.ProductOrderFactoryAI
import play.api.libs.json.JsObject


class S77ProductOrderFactory extends ProductOrderFactoryAI[S77Bet, S77ParticipationPools, S77GamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[S77Bet] = bets.map { bet => 
    S77Bet(numbers = (bet \ "numbers").get.as[Seq[Int]])
  }

  override protected def parseParticipationPools(pools: JsObject): S77ParticipationPools = fromIntermediateMultiDayPoolsData(pools){ values =>
    S77ParticipationPools(values.firstDate, values.drawDays, values.drawCount)
  }
  
  override protected def createOrder(bets: Seq[S77Bet], pools: S77ParticipationPools, variant: Option[String], json: JsObject): S77GamingProductOrder = {
    S77GamingProductOrder(bets, pools, variant, json)
  }

}
