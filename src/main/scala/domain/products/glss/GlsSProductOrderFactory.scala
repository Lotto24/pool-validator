package domain.products.glss

import java.time.LocalDate

import domain.PoolResourceProvider.ProductOrderFactoryAI
import play.api.libs.json.JsObject


class GlsSProductOrderFactory extends ProductOrderFactoryAI[GlsSBet, GlsSParticipationPools, GlsSGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[GlsSBet] = bets.map { bet => 
    GlsSBet(numbers = (bet \ "numbers").get.as[Seq[Int]])
  }

  protected def parseParticipationPools(pools: JsObject): GlsSParticipationPools = fromIntermediateSingleDayPoolsData(pools) { data =>
    GlsSParticipationPools(data.firstDate, data.drawCount)
  }
  
  override protected def createOrder(bets: Seq[GlsSBet], pools: GlsSParticipationPools, variant: Option[String], json: JsObject): GlsSGamingProductOrder = {
    GlsSGamingProductOrder(bets, pools, variant, json)
  }

}
