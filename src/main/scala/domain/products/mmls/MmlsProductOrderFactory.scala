package domain.products.mmls

import domain.PoolResourceProvider.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class MmlsProductOrderFactory extends ProductOrderFactoryAI[MmlsBet, MmlsParticipationPools, MmlsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[MmlsBet] = bets.map { bet => MmlsBet(
    numbers = (bet \ "numbers").as[Seq[Int]], megaBall = (bet \ "mega-ball").as[Int]
  )}

  protected def parseParticipationPools(pools: JsObject): MmlsParticipationPools = fromIntermediateMultiDayPoolsData(pools) { data =>
    MmlsParticipationPools(data.firstDate, data.drawDays, data.drawCount)
  }

  override protected def createOrder(bets: Seq[MmlsBet], pools: MmlsParticipationPools, variant: Option[String], json: JsObject): MmlsGamingProductOrder = {
    MmlsGamingProductOrder(bets, pools, variant, json)
  }

}
