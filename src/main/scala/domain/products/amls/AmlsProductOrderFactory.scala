package domain.products.amls

import domain.ProductOrderFactoryAI
import play.api.libs.json.JsObject


class AmlsProductOrderFactory extends ProductOrderFactoryAI[AmlsBet, AmlsParticipationPools, AmlsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[AmlsBet] = bets.map { bet => 
    AmlsBet(numbers = (bet \ "numbers").as[Seq[Int]])
  }

  protected def parseParticipationPools(pools: JsObject): AmlsParticipationPools = fromIntermediateSingleDayPoolsData(pools) { data =>
    AmlsParticipationPools(data.firstDate, data.drawCount)
  }
  
  override protected def createOrder(bets: Seq[AmlsBet], pools: AmlsParticipationPools, variant: Option[String], json: JsObject): AmlsGamingProductOrder = {
    AmlsGamingProductOrder(bets, pools, variant, json)
  }

}
