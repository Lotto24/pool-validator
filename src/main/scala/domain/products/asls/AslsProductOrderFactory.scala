package domain.products.asls

import domain.PoolResourceProvider.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class AslsProductOrderFactory extends ProductOrderFactoryAI[AslsBet, AslsParticipationPools, AslsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[AslsBet] = bets.map { bet => AslsBet(
    numbers = (bet \ "numbers").as[Seq[Int]]
  )}

  protected def parseParticipationPools(pools: JsObject): AslsParticipationPools = fromIntermediateSingleDayPoolsData(pools) { data =>
    AslsParticipationPools(data.firstDate, data.drawCount)
  }

  override protected def createOrder(bets: Seq[AslsBet], pools: AslsParticipationPools, variant: Option[String], json: JsObject): AslsGamingProductOrder = {
    AslsGamingProductOrder(bets, pools, variant, json)
  }

}
