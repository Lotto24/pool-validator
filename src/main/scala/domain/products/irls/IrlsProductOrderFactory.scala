package domain.products.irls

import domain.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class IrlsProductOrderFactory extends ProductOrderFactoryAI[IrlsBet, IrlsParticipationPools, IrlsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[IrlsBet] = bets.map { bet => IrlsBet(
    numbers = (bet \ "numbers").as[Seq[Int]]
  )}

  override protected def parseParticipationPools(pools: JsObject): IrlsParticipationPools = fromIntermediateMultiDayPoolsData(pools){ data =>
    IrlsParticipationPools(data.firstDate, data.drawDays, data.drawCount)
  }

  override protected def createOrder(bets: Seq[IrlsBet], pools: IrlsParticipationPools, variant: Option[String], json: JsObject): IrlsGamingProductOrder = {
    IrlsGamingProductOrder(bets, pools, variant, json)
  }

}
