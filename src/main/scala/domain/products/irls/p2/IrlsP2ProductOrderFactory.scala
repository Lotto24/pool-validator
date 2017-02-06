package domain.products.irls.p2

import domain.PoolResourceProvider.ProductOrderFactoryAI
import domain.products.irls.IrlsBet
import play.api.libs.json.JsObject

class IrlsP2ProductOrderFactory extends ProductOrderFactoryAI[IrlsBet, IrlsP2ParticipationPools, IrlsP2GamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[IrlsBet] = bets.map { bet => IrlsBet(
    numbers = (bet \ "numbers").as[Seq[Int]]
  )}

  override protected def parseParticipationPools(pools: JsObject): IrlsP2ParticipationPools = fromIntermediateMultiDayPoolsData(pools){ data =>
    IrlsP2ParticipationPools(data.firstDate, data.drawDays, data.drawCount)
  }

  override protected def createOrder(bets: Seq[IrlsBet], pools: IrlsP2ParticipationPools, variant: Option[String], json: JsObject): IrlsP2GamingProductOrder = {
    IrlsP2GamingProductOrder(bets, pools, variant, json)
  }

}
