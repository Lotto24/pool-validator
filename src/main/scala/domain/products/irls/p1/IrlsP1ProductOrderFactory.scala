package domain.products.irls.p1

import domain.ProductOrderFactoryAI
import domain.products.irls.IrlsBet
import play.api.libs.json.JsObject

class IrlsP1ProductOrderFactory extends ProductOrderFactoryAI[IrlsBet, IrlsP1ParticipationPools, IrlsP1GamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[IrlsBet] = bets.map { bet => IrlsBet(
    numbers = (bet \ "numbers").as[Seq[Int]]
  )}

  override protected def parseParticipationPools(pools: JsObject): IrlsP1ParticipationPools = fromIntermediateMultiDayPoolsData(pools){ data =>
    IrlsP1ParticipationPools(data.firstDate, data.drawDays, data.drawCount)
  }

  override protected def createOrder(bets: Seq[IrlsBet], pools: IrlsP1ParticipationPools, variant: Option[String], json: JsObject): IrlsP1GamingProductOrder = {
    IrlsP1GamingProductOrder(bets, pools, variant, json)
  }

}
