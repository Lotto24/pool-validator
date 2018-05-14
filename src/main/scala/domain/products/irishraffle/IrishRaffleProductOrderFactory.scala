package domain.products.irishraffle

import domain.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class IrishRaffleProductOrderFactory extends ProductOrderFactoryAI[IrishRaffleBet, IrishRaffleParticipationPools, IrishRaffleGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[IrishRaffleBet] = bets.map { bet => IrishRaffleBet(
    numbers = (bet \ "numbers").as[Seq[Int]]
  )}

  override protected def parseParticipationPools(pools: JsObject): IrishRaffleParticipationPools = fromIntermediateMultiDayPoolsData(pools){ data =>
    IrishRaffleParticipationPools(data.firstDate, data.drawDays, data.drawCount)
  }

  override protected def createOrder(bets: Seq[IrishRaffleBet], pools: IrishRaffleParticipationPools, variant: Option[String], json: JsObject): IrishRaffleGamingProductOrder = {
    IrishRaffleGamingProductOrder(bets, pools, variant, json)
  }

}
