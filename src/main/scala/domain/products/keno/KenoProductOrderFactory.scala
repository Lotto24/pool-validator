package domain.products.keno

import domain.PoolResourceProvider.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class KenoProductOrderFactory extends ProductOrderFactoryAI[KenoBet, KenoParticipationPools, KenoGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[KenoBet] = bets.map { bet => KenoBet(
    numbers = (bet \ "numbers").as[Seq[Int]], stake = (bet \ "stake").as[Int]
  )}

  protected def parseParticipationPools(pools: JsObject): KenoParticipationPools = fromIntermediateSingleDayPoolsData(pools) { data =>
    KenoParticipationPools(data.firstDate, data.drawCount)
  }

  override protected def createOrder(bets: Seq[KenoBet], pools: KenoParticipationPools, variant: Option[String], json: JsObject): KenoGamingProductOrder = {
    KenoGamingProductOrder(bets, pools, variant, json)
  }

}
