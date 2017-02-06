package domain.products.awls

import domain.PoolResourceProvider.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class AwlsProductOrderFactory extends ProductOrderFactoryAI[AwlsBet, AwlsParticipationPools, AwlsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[AwlsBet] = bets.map { bet => AwlsBet(
    numbers = (bet \ "numbers").as[Seq[Int]]
  )}

  protected def parseParticipationPools(pools: JsObject): AwlsParticipationPools = fromIntermediateSingleDayPoolsData(pools) { data =>
    AwlsParticipationPools(data.firstDate, data.drawCount)
  }

  override protected def createOrder(bets: Seq[AwlsBet], pools: AwlsParticipationPools, variant: Option[String], json: JsObject): AwlsGamingProductOrder = {
    AwlsGamingProductOrder(bets, pools, variant, json)
  }

}
