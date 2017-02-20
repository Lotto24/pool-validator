package domain.products.ukls

import domain.PoolResourceProvider.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class UklsProductOrderFactory extends ProductOrderFactoryAI[UklsBet, UklsParticipationPools, UklsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[UklsBet] = bets.map { bet => UklsBet(
    numbers = (bet \ "numbers").as[Seq[Int]]
  )}

  override protected def parseParticipationPools(pools: JsObject): UklsParticipationPools = fromIntermediateMultiDayPoolsData(pools){ values =>
    UklsParticipationPools(values.firstDate, values.drawDays, values.drawCount)
  }

  override protected def createOrder(bets: Seq[UklsBet], pools: UklsParticipationPools, variant: Option[String], json: JsObject): UklsGamingProductOrder = {
    UklsGamingProductOrder(bets, pools, variant, json)
  }

}
