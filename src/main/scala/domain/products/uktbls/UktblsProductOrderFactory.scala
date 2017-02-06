package domain.products.uktbls

import domain.PoolResourceProvider.ProductOrderFactoryAI
import play.api.libs.json.JsObject

class UktblsProductOrderFactory extends ProductOrderFactoryAI[UktblsBet, UktblsParticipationPools, UktblsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[UktblsBet] = bets.map { bet => UktblsBet(
    numbers = (bet \ "numbers").as[Seq[Int]],
    thunderball = (bet \ "thunderball").as[Int]
  )}

  override protected def parseParticipationPools(pools: JsObject): UktblsParticipationPools = fromIntermediateMultiDayPoolsData(pools){ values =>
    UktblsParticipationPools(values.firstDate, values.drawDays, values.drawCount)
  }

  override protected def createOrder(bets: Seq[UktblsBet], pools: UktblsParticipationPools, variant: Option[String], json: JsObject): UktblsGamingProductOrder = {
    UktblsGamingProductOrder(bets, pools, variant, json)
  }

}