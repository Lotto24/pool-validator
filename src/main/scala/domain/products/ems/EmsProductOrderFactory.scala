package domain.products.ems

import domain.ProductOrderFactoryAI
import play.api.libs.json.JsObject


class EmsProductOrderFactory extends ProductOrderFactoryAI[EmsBet, EmsParticipationPools, EmsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[EmsBet] = bets.map { bet => 
    EmsBet(
      numbers = (bet \ "numbers").get.as[Seq[Int]],
      starnumbers = (bet \ "starnumbers").get.as[Seq[Int]]
    )
  }
  
  override protected def parseParticipationPools(pools: JsObject): EmsParticipationPools = fromIntermediateMultiDayPoolsData(pools){ data =>
    EmsParticipationPools(data.firstDate, data.drawDays, data.drawCount)
  }
  
  override protected def createOrder(bets: Seq[EmsBet], pools: EmsParticipationPools, variant: Option[String], json: JsObject): EmsGamingProductOrder = {
    EmsGamingProductOrder(bets, pools, variant, json)
  }

}
