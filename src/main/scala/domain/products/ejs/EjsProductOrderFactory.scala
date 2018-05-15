package domain.products.ejs

import domain.ProductOrderFactoryAI
import play.api.libs.json.JsObject


class EjsProductOrderFactory extends ProductOrderFactoryAI[EjsBet, EjsParticipationPools, EjsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[EjsBet] = bets.map { bet => 
    EjsBet(
      numbers = (bet \ "numbers").get.as[Seq[Int]],
      euroNumbers = (bet \ "euronumbers").as[Seq[Int]]
    )
  }

  protected def parseParticipationPools(pools: JsObject): EjsParticipationPools = fromIntermediateSingleDayPoolsData(pools) { data =>
    EjsParticipationPools(data.firstDate, data.drawCount)
  }
  
  override protected def createOrder(bets: Seq[EjsBet], pools: EjsParticipationPools, variant: Option[String], json: JsObject): EjsGamingProductOrder = {
    EjsGamingProductOrder(bets, pools, variant, json)
  }

}
