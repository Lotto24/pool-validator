package domain.products.gls

import domain.PoolResourceProvider.ProductOrderFactoryAI
import play.api.libs.json.JsObject


class GlsProductOrderFactory extends ProductOrderFactoryAI[GlsBet, GlsParticipationPools, GlsGamingProductOrder]{

  protected def parseBets(bets: Seq[JsObject]): Seq[GlsBet] = bets.map { bet => 
    GlsBet(
      numbers = (bet \ "numbers").get.as[Seq[Int]],
      supernumber = (bet \ "super-number").get.as[Int],
      system = (bet \ "system").toOption.map(_.as[String])
    )
  }

  override protected def parseParticipationPools(pools: JsObject): GlsParticipationPools = fromIntermediateMultiDayPoolsData(pools){ data =>
    GlsParticipationPools(data.firstDate, data.drawDays, data.drawCount)
  }
  
  override protected def createOrder(bets: Seq[GlsBet], pools: GlsParticipationPools, variant: Option[String], json: JsObject): GlsGamingProductOrder = {
    GlsGamingProductOrder(bets, pools, variant, json)
  }

}
