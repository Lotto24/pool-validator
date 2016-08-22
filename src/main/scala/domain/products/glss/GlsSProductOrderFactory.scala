package domain.products.glss


import java.net.URI
import java.nio.file.Path
import java.time.LocalDate

import domain.PoolResourceProvider.ProductOrderFactory
import domain.products.GamingProductOrder
import play.api.libs.json.{JsArray, JsObject}

import scala.util.{Failure, Try}


class GlsSProductOrderFactory extends ProductOrderFactory{

  override def isApplicableFor(productURI: URI): Boolean = productURI == GlsSGamingProductOrder.productURI

  override def create(productURI: URI, orderData: JsObject, docPath: Path): Try[GamingProductOrder] = {
    productURI match {
      case GlsSGamingProductOrder.productURI => Try {
        val bets = parseBets((orderData \ "bets").as[JsArray])
        val partPools = parseParticipationPools((orderData \ "participation-pools").as[JsObject])
        GlsSGamingProductOrder(bets, partPools, orderData)
      }
      case x => Failure(new Exception(s"unexpected productURI:$x"))
    }
  }

  private def parseBets(bets: JsArray): Seq[GlsSBet] = {
    bets.as[JsArray].value.map { bet =>
      val numbers = (bet.as[JsObject] \ "numbers").as[JsArray].value.map(_.as[Int]).toSet
      GlsSBet(numbers)
    }
  }

  def parseParticipationPools(pools: JsObject): GlsSParticipationPools = {
    val firstDate = LocalDate.parse((pools \ "first-date").as[String])
    val drawCount = (pools \ "draw-count").as[Int]
    GlsSParticipationPools(firstDate, drawCount)
  }

}
