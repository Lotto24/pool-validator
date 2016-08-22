package domain.products.gls

import java.net.URI
import java.nio.file.Path
import java.time.LocalDate

import domain.PoolResourceProvider.ProductOrderFactory
import domain.products.GamingProductOrder
import play.api.libs.json.{JsArray, JsObject}
import util.Utils

import scala.util.{Failure, Try}


class GlsProductOrderFactory extends ProductOrderFactory{

  override def isApplicableFor(productURI: URI): Boolean = productURI == GlsGamingProductOrder.productURI

  override def create(productURI: URI, orderData: JsObject, docPath: Path): Try[GamingProductOrder] = {
    productURI match {
      case GlsGamingProductOrder.productURI => Try {
        val bets = parseBets((orderData \ "bets").as[JsArray])
        val partPools = parseParticipationPools((orderData \ "participation-pools").as[JsObject])
        GlsGamingProductOrder(bets, participationPools = partPools, orderData)
      }
      case x => Failure(new Exception(s"unexpected productURI:$x"))
    }
  }

  private def parseBets(bets: JsArray): Seq[GlsBet] = {
    bets.as[JsArray].value.map { bet =>
      val number = (bet.as[JsObject] \ "numbers").as[JsArray].value.map(_.as[Int])
      val superNumber = (bet.as[JsObject] \ "super-number").as[Int]
      val system = (bet.as[JsObject] \ "system").toOption.map(_.as[String])
      GlsBet(number, superNumber, system)
    }
  }

  def parseParticipationPools(pools: JsObject): GlsParticipationPools = {
    val firstDate = LocalDate.parse((pools \ "first-date").as[String])
    val drawDays = (pools \ "draw-days").get.as[JsArray].value.map(_.as[String]).map(Utils.dayOfWeekFromString(_).get).toSet
    val drawCount = (pools \ "draw-count").as[Int]
    GlsParticipationPools(firstDate, drawDays, drawCount)
  }

}
