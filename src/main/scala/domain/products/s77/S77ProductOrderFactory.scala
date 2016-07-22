package domain.products.s77



import java.net.URI
import java.nio.file.Path
import java.time.LocalDate

import domain.PoolResourceProvider.ProductOrderFactory
import domain.products.GamingProductOrder
import play.api.libs.json.{JsArray, JsObject}
import util.Utils

import scala.util.{Failure, Try}


class S77ProductOrderFactory extends ProductOrderFactory{

  override def isApplicableFor(productURI: URI): Boolean = productURI == S77GamingProductOrder.productURI

  override def create(productURI: URI, orderData: JsObject, docPath: Path): Try[GamingProductOrder] = {
    productURI match {
      case S77GamingProductOrder.productURI => Try {
        val bets = parseBets((orderData \ "bets").as[JsArray])
        val partPools = parseParticipationPools((orderData \ "participation-pools").as[JsObject])
        S77GamingProductOrder(bets, participationPools = partPools)
      }
      case x => Failure(new Exception(s"unexpected productURI:$x"))
    }
  }

  private def parseBets(bets: JsArray): Seq[S77Bet] = {
    bets.as[JsArray].value.map { bet =>
      val numbers = (bet.as[JsObject] \ "numbers").as[JsArray].value.map(_.as[Int]).toSet
      S77Bet(numbers)
    }
  }

  def parseParticipationPools(pools: JsObject): S77ParticipationPools = {
    val firstDate = LocalDate.parse((pools \ "first-date").as[String])
    val drawDays = (pools \ "draw-days").get.as[JsArray].value.map(_.as[String]).map(Utils.dayOfWeekFromString(_).get).toSet
    val drawCount = (pools \ "draw-count").as[Int]
    S77ParticipationPools(firstDate, drawDays, drawCount)
  }

}
