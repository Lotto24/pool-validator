package domain.products.ejs

import java.net.URI
import java.nio.file.Path
import java.time.LocalDate

import domain.PoolResourceProvider.ProductOrderFactory
import play.api.libs.json.{JsArray, JsObject}

import scala.util.{Failure, Try}


class EjsProductOrderFactory extends ProductOrderFactory {

  override def isApplicableFor(productURI: URI): Boolean = productURI == EjsGamingProductOrder.productURI

  override def create(productURI: URI, orderData: JsObject, docPath: Path): Try[EjsGamingProductOrder] = {
    productURI match {
      case EjsGamingProductOrder.productURI => Try {
        val bets = parseBets((orderData \ "bets").as[JsArray])
        val partPools = parseParticipationPools((orderData \ "participation-pools").as[JsObject])
        EjsGamingProductOrder(bets, participationPools = partPools)
      }
      case x => Failure(new Exception(s"unexpected productURI:$x"))
    }
  }

  private def parseBets(bets: JsArray): Seq[EjsBet] = {
    bets.as[JsArray].value.map { bet => EjsBet(
      numbers = (bet.as[JsObject] \ "numbers").get.as[JsArray].value.map(_.as[Int]).toSet,
      euroNumbers = (bet.as[JsObject] \ "euronumbers").get.as[JsArray].value.map(_.as[Int]).toSet
    )}
  }

  def parseParticipationPools(pools: JsObject): EjsParticipationPools = {
    val drawCount = (pools \ "draw-count").as[Int]
    val firstDate = LocalDate.parse((pools \ "first-date").as[String])
    EjsParticipationPools(firstDate, drawCount)
  }
}

