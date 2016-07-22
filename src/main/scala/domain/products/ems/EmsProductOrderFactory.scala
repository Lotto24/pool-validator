package domain.products.ems

import java.net.URI
import java.nio.file.Path
import java.time.{DayOfWeek, LocalDate}

import domain.PoolResourceProvider.ProductOrderFactory
import play.api.libs.json.{JsArray, JsObject}

import scala.util.{Failure, Try}


class EmsProductOrderFactory extends ProductOrderFactory {

  override def isApplicableFor(productURI: URI): Boolean = productURI == EmsGamingProductOrder.productURI

  override def create(productURI: URI, orderData: JsObject, docPath: Path): Try[EmsGamingProductOrder] = {
    productURI match {
      case EmsGamingProductOrder.productURI => Try {
        val bets = parseBets((orderData \ "bets").as[JsArray])
        val partPools = parseParticipationPools((orderData \ "participation-pools").as[JsObject])
        EmsGamingProductOrder(bets, participationPools = partPools)
      }
      case x => Failure(new Exception(s"unexpected productURI:$x"))
    }
  }

  private def parseBets(bets: JsArray): Seq[EmsBet] = {
    bets.as[JsArray].value.map { bet => EmsBet(
      numbers = (bet.as[JsObject] \ "numbers").get.as[JsArray].value.map(_.as[Int]).toSet,
      starnumbers = (bet.as[JsObject] \ "starnumbers").get.as[JsArray].value.map(_.as[Int]).toSet
    )}
  }

  def parseParticipationPools(pools: JsObject): EmsParticipationPools = {
    val drawCount = (pools \ "draw-count").as[Int]
    val drawDays: Set[DayOfWeek] = (pools \ "draw-days").get.as[JsArray].value.map(_.as[String]).map(DayOfWeek.valueOf).toSet
    val firstDate = LocalDate.parse((pools \ "first-date").as[String])
    EmsParticipationPools(firstDate, drawDays, drawCount)
  }
}

