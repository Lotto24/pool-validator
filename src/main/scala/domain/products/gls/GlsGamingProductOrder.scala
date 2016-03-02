package domain.products.gls

import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct._
import domain.products.ML24GamingProduct.GLS
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, ParticipationPools}

/**
  * One bet of a German Lotto gaming product order
  */
case class GlsBet(numbers: Seq[Int],
                  supernumber: Int,
                  system: Option[String]) extends Bet

/**
  * The participation pools or draws of a German Lotto gaming product order
  */
case class GlsParticipationPools(firstDate: LocalDate,
                                 drawDays: Set[DayOfWeek],
                                 weeks: Int) extends ParticipationPools {

  override def ids: Set[ParticipationPoolId] = {
    Iterable.iterate(firstDate, 7 * weeks)(_.plusDays(1))
      .filter(drawDate => drawDays(drawDate.getDayOfWeek))
      .map(formatParticipationPoolId(GLS.id, _))
      .toSet
  }

}

/**
  * A German Lotto gaming product order
  */
case class GlsGamingProductOrder(bets: Seq[GlsBet],
                                 participationPools: GlsParticipationPools) extends GamingProductOrder {
  override def productURI: URI = GlsGamingProductOrder.productURI
}


object GlsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(GLS.id)
}
