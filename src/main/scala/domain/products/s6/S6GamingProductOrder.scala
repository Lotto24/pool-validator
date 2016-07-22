package domain.products.s6

import domain.products.GamingProductOrder
import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.GamingProduct._
import domain.products.ML24GamingProduct.{GLS, S6}
import domain.products.ParticipationPools.{ParticipationPoolId, formatParticipationPoolId}
import domain.products.{Bet, GamingProductOrder, ParticipationPools}


case class S6Bet(
  numbers: Set[Int]
) extends Bet


case class S6GamingProductOrder(
  bets: Seq[S6Bet],
  participationPools: S6ParticipationPools
) extends GamingProductOrder {
  override def productURI: URI = S6GamingProductOrder.productURI
}


object S6GamingProductOrder {
  val productURI: URI = gamingProductIdToURI(S6.id)
}

case class S6ParticipationPools(
  firstDate: LocalDate,
  drawDays: Set[DayOfWeek],
  drawCount: Int
) extends ParticipationPools {

  override def ids: Set[ParticipationPoolId] = {
    Iterator.iterate(firstDate)(_.plusDays(1))
      .filter(drawDate => drawDays(drawDate.getDayOfWeek))
      .take(drawCount)
      .map(formatParticipationPoolId(S6.id, _))
      .toSet
  }

}
