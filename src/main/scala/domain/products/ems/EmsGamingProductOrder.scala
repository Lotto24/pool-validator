package domain.products.ems

import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import domain.products.ML24GamingProduct.EMS
import domain.products.ParticipationPools.ParticipationPoolId
import domain.products.{Bet, GamingProduct, GamingProductOrder, ParticipationPools}
import domain.products.ParticipationPools.{formatParticipationPoolId, parseParticipationPoolId}

import GamingProduct.gamingProductIdToURI


case class EmsBet(
  numbers: Set[Int],
  starnumbers: Set[Int]
) extends Bet


case class EmsParticipationPools(
  firstDate: LocalDate,
  drawDays: Set[DayOfWeek],
  drawCount: Int
) extends ParticipationPools {

  override def ids: Set[ParticipationPoolId] = {
    Iterator.iterate(firstDate)(_.plusDays(1))
      .filter(drawDate => drawDays(drawDate.getDayOfWeek))
      .take(drawCount)
      .map(formatParticipationPoolId(EMS.id, _))
      .toSet
  }
}


case class EmsGamingProductOrder(
  bets: Seq[EmsBet],
  participationPools: EmsParticipationPools
) extends GamingProductOrder {
  override def productURI: URI = EmsGamingProductOrder.productURI
}


object EmsGamingProductOrder {
  val productURI: URI = gamingProductIdToURI(EMS.id)
}
