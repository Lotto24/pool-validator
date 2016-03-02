package domain.products

/**
 * http://www.lotteries.io/concepts/draw-series
 */
sealed trait DrawSeries extends Serializable

object DrawSeries {

  case object `6aus49` extends DrawSeries

  case object Spiel77 extends DrawSeries

  case object Super6 extends DrawSeries

  case object Glücksspirale extends DrawSeries

  case object Eurojackpot extends DrawSeries

  case object Euromillions extends DrawSeries

  case object Keno extends DrawSeries

  case object Plus5 extends DrawSeries

  case object Xmasl extends DrawSeries

}
