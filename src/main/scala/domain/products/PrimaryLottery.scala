package domain.products

import java.util.Locale

/**
  * http://www.lotteries.io/concepts/draw-series
  */
sealed trait PrimaryLottery extends Serializable {
  def id: String
}

object PrimaryLottery {

  val allLotteries: Vector[PrimaryLottery] = {
    Vector(
      `6aus49`,
      Spiel77,
      Super6,
      Glücksspirale,
      Eurojackpot,
      Euromillions,
      EuromillionsPlus,
      IrishLotto,
      IrishLottoPlus1,
      IrishLottoPlus2,
      IrishRaffle,
      UkLotto,
      AUSOzLotto,
      AUSPowerball,
      AUSSaturdayLotto,
      AUSMondayLotto,
      AUSWednesdayLotto,
      SwedishLotto,
      FrenchLotto,
      PolishLotto,
      Keno,
      Plus5,
      Xmasl,
      UkThunderball
    )
  }

  val allLotteriesById: Map[String, PrimaryLottery] = allLotteries.map(l => l.id -> l).toMap

  def getLottery(id: String): Option[PrimaryLottery] = allLotteriesById.get(id)

  case object `6aus49` extends PrimaryLottery {
    val Name = "6aus49"
    override def id: String = Name
  }

  case object Spiel77 extends PrimaryLottery {
    val Name = "spiel77"
    override def id: String = Name
  }

  case object Super6 extends PrimaryLottery {
    val Name = "super6"
    override def id: String = Name
  }

  case object Glücksspirale extends PrimaryLottery {
    val Name = "gluecksspirale"
    override def id: String = Name
  }

  case object Eurojackpot extends PrimaryLottery {
    val Name = "eurojackpot"
    override def id: String = Name
  }

  case object Euromillions extends PrimaryLottery {
    val locale = new Locale("es", "ES")
    val Name = "euromillions"
    override def id: String = Name
  }

  case object EuromillionsPlus extends PrimaryLottery {
    val Name = "euromillionsplus"
    override def id: String = Name
  }

  case object IrishLotto extends PrimaryLottery {
    val Name = "irishlotto"
    override def id: String = Name
  }

  case object IrishLottoPlus1 extends PrimaryLottery {
    val Name = "irishlottoplus1"
    override def id: String = Name
  }

  case object IrishLottoPlus2 extends PrimaryLottery {
    val Name = "irishlottoplus2"
    override def id: String = Name
  }

  case object UkLotto extends PrimaryLottery {
    val Name = "uklotto"
    override def id: String = Name
  }

  case object AUSOzLotto extends PrimaryLottery {
    val Name = "ozlotto"
    override def id: String = Name
  }

  case object AUSPowerball extends PrimaryLottery {
    val Name = "auspowerball"
    override def id: String = Name
  }

  case object AUSSaturdayLotto extends PrimaryLottery {
    val Name = "auslottosaturday"
    override def id: String = Name
  }

  case object AUSMondayLotto extends PrimaryLottery {
    val Name = "auslottomonday"
    override def id: String = Name
  }

  case object AUSWednesdayLotto extends PrimaryLottery {
    val Name = "auslottowednesday"
    override def id: String = Name
  }

  case object SwedishLotto extends PrimaryLottery {
    val Name = "swedishlotto"
    override def id: String = Name
  }

  case object FrenchLotto extends PrimaryLottery {
    val Name = "frenchlotto"
    override def id: String = Name
  }

  case object PolishLotto extends PrimaryLottery {
    val Name = "polishlotto"
    override def id: String = Name
  }

  case object Keno extends PrimaryLottery {
    val Name = "keno"
    override def id: String = Name
  }

  case object Plus5 extends PrimaryLottery {
    val Name = "plus5"
    override def id: String = Name
  }

  case object Xmasl extends PrimaryLottery {
    val Name = "xmasl"
    override def id: String = Name
  }

  case object UkThunderball extends PrimaryLottery {
    val Name = "ukthunderball"
    override def id: String = Name
  }

  case object IrishRaffle extends PrimaryLottery {
    val Name = "irishraffle"
    override def id: String = Name
  }

}
