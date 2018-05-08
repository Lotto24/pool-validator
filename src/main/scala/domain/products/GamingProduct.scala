package domain.products

import enumeratum.Enum
import enumeratum.EnumEntry
import java.net.URI

import domain.products.GamingProduct.GamingProductId

import scala.collection.immutable

sealed trait ML24GamingProduct extends GamingProduct with EnumEntry


trait GamingProduct {

  def id: GamingProductId

  def lottery: PrimaryLottery

}


object GamingProduct {

  final type GamingProductId = String

  private val gamingProductURIPrefix = "http://zoe.mylotto24.co.uk/gaming-products/mylotto24/"
  private val gamingProductURIPattern = (gamingProductURIPrefix + "(.*)").r

  def gamingProductIdFromURI(uri: URI): GamingProductId = uri.toString match {
    case gamingProductURIPattern(gamingProductId) => gamingProductId
  }

  def gamingProductIdToURI(id: GamingProductId): URI = {
    URI.create(gamingProductURIPrefix + id)
  }

}


object ML24GamingProduct extends Enum[ML24GamingProduct] {

  object EJS extends ML24GamingProduct {

    override val id: GamingProductId = "ejs"
    override val lottery: PrimaryLottery = PrimaryLottery.Eurojackpot

  }

  object GLS extends ML24GamingProduct {

    override val id: GamingProductId = "gls"
    override val lottery: PrimaryLottery = PrimaryLottery.`6aus49`

  }

  object S6 extends ML24GamingProduct {

    override val id: GamingProductId = "s6"
    override val lottery: PrimaryLottery = PrimaryLottery.Super6

  }

  object S77 extends ML24GamingProduct {

    override val id: GamingProductId = "s77"
    override val lottery: PrimaryLottery = PrimaryLottery.Spiel77

  }

  object EMS extends ML24GamingProduct {

    override val id: GamingProductId = "ems"
    override val lottery: PrimaryLottery = PrimaryLottery.Euromillions

  }

  object GLSS extends ML24GamingProduct {

    override val id: GamingProductId = "glss"
    override val lottery: PrimaryLottery = PrimaryLottery.Glücksspirale

  }

  object IRLS extends ML24GamingProduct {

    override val id: GamingProductId = "irls"
    override val lottery: PrimaryLottery = PrimaryLottery.IrishLotto
  }

  object UKLS extends ML24GamingProduct {

    override val id: GamingProductId = "ukls"
    override val lottery: PrimaryLottery = PrimaryLottery.UkLotto
  }

  object AOLS extends ML24GamingProduct {

    override val id: GamingProductId = "aols"
    override val lottery: PrimaryLottery = PrimaryLottery.AUSOzLotto
  }

  object APLS extends ML24GamingProduct {

    override val id: GamingProductId = "apls"
    override val lottery: PrimaryLottery = PrimaryLottery.AUSPowerball
  }

  object ASLS extends ML24GamingProduct {

    override val id: GamingProductId = "asls"
    override val lottery: PrimaryLottery = PrimaryLottery.AUSSaturdayLotto
  }

  object AMLS extends ML24GamingProduct {

    override val id: GamingProductId = "amls"
    override val lottery: PrimaryLottery = PrimaryLottery.AUSMondayLotto
  }

  object AWLS extends ML24GamingProduct {

    override val id: GamingProductId = "awls"
    override val lottery: PrimaryLottery = PrimaryLottery.AUSWednesdayLotto
  }

  object SLS extends ML24GamingProduct {

    override val id: GamingProductId = "sls"
    override val lottery: PrimaryLottery = PrimaryLottery.SwedishLotto
  }

  object UKTBLS extends ML24GamingProduct {

    override val id: GamingProductId = "uktbls"
    override def lottery: PrimaryLottery = PrimaryLottery.UkThunderball
  }

  object FLS extends ML24GamingProduct {

    override val id: GamingProductId = "fls"
    override val lottery: PrimaryLottery = PrimaryLottery.FrenchLotto
  }

  object PLS extends ML24GamingProduct {

    override val id: GamingProductId = "pls"
    override val lottery: PrimaryLottery = PrimaryLottery.PolishLotto
  }

  object XMASL extends ML24GamingProduct {

    override val id: GamingProductId = "xmasl"
    override val lottery: PrimaryLottery = PrimaryLottery.Xmasl
  }

  object IRLSP1 extends ML24GamingProduct {

    override val id: GamingProductId = "irlsp1"
    override val lottery: PrimaryLottery = PrimaryLottery.IrishLottoPlus1
  }

  object IRLSP2 extends ML24GamingProduct {

    override val id: GamingProductId = "irlsp2"
    override val lottery: PrimaryLottery = PrimaryLottery.IrishLottoPlus2
  }

  object EMSPLUS extends ML24GamingProduct {

    override val id: GamingProductId = "emsp"
    override val lottery: PrimaryLottery = PrimaryLottery.EuromillionsPlus
  }

  object IRISHRAFFLE extends ML24GamingProduct {

    override val id: GamingProductId = "irlsr"
    override val lottery: PrimaryLottery = PrimaryLottery.IrishRaffle
  }

  object PLUS5 extends ML24GamingProduct {
    override val id: GamingProductId = "plus5"
    override val lottery: PrimaryLottery = PrimaryLottery.Plus5
  }

  object KENO extends ML24GamingProduct {
    override val id: GamingProductId = "keno"
    override val lottery: PrimaryLottery = PrimaryLottery.Keno
  }

  object C4LS extends ML24GamingProduct {
    override val id: GamingProductId = "c4ls"
    override val lottery: PrimaryLottery = PrimaryLottery.Cash4Life
  }

  object USPBLS extends ML24GamingProduct {
    override val id: GamingProductId = "uspbls"
    override val lottery: PrimaryLottery = PrimaryLottery.UsPowerball
  }

  object MMLS extends ML24GamingProduct {
    override val id: GamingProductId = "mmls"
    override val lottery: PrimaryLottery = PrimaryLottery.MegaMillions
  }

  override lazy val values: immutable.IndexedSeq[ML24GamingProduct] = findValues
  
  val knownProductIds: Set[GamingProductId] = values.map(_.id).toSet

}