package domain.products

import java.net.URI
import domain.products.GamingProduct.GamingProductId


sealed trait ML24GamingProduct extends GamingProduct


trait GamingProduct {

  def id: GamingProductId

  def drawSeries: DrawSeries

}


object GamingProduct{

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


object ML24GamingProduct {

  object EJS extends ML24GamingProduct {

    override val id: GamingProductId = "ejs"
    override val drawSeries: DrawSeries = DrawSeries.Eurojackpot

  }

  object GLS extends ML24GamingProduct {

    override val id: GamingProductId = "gls"
    override val drawSeries: DrawSeries = DrawSeries.`6aus49`

  }

  object S6 extends ML24GamingProduct {

    override val id: GamingProductId = "s6"
    override val drawSeries: DrawSeries = DrawSeries.Super6

  }

  object S77 extends ML24GamingProduct {

    override val id: GamingProductId = "s77"
    override val drawSeries: DrawSeries = DrawSeries.Spiel77

  }

  object EMS extends ML24GamingProduct {

    override val id: GamingProductId = "ems"
    override val drawSeries: DrawSeries = DrawSeries.Euromillions

  }

  object GLSS extends ML24GamingProduct {

    override val id: GamingProductId = "glss"
    override val drawSeries: DrawSeries = DrawSeries.Glücksspirale

  }

  val All = Vector(EJS, GLS, S6, S77, EMS, GLSS)

}