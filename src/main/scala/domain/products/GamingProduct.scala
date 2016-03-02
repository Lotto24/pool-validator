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

  val All = Vector(EJS, GLS)

}
