package domain.products

import java.net.URI
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import domain.products.GamingProduct.GamingProductId
import domain.products.ParticipationPools.ParticipationPoolId
import play.api.libs.json.JsObject


trait GamingProductOrder{
  def participationPools: ParticipationPools
  def productURI: URI
  def bets : Seq[Bet]
  
  /** Used to enable the option of showing raw-data in the UI */
  def json : JsObject
  
  /** Delivers a copy with an empty `json` value (useful for tests).*/
  def withEmptyJson(): GamingProductOrder 
}

trait ParticipationPools {
  def firstDate: LocalDate

  def ids: Set[ParticipationPoolId]
}

object ParticipationPools {
  final type ParticipationPoolId = String

  private val ParticipationPoolIdPattern = "([^/]+)/(.+)".r
  private val ParticipationPoolIdDateFormat = DateTimeFormatter.ISO_LOCAL_DATE

  def formatParticipationPoolId(gamingProductId: GamingProductId, drawDate: LocalDate): ParticipationPoolId = {
    gamingProductId + "/" + ParticipationPoolIdDateFormat.format(drawDate)
  }

  def parseParticipationPoolId(id: ParticipationPoolId): (GamingProductId, LocalDate) = id match {
    case ParticipationPoolIdPattern(gamingProductId, drawDate) => (gamingProductId, LocalDate.parse(drawDate, ParticipationPoolIdDateFormat))
  }
}

trait Bet

