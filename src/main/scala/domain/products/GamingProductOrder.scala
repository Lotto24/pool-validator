package domain.products

import java.net.URI
import java.time.{DayOfWeek, LocalDate}
import java.time.format.DateTimeFormatter

import domain.products.GamingProduct.GamingProductId
import domain.products.ParticipationPools.ParticipationPoolId
import play.api.libs.json.JsObject


trait GamingProductOrder{
  def participationPools: ParticipationPools
  def variant: Option[String]
  def productURI: URI
  def bets : Seq[Bet]
  
  /** Used to enable the option of showing raw-data in the UI */
  def json : JsObject
  
  /** Delivers a copy with an empty `json` value (useful for tests).*/
  def withEmptyJson(): GamingProductOrder 
}

trait ParticipationPools {
  def firstDate: LocalDate

  def drawCount : Int
  
  def ids: Set[ParticipationPoolId]
}

trait ParticipationPoolsMultiplyDays extends ParticipationPools {
  def drawDays : Set[DayOfWeek]
}

object ParticipationPools {
  final type ParticipationPoolId = String

  private val ParticipationPoolIdPattern = "([^/]+)/(.+)".r
  private val ParticipationPoolIdDateFormat = DateTimeFormatter.ISO_LOCAL_DATE

  def formatParticipationPoolId(gamingProductId: GamingProductId, drawDate: LocalDate): ParticipationPoolId = {
    gamingProductId.name + "/" + ParticipationPoolIdDateFormat.format(drawDate)
  }

  def parseParticipationPoolId(id: ParticipationPoolId): (GamingProductId, LocalDate) = id match {
    case ParticipationPoolIdPattern(gamingProductId, drawDate) => (Symbol(gamingProductId), LocalDate.parse(drawDate, ParticipationPoolIdDateFormat))
  }
}

trait Bet

