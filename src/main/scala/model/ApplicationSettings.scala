package model

import java.io.File
import java.text.DecimalFormat

import model.ApplicationSettings.CredentialsConfig
import util.Utils
import util.Utils.{ErrorMsg, UIValue}

case class ApplicationSettings(credentialsSpecs: CredentialsConfig,
                               archiveExtractionTarget: UIValue[File],
                               validatePoolOnLoading: Boolean,
                               showUIDebugControls: Boolean) {

  def errors: Seq[ErrorMsg] = archiveExtractionTarget.error.toSeq ++ credentialsSpecs.errors

  def isValid: Boolean = errors.isEmpty

}


object ApplicationSettings {


  case class CredentialsConfig(certConfigItems: Seq[CertificateCfgItem],
                               timestamperCertConfigItems : Seq[TimestamperCertCfgItem],
                               publicKeyConfigItems: Seq[PublicKeyCfgItem]) {

    def errors: Seq[ErrorMsg] = certConfigItems.flatMap(_.file.error) ++ publicKeyConfigItems.flatMap(_.file.error)

  }


  object CredentialsConfig {

    /**
      * Delivers a new unique "stableId" considering a list of existing items with a "stableId"
      * */
    def getNewUniqueStableId(existingItems: Seq[{def stableId : String}]): String = {
      val currentIds = existingItems.map(_.stableId.toInt)
      val newId_int = if (currentIds.isEmpty) 1 else (currentIds.max + 1)
      new DecimalFormat("00").format(newId_int)
    }

  }

  case class CertificateCfgItem(stableId: String, name: String, file: UIValue[File], description: Option[String])

  case class TimestamperCertCfgItem(stableId: String, priority : Int, name: String, file: UIValue[File], description: Option[String])

  case class PublicKeyCfgItem(stableId: String, keyId: String, algorithm: String, file: UIValue[File], description: Option[String])

}
