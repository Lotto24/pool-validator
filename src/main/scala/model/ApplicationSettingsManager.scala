package model

import java.io.File
import java.net.URI

import com.typesafe.scalalogging.Logger
import model.ApplicationSettings._
import model.ApplicationSettingsManager.{LoadFailed, LoadResult, Loaded}
import org.apache.commons.configuration.PropertiesConfiguration
import org.slf4j.LoggerFactory
import util.Utils
import util.Utils.{ErrorMsg, UIValue}

import scala.collection.JavaConversions._
import scala.util.Try

/**
  */
trait ApplicationSettingsManager {
  def loadSettings(configSource: URI): LoadResult

  /** Takes a (possibly non-validated) `Settings`-instance, validates it and returns an enriched copy
    * containing the validation results. */
  def validateSettings(toBeValidated: ApplicationSettings): ApplicationSettings

  def saveSettings(settings: ApplicationSettings, target: URI): Try[Boolean]
}

object ApplicationSettingsManager {

  sealed trait LoadResult

  case class Loaded(settings: ApplicationSettings) extends LoadResult

  case class LoadFailed(errors: Seq[ErrorMsg]) extends LoadResult
}

abstract class ApplicationSettingsManagerAI extends ApplicationSettingsManager {

  /** Loads a partially validated `Settings`-instance. */
  def loadSettingsImpl(configSource: URI): LoadResult

  override def loadSettings(configSource: URI): LoadResult = {
    try{
      loadSettingsImpl(configSource: URI) match {
        case Loaded(partiallyValidatedSettings) =>
          Loaded(validateSettings(partiallyValidatedSettings))
        case failed: LoadFailed =>
          failed
      }
    } catch {
      case t : Throwable =>
        LoadFailed(Seq(ErrorMsg(message=s"Failed to load Settings:${t.getMessage}", exception = Some(t))))
    }
  }

  override def validateSettings(toBeValidated: ApplicationSettings): ApplicationSettings = {
    val credentialSpecs_validated = validateCredentialSpecs(toBeValidated.credentialsSpecs)

    val archiveExtractionTarget_validated = validateUIValue_file(toBeValidated.archiveExtractionTarget,
      checkIsWriteable = true, isDirectoryHint = true)

    toBeValidated.copy(
      credentialsSpecs = credentialSpecs_validated,
      archiveExtractionTarget = archiveExtractionTarget_validated
    )
  }

  private def validateCredentialSpecs(toBeValidated: CredentialsConfig): CredentialsConfig = {
    val keySpecs_validated: Seq[PublicKeyCfgItem] = {
      toBeValidated.publicKeyConfigItems.map{ keySpec =>
        keySpec.copy(file = validateUIValue_publicKeyPemFile(keySpec))
      }
    }

    val certSpecs_validated: Seq[CertificateCfgItem] = {
      toBeValidated.certConfigItems.map{ certSpec =>
        certSpec.copy(file = validateUIValue_X509CertPemFile(certSpec.file, name=certSpec.name))
      }
    }

    val tsCertSpecs_validated = toBeValidated.timestamperCertConfigItems.map{ certSpec =>
      certSpec.copy(file = validateUIValue_X509CertPemFile(certSpec.file, name=certSpec.name))
    }


    toBeValidated.copy(publicKeyConfigItems = keySpecs_validated, certConfigItems = certSpecs_validated,
      timestamperCertConfigItems = tsCertSpecs_validated)
  }

  protected def validateUIValue_publicKeyPemFile(toValidate: PublicKeyCfgItem): UIValue[File] = {
    validateUIValue_file(toValidate.file) match {
      case v if v.error.nonEmpty => v
      case v => Utils.isInvalidPublicKeyPemFile(v.value.get) match {
        case err@Some(error) =>
          val errTxt = s"Error with public key '${toValidate.keyId}' (file" +
            s": ${toValidate.file.value.map(_.toString).getOrElse("")}): ${error.message}"
          v.copy(error = Some(error.copy(message = errTxt)))
        case _ => v
      }
    }
  }

  protected def validateUIValue_X509CertPemFile(toValidate: UIValue[File], name: String): UIValue[File] = {
    validateUIValue_file(toValidate) match {
      case v if v.error.nonEmpty =>
        v
      case v => Utils.isInvalidX509CertPemFile(v.value.get) match {
        case err@Some(error) =>
          val errTxt = s"Error related to certificate '${name}' (file" +
            s": ${toValidate.value.map(_.toString).getOrElse("")}): ${error.message}"
          v.copy(error = Some(error.copy(message = errTxt)))
        case _ =>
          v
      }
    }
  }

  /**
    * @param checkIsWriteable if 'false' only the `File.canRead` is checked.
    **/
  protected def validateUIValue_file(toValidate: UIValue[File],
                                     checkIsWriteable: Boolean = true,
                                     isDirectoryHint: Boolean = false): UIValue[File] = {
    if (toValidate.error.nonEmpty) {
      toValidate //=> keep the original Error
    } else {
      if (toValidate.value.isEmpty || toValidate.value.get.toString.isEmpty) {
        UIValue(None, Some(ErrorMsg(s"filePath is empty!")))
      } else {
        val fCheckAccessibility = if (checkIsWriteable) Utils.isFileUnwriteable _ else Utils.isFileUnreadable _
        fCheckAccessibility(toValidate.value.get, isDirectoryHint) match {
          case Some(error) =>
            val msg = s"Invalid file '${toValidate.value.get.toString}'"
            UIValue(None, Some(ErrorMsg(msg, detail = Some(error))))
          case _ => UIValue(Some(toValidate.value.get))
        }
      }
    }
  }
}

class ApplicationSettingsManagerPropertyImpl extends ApplicationSettingsManagerAI {

  private val propKeyPrefix_publicKey = "publicKeys"
  private val propKeyPrefix_certificate = "certificates"
  private val propKeyPrefix_timstamperCertificate = "timestamper-certificates"
  private val logger = Logger(LoggerFactory.getLogger(this.getClass))

  override def loadSettingsImpl(configSource: URI): LoadResult = {
    try {
      val config: PropertiesConfiguration = new PropertiesConfiguration(configSource.toURL)
      val showDebugControls= config.getBoolean("ui.showDebugControls", false)
      val archiveExtractionTarget = getFileFromCfg(config, "archiveExtractionTarget")
      val credentialsSpecs = loadCredentialsSpecs(config)

      Loaded(ApplicationSettings(
        credentialsSpecs,
        archiveExtractionTarget = archiveExtractionTarget,
        showUIDebugControls = showDebugControls))
    } catch {
      case e: Exception => LoadFailed(Seq(ErrorMsg(e.getMessage, exception = Some(e))))
    }
  }

  /** Delivers a Property-key following a certain construction-pattern */
  private def getPropKey_publicKey(config: PropertiesConfiguration, keyIndex: String, field: PublicKeyField.Value): String = {
    s"${propKeyPrefix_publicKey}.${keyIndex}.${field.toString}"
  }

  /** Delivers a Property-key following a certain construction-pattern */
  private def getPropKey_certificate(config: PropertiesConfiguration, keyIndex: String, field: CertificateField.Value): String = {
    s"${propKeyPrefix_certificate}.${keyIndex}.${field.toString}"
  }

  /** Delivers a Property-key following a certain construction-pattern */
  private def getPropKey_timestamperCertificate(config: PropertiesConfiguration, keyIndex: String, field: TimestamperCertificateField.Value): String = {
    s"${propKeyPrefix_timstamperCertificate}.${keyIndex}.${field.toString}"
  }

  private def loadCredentialsSpecs(config: PropertiesConfiguration): CredentialsConfig = {
    val certificateSpecs = loadCertificateSpecs(config)
    val publicKeySpecs = loadPublicKeySpecs(config)
    val timestamperCertsSpecs = loadTimestamperCertSpecs(config)
    CredentialsConfig(certificateSpecs, timestamperCertsSpecs, publicKeySpecs)
  }

  /**
    * Delivers the set of `<stableId>` for a given `prefix` for the property-key schema `<prefix>.<stableId>.<someFieldKey>`.
    * */
  private def getStableIds(config: PropertiesConfiguration, prefix: String): Set[String] = {
    val allRelatedKeys = config.getKeys(prefix).toVector
    val stableIdSet = allRelatedKeys.flatMap{key =>
      key.isEmpty match {
        case true => None
        case false =>
          val sep = "."
          val remainingKey = key.substring(prefix.length)
          val index_start = if(remainingKey.startsWith(sep)) 1 else 0
          val index_pastEnd = if(remainingKey.startsWith(sep)) remainingKey.updated(0,'Z').indexOf(sep) else remainingKey.indexOf(sep)
          val result = remainingKey.substring(index_start, index_pastEnd)
          Some(result)
      }
    }.toSet
    stableIdSet
  }

  /**
    * storage-format: `<propKeyPrefix_certificate>.<stableId>.<PublicKeyField>`
    * */
  private def loadCertificateSpecs(config: PropertiesConfiguration): Seq[CertificateCfgItem] = {
    val stableIdSet = getStableIds(config, prefix=propKeyPrefix_certificate)
    val keySpecsSeq = stableIdSet.toSeq.flatMap{keyIndex =>
      val k_name = getPropKey_certificate(config, keyIndex, CertificateField.Name)
      val k_description = getPropKey_certificate(config, keyIndex, CertificateField.Description)
      val k_file = getPropKey_certificate(config, keyIndex, CertificateField.FILE)
      for(
        name <- Option(config.getString(k_name))
      ) yield {
        CertificateCfgItem(stableId=keyIndex, name=name, file = getFileFromCfg(config, k_file),
          description=Option(config.getString(k_description))
        )
      }
    }
    keySpecsSeq
  }


  /**
    * storage-format: `<propKeyPrefix_publicKey>.<stableId>.<PublicKeyField>`
    * */
  private def loadPublicKeySpecs(config: PropertiesConfiguration): Seq[PublicKeyCfgItem] = {
    val stableIdSet = getStableIds(config, prefix=propKeyPrefix_publicKey)
    val result = stableIdSet.toSeq.flatMap{keyIndex =>
      val k_id = getPropKey_publicKey(config, keyIndex, PublicKeyField.Id)
      val k_algo = getPropKey_publicKey(config, keyIndex, PublicKeyField.Algorithm)
      val k_description = getPropKey_publicKey(config, keyIndex, PublicKeyField.Description)
      val k_file = getPropKey_publicKey(config, keyIndex, PublicKeyField.FILE)
      for(
        id <- Option(config.getString(k_id));
        algo <- Option(config.getString(k_algo))
      ) yield {
        PublicKeyCfgItem(stableId=keyIndex, keyId=id, algorithm = algo, file=getFileFromCfg(config, k_file),
          description=Option(config.getString(k_description))
        )
      }
    }
    result
  }

  private def loadTimestamperCertSpecs(config: PropertiesConfiguration): Seq[TimestamperCertCfgItem] = {
    val stableIdSet = getStableIds(config, prefix=propKeyPrefix_timstamperCertificate)
    val result = stableIdSet.toSeq.flatMap{keyIndex =>
      val k_name = getPropKey_timestamperCertificate(config, keyIndex, TimestamperCertificateField.Name)
      val k_priority = getPropKey_timestamperCertificate(config, keyIndex, TimestamperCertificateField.Priority)
      val k_description = getPropKey_timestamperCertificate(config, keyIndex, TimestamperCertificateField.Description)
      val k_file = getPropKey_timestamperCertificate(config, keyIndex, TimestamperCertificateField.FILE)
      for(
        name <- Option(config.getString(k_name))
      ) yield {
        TimestamperCertCfgItem(stableId=keyIndex, name=name, file = getFileFromCfg(config, k_file),
          priority = config.getInt(k_priority, 0), description=Option(config.getString(k_description))
        )
      }
    }
    result
  }

  private def saveCredentialsSpecs(config: PropertiesConfiguration, credentialsSpecs: CredentialsConfig): Unit = {
    saveAllCertificateSpecs(config, credentialsSpecs.certConfigItems)
    saveAllPublicKeySpecs(config, credentialsSpecs.publicKeyConfigItems)
    saveAllTimestamperCertSpecs(config, credentialsSpecs.timestamperCertConfigItems)
  }

  private def saveAllPublicKeySpecs(config: PropertiesConfiguration, keySpecs: Seq[PublicKeyCfgItem]): Unit = {
    keySpecs.foreach{keySpec =>
      savePublicKeySpec(config, keySpec)
    }
    deleteOrphanedProperties(config, propKeyPrefix=propKeyPrefix_publicKey,
      stillNeededKeys=keySpecs.map(x =>  s"${propKeyPrefix_publicKey}.${x.stableId}"))
  }

  private def saveAllCertificateSpecs(config: PropertiesConfiguration, certSpecs: Seq[CertificateCfgItem]): Unit = {
    println(s"saveAllCertificateSpecs()..#certSpecs: ${certSpecs.size}")

    certSpecs.foreach{certSpec =>
      saveCertificateSpec(config, certSpec)
    }
    deleteOrphanedProperties(config, propKeyPrefix=propKeyPrefix_certificate,
      stillNeededKeys=certSpecs.map(x =>  s"${propKeyPrefix_certificate}.${x.stableId}"))
  }

  private def saveAllTimestamperCertSpecs(config: PropertiesConfiguration, certSpecs : Seq[TimestamperCertCfgItem]) : Unit = {
    println(s"saveAllTimestamperCertSpecs()..#certSpecs: ${certSpecs.size}")
    require(certSpecs.map(_.priority).toSet.size == certSpecs.size,
      s"certSpecs.map(_.position) must not contain duplicates: ${certSpecs.map(_.priority)}")
    certSpecs.sortBy(_.priority).foreach{ certSpec =>
      saveTimestamperCertSpec(config, certSpec)
    }
    deleteOrphanedProperties(config, propKeyPrefix=propKeyPrefix_timstamperCertificate,
      stillNeededKeys=certSpecs.map(x =>  s"${propKeyPrefix_timstamperCertificate}.${x.stableId}"))
  }

  private def deleteOrphanedProperties(config: PropertiesConfiguration, propKeyPrefix: String, stillNeededKeys: Seq[String]): Unit = {
    val allRelatedKeys = config.getKeys(propKeyPrefix).toVector
    val keysToDelete = allRelatedKeys.filter(x => ! stillNeededKeys.exists(n => x.startsWith(n)))
    keysToDelete.foreach{key =>
      config.clearProperty(key)
    }
  }

  private def saveCertificateSpec(config: PropertiesConfiguration, certSpec: CertificateCfgItem): Unit = {
    config.setProperty(getPropKey_certificate(config, certSpec.stableId, CertificateField.Name), certSpec.name)
    config.setProperty(getPropKey_certificate(config, certSpec.stableId, CertificateField.FILE), certSpec.file.value.map(_.toString).getOrElse(""))
    config.setProperty(getPropKey_certificate(config, certSpec.stableId, CertificateField.Description), certSpec.description.getOrElse(""))
  }

  private def savePublicKeySpec(config: PropertiesConfiguration, keySpec: PublicKeyCfgItem): Unit = {
    config.setProperty(getPropKey_publicKey(config, keySpec.stableId, PublicKeyField.Id), keySpec.keyId)
    config.setProperty(getPropKey_publicKey(config, keySpec.stableId, PublicKeyField.Algorithm), keySpec.algorithm)
    config.setProperty(getPropKey_publicKey(config, keySpec.stableId, PublicKeyField.FILE), keySpec.file.value.map(_.toString).getOrElse(""))
    config.setProperty(getPropKey_publicKey(config, keySpec.stableId, PublicKeyField.Description), keySpec.description.getOrElse(""))
  }

  private def saveTimestamperCertSpec(config: PropertiesConfiguration, certSpec : TimestamperCertCfgItem) : Unit = {
    config.setProperty(getPropKey_timestamperCertificate(config, certSpec.stableId, TimestamperCertificateField.Name), certSpec.name)
    config.setProperty(getPropKey_timestamperCertificate(config, certSpec.stableId, TimestamperCertificateField.FILE), certSpec.file.value.map(_.toString).getOrElse(""))
    config.setProperty(getPropKey_timestamperCertificate(config, certSpec.stableId, TimestamperCertificateField.Priority), certSpec.priority.toString)
    config.setProperty(getPropKey_timestamperCertificate(config, certSpec.stableId, TimestamperCertificateField.Description), certSpec.description.getOrElse(""))
  }

  private def getFileFromCfg(config: PropertiesConfiguration, key: String): UIValue[File] = {
    Option(config.getProperty(key)) match {
      case Some(filePath: String) => UIValue(Some(new File(filePath)))
      case _ => UIValue(None, Some(ErrorMsg(s"Key not found: '$key'")))
    }
  }

  override def saveSettings(settings: ApplicationSettings, target: URI): Try[Boolean] = {
    Try {
      val config: PropertiesConfiguration = new PropertiesConfiguration(target.toURL)
      config.setProperty("archiveExtractionTarget",
        settings.archiveExtractionTarget.value.map(_.toString).getOrElse(""))
      saveCredentialsSpecs(config, settings.credentialsSpecs)
      logger.info(s"saveSettings()..location:${target.toURL}")
      config.save(target.toURL)
      true
    }
  }

  // ===================================================================================================================
  // Property key field constants
  // ===================================================================================================================

  private object  PublicKeyField extends Enumeration {
    val Id = Value(0, "id")
    val Algorithm = Value(1,"algorithm")
    val FILE = Value(2,"file")
    val Description = Value(3, "description")
  }

  private object  CertificateField extends Enumeration {
    val Name = Value(0, "id")
    val FILE = Value(1,"file")
    val Description = Value(2, "description")
  }

  private object  TimestamperCertificateField extends Enumeration {
    val Name = Value(0, "id")
    val FILE = Value(1,"file")
    val Priority = Value(2, "priority")
    val Description = Value(3, "description")
  }

}

