package domain

import java.security.PublicKey
import java.security.cert.X509Certificate

import domain.CredentialsManagerImpl.PublicKeyMapKey

import scala.util.{Failure, Success, Try}


trait CredentialsProvider {
  def getPublicKey(keyId: String, algorithm: String): Try[PublicKey]

  def getCertificate(name: String): Try[X509Certificate]

  def getTimestamperCertificates: Seq[X509Certificate]
}


object CredentialsProvider {
  val RootCaCertificate: String = "root-ca-cert"
}


trait CredentialsManager extends CredentialsProvider {

  /**
    * @return returns a copy of the KeyManager with the new `key` added.
    **/
  def withPublicKey(keyId: String, algorithm: String, key: Try[PublicKey]): CredentialsManager

  /**
    * @return returns a copy of the KeyManager with the new `certificate` added.
    **/
  def withCertificate(name: String, certificate: Try[X509Certificate]): CredentialsManager

  def withTimestamperCertificates(certs: Seq[X509Certificate]): CredentialsManager

  def withClearedPublicKeys(): CredentialsManager

  def withClearedCertificates(): CredentialsManager
}


object CredentialsManagerImpl {

  case class PublicKeyMapKey(keyId: String, algorithm: String)

}


class CredentialsManagerImpl(keys: Map[PublicKeyMapKey, Try[PublicKey]] = Map.empty,
                             certificates: Map[String, Try[X509Certificate]] = Map.empty,
                             timestamperCertificates: Seq[X509Certificate] = Seq.empty) extends CredentialsManager {

  override def getPublicKey(keyId: String, algorithm: String): Try[PublicKey] = {
    val mapKey = PublicKeyMapKey(keyId, algorithm)
    keys.get(mapKey) match {
      case Some(publicKey) => publicKey
      case _ => Failure(new Exception(s"no PublicKey found for keyId='${mapKey.keyId}', algorithm='${mapKey.algorithm}'"))
    }
  }

  override def getCertificate(name: String): Try[X509Certificate] = {
    certificates.get(name) match {
      case Some(certificate) => certificate
      case _ => Failure(new Exception(s"no Certificate found for $name"))
    }
  }

  override def withPublicKey(keyId: String, algorithm: String, key: Try[PublicKey]): CredentialsManagerImpl = {
    new CredentialsManagerImpl(keys = keys.updated(PublicKeyMapKey(keyId, algorithm), key),
      certificates = certificates, timestamperCertificates = timestamperCertificates)
  }

  override def getTimestamperCertificates: Seq[X509Certificate] = timestamperCertificates

  override def withCertificate(name: String, certificate: Try[X509Certificate]): CredentialsManagerImpl = {
    new CredentialsManagerImpl(keys,
      certificates = certificates.updated(name, certificate), timestamperCertificates = timestamperCertificates)
  }

  override def withTimestamperCertificates(certs: Seq[X509Certificate]): CredentialsManager = {
    new CredentialsManagerImpl(keys, certificates = certificates, timestamperCertificates = certs)
  }

  override def withClearedPublicKeys(): CredentialsManager = {
    new CredentialsManagerImpl(keys = Map.empty, certificates, timestamperCertificates = timestamperCertificates)
  }

  override def withClearedCertificates(): CredentialsManager = {
    new CredentialsManagerImpl(keys, certificates = Map.empty, timestamperCertificates = timestamperCertificates)
  }

}