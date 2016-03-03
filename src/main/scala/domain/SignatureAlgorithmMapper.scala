package domain

/**
  */
trait SignatureAlgorithmMapper {

  /**
    * Maps a symbolic signature algorithm name (e.g. 'rsa-sha256') to an algorithm `String` that can
    * be used to invoke `java.security.Signature.getInstance(String algorithm)` (e.g. 'SHA256withRSA').
    **/
  def mapAlgorithmName(symbolicName: String): Option[String]
}


object DefaultSignatureAlgMapper extends SignatureAlgorithmMapper {

  private val algMap = Map("rsa-sha256" -> "SHA256withRSA")

  override def mapAlgorithmName(symbolicName: String): Option[String] = algMap.get(symbolicName)

}