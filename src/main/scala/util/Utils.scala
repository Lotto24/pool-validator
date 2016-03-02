package util

import java.io._
import java.security.PublicKey
import java.security.cert.{CertificateFactory, X509Certificate}

import org.apache.commons.io.IOUtils
import org.apache.commons.io.filefilter.DirectoryFileFilter
import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo
import org.bouncycastle.openssl.PEMParser
import org.bouncycastle.openssl.jcajce.JcaPEMKeyConverter

import scala.util.{Failure, Success, Try}


object Utils {


  def getAsSeq(is: InputStream, closeStream: Boolean = true): Try[IndexedSeq[Byte]] = {
    Try {
      val result = IOUtils.toByteArray(is).toIndexedSeq
      if (closeStream)
        is.close()
      result
    }
  }


  def readPublicKeyFromPemFile(pemFile: File): Try[PublicKey] = {
    tryFileReadable(pemFile).flatMap{file =>
      val fis = new FileInputStream(pemFile)
      val pemParser = new PEMParser(new InputStreamReader(fis))
      val r = pemParser.readObject() match {
        case pki: SubjectPublicKeyInfo =>
          val keyBytes = pki.parsePublicKey().getEncoded // X.509 for public key
          val c = new JcaPEMKeyConverter
          Success(c.getPublicKey(pki))
        case x =>
          Failure(new Exception(s"unexpected object: ${x}"))
      }
      fis.close()
      r
    }
  }

  def readX509CertificateFromPemFile(pemFile : File): Try[X509Certificate] = {
    tryFileReadable(pemFile).map{file =>
      val fis = new FileInputStream(file)
      val cf = CertificateFactory.getInstance("X.509")
      val r = cf.generateCertificate(fis).asInstanceOf[X509Certificate]
      fis.close()
      r
    }
  }

  val DirectoryFilter = DirectoryFileFilter.DIRECTORY.asInstanceOf[FileFilter]

  implicit def fileInputStreamOf(file: File): FileInputStream = new FileInputStream(file)

  /** @return None indicates the File is readable, Some(string) indicates some error. */
  def isFileUnreadable(file: File, isDirectoryHint: Boolean = false): Option[String] = {
    if (!file.exists())
      return Some(s"${if (isDirectoryHint) "Directory" else "File"} ${file.toString} does not exist")
    if (!file.canRead)
      return Some(s"${if (file.isDirectory) "Directory" else "File"} ${file.toString} is not readable")
    None
  }

  def tryFileReadable(file: File, isDirectoryHint: Boolean = false): Try[File] = isFileUnreadable(file, isDirectoryHint) match {
    case Some(error) => Failure(new Exception(error))
    case _ => Success(file)
  }

  /** @return None indicates the File is readable, Some(string) indicates some error. */
  def isFileUnwriteable(file: File, isDirectoryHint: Boolean = false): Option[String] = {
    if (!file.exists())
      return Some(s"${if (isDirectoryHint) "Directory" else "File"} ${file.toString} does not exist")
    if (!file.canWrite)
      return Some(s"${if (file.isDirectory) "Directory" else "File"} ${file.toString} is not writable")
    None
  }

  /** @return None indicates the File is a valid X509Certificate pem-file, Some(string) indicates some error. */
  def isInvalidX509CertPemFile(file: File): Option[ErrorMsg] = {
    Utils.isFileUnreadable(file) match {
      case e@Some(error) => Some(ErrorMsg(error, detail = None))
      case _ =>
        Utils.readX509CertificateFromPemFile(file) match {
          case Failure(t) => Some(ErrorMsg("No X509Certificate .pem file", detail = Option(t.getMessage), exception = Some(t)))
          case Success(_) => None
        }
    }
  }

  /** @return None indicates the File is a valid X509Certificate pem-file, Some(string) indicates some error. */
  def isInvalidPublicKeyPemFile(file: File): Option[ErrorMsg] = {
    Utils.isFileUnreadable(file) match {
      case e@Some(error) => Some(ErrorMsg(error, detail = None))
      case _ =>
        Utils.readPublicKeyFromPemFile(file) match {
          case Failure(t) => Option(ErrorMsg("No PublicKey .pem file", detail = Some(t.getMessage), exception = Some(t)))
          case Success(_) => None
        }
    }
  }

  def stringToOption(string:  String): Option[String] = if( (string == null) || string.isEmpty) None else Some(string)

  def mkFailure[T](msg: String): Failure[T] = new Failure[T](new Exception(msg))

  case class ErrorMsg(message: String, detail: Option[String] = None, exception: Option[Throwable] = None)

  case class UIValue[T](value: Option[T], error: Option[ErrorMsg] = None) {
    def isValid: Boolean = error.isEmpty
  }

}
