package domain

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.security.cert.X509Certificate
import java.security.{MessageDigest, PublicKey, Signature}
import java.time._
import java.util
import java.util.Base64
import java.util.concurrent.CancellationException
import java.util.concurrent.atomic.AtomicInteger

import _root_.util.Task.CancellableSupplierAI
import _root_.util.{Task, TaskImpl}
import com.typesafe.scalalogging.Logger
import domain.PoolMetadata.PoolDigest
import domain.PoolResource.{Filenames, PRType}
import domain.PoolValidator._
import domain.products.GamingProduct._
import domain.products.ParticipationPools.parseParticipationPoolId
import org.apache.commons.codec.binary.{Base64 => Base64_AC}
import org.bouncycastle.asn1.DEROctetString
import org.bouncycastle.asn1.x509.{ExtendedKeyUsage, Extension, KeyPurposeId}
import org.bouncycastle.cert.X509CertificateHolder
import org.bouncycastle.cms.SignerInformation
import org.bouncycastle.cms.jcajce.JcaSimpleSignerInfoVerifierBuilder
import org.bouncycastle.operator.jcajce.JcaContentVerifierProviderBuilder
import org.bouncycastle.tsp.{TimeStampRequest, TimeStampRequestGenerator, TimeStampResponse, TimeStampToken}
import org.bouncycastle.util.Selector
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._
import scala.collection.parallel.ParSeq
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}


trait PoolValidator {

  /**
    * Setter for a `CredentialsManager` which is used to obtain PublicKeys and Certificates during the validation.
    **/
  def setCredentialsProvider(credentialsProvider: CredentialsProvider): this.type

  /**
    * Validates a single order. The according order-documents are expected to be within the given `orderDirPath`.
    **/
  def validateOrder(orderDirPath: Path, drawTime: Try[Instant]): OrderValidationResult

  /**
    * Validates a participation pool at the given poolLocation.
    **/
  def validatePool(poolLocation: Path, drawTime: Try[Instant],
                   intermediateResultCallback: IntermediateResultCallback): Task[ArchiveValidationResult]
}


object PoolValidator {

  trait IntermediateResultCallback {

    def onOrderValidated(result: OrderValidationResult, countValidatedOrders: Int)

  }


  sealed trait ValidationResult {

    def isValid: Boolean

  }


  case class OrderValidationResult(orderLocation: Path, checkResults: IndexedSeq[CheckResult]) extends ValidationResult {

    override def isValid: Boolean = checkResults.forall(_.isOk)

    def orderId: String = orderLocation.getFileName.toString

  }


  case class ArchiveValidationResult(poolLocation: Path,
                                     poolValidationResults: IndexedSeq[CheckResult],
                                     orderValidationResults: IndexedSeq[OrderValidationResult]) extends ValidationResult {

    override def isValid: Boolean = poolValidationResults.forall(_.isOk) && orderValidationResults.forall(_.isValid)

  }

  class UnexpectedValueException[T](msg: String, expected: Option[T], actual: Option[T]) extends Exception(msg)


  trait Check {

    def description: String

    def affectedResources: Set[PRType.Value]

  }

  /**
    * Description class for checks concerned to a single order.
    **/
  case class OrderCheck(description: String, affectedResources: Set[PRType.Value]) extends Check

  /**
    * Description class for checks concerned to the participation pool.
    **/
  case class PoolCheck(description: String, affectedResources: Set[PRType.Value]) extends Check


  // pool validation checks
  val PoolDigestCheck = PoolCheck(description = "The participation pool digest must match the included orders",
    affectedResources = Set(PRType.MetaData, PRType.PoolDirectory))
  val PoolDigestTimestampExistsCheck = PoolCheck(description = s"Document ${Filenames.PoolDigestTimestamp} must exist",
    affectedResources = Set(PRType.PoolDigestTimestamp))
  val PoolDigestTimestampAuthCheck = PoolCheck(description = "The participation pool digest's timestamp must be authentic",
    affectedResources = Set(PRType.PoolDigestTimestamp))
  val PoolDigestTimestampResultSignatureCheck = PoolCheck(description = "The participation pool digest's timestamp must match the pool digest",
    affectedResources = Set(PRType.PoolDigestTimestamp)
  )
  val PoolDigestTimestampBeforeDrawTimeCheck = PoolCheck(description = "The participation pool digest's timestamp must be created before draw time",
    affectedResources = Set(PRType.PoolDigestTimestamp)
  )


  // order validation checks
  val AllOrderDocumentsMustExistCheck = OrderCheck(description = "All order documents must exist",
    affectedResources = Set(PRType.OrderDirectory, PRType.Order, PRType.OrderSignature,
      PRType.OrderResult, PRType.OrderResultSignature, PRType.OrderResultSignatureTimestamp)
  )
  val RetailerOrderSignatureCheck = OrderCheck(description = "Retailer order signature must be valid",
    affectedResources = Set(PRType.Order, PRType.OrderSignature))
  val OrderHashMatchesOrderDirectoryCheck = OrderCheck(description = "Computed order-hash must match the order directory name",
    affectedResources = Set(PRType.OrderDirectory, PRType.Order))
  val PoolParticipationCheck = OrderCheck(description = "Order must participate in pool",
    affectedResources = Set(PRType.Order, PRType.MetaData))
  val CheckOrderResultOperatorSignature = OrderCheck(description = "Order document must match retailer order",
    affectedResources = Set(PRType.Order, PRType.OrderResult))
  val OrderAcceptedCheck = OrderCheck(description = "Order must be accepted",
    affectedResources = Set(PRType.OrderResult))
  val OrderResultSignatureValidCheck = OrderCheck(description = "Order document signature must be valid",
    affectedResources = Set(PRType.OrderResult, PRType.OrderSignature))

  val OrderResultSignatureTimestampAuthCheck = OrderCheck(description = "Order document signature timestamp must be authentic",
    affectedResources = Set(PRType.OrderResultSignatureTimestamp))
  val OrderResultSignatureTimestampResultSignatureCheck = OrderCheck(description = "Order document signature timestamp must match order document signature",
    affectedResources = Set(PRType.OrderResultSignature, PRType.OrderResultSignatureTimestamp))
  val OrderResultSignatureTimestampBeforeDrawTimeCheck = OrderCheck(description = "Order document signature timestamp must be created before draw time",
    affectedResources = Set(PRType.OrderResultSignatureTimestamp))

  val allOrderRelatedChecks: Seq[OrderCheck] = Vector(
    AllOrderDocumentsMustExistCheck,
    RetailerOrderSignatureCheck,
    OrderHashMatchesOrderDirectoryCheck,
    PoolParticipationCheck,
    CheckOrderResultOperatorSignature,
    OrderAcceptedCheck,
    OrderResultSignatureValidCheck,
    OrderResultSignatureTimestampAuthCheck,
    OrderResultSignatureTimestampResultSignatureCheck,
    OrderResultSignatureTimestampBeforeDrawTimeCheck
  )

  val allPoolRelatedChecks: Seq[PoolCheck] = Vector(
    PoolDigestCheck,
    PoolDigestTimestampExistsCheck,
    PoolDigestTimestampAuthCheck,
    PoolDigestTimestampResultSignatureCheck,
    PoolDigestTimestampBeforeDrawTimeCheck
  )

  val allChecks: IndexedSeq[Check] = (allPoolRelatedChecks ++ allOrderRelatedChecks).toVector


  sealed trait CheckResult {
    def check: Check

    def isOk: Boolean
  }


  case class CheckOk(check: Check) extends CheckResult {
    override def isOk: Boolean = true
  }


  case class CheckFailure(check: Check,
                          message: String,
                          exception: Option[Throwable] = None) extends CheckResult {
    override val isOk: Boolean = false
  }

}


class PoolValidatorImpl(resourceProvider: PoolResourceProvider,
                        implicit val ec: ExecutionContext,
                        private var credentialsProvider: CredentialsProvider) extends PoolValidator {


  private val logger = Logger(LoggerFactory.getLogger(this.getClass))

  //Base64.Encoder are safe for use by multiple concurrent threads.
  private val base64UrlEncoder = Base64.getUrlEncoder
  private val base64UrlEncoderWithoutPadding = Base64.getUrlEncoder.withoutPadding()
  private val base64Encoder: Base64.Encoder = Base64.getEncoder
  private val base64EncoderWithoutPadding: Base64.Encoder = Base64.getEncoder.withoutPadding()

  override def setCredentialsProvider(provider: CredentialsProvider): this.type = {
    this.credentialsProvider = provider
    this
  }

  private def isBase64StringURLEncoded(base64String: String) = {
    !base64String.contains('+') && !base64String.contains('/')
  }

  private def hasBASE64StringPadding(base64String: String) = {
    base64String.endsWith("=")
  }

  private[domain] def sha256UrlSafe(data: String): String = {
    val md = MessageDigest.getInstance("SHA-256")
    val digest = md.digest(data.getBytes(StandardCharsets.UTF_8))
    new String(base64UrlEncoder.encode(digest))
  }

  private[domain] def sha256AsBase64(data: IndexedSeq[Byte], urlSafe: Boolean, withPadding: Boolean): String = {
    val md = MessageDigest.getInstance("SHA-256")
    val digest = md.digest(data.toArray)
    val enc = {
      if (urlSafe) {
        if (withPadding) base64UrlEncoder else base64UrlEncoderWithoutPadding
      } else {
        if (withPadding) base64Encoder else base64EncoderWithoutPadding
      }
    }
    new String(enc.encode(digest))
  }

  def checkPoolParticipation(order: Try[Order], metainfo: Try[PoolMetadata]): CheckResult = {
    if (order.isFailure || metainfo.isFailure) {
      val errors = order.failed.toOption.map(_ => s"Missing resource: ${Filenames.Order}") ++
        metainfo.failed.toOption.map(_ => s"Missing resource: ${Filenames.Metadata}")
      CheckFailure(PoolParticipationCheck, errors.mkString(", "))
    } else {
      val (gamingProductId, _) = parseParticipationPoolId(metainfo.get.participationPoolId)
      val participationPoolId = metainfo.get.participationPoolId

      val matchingProductOrderResults = order.get.gamingProductOrders.collect {
        case (gamingProductUri, productOrder) if gamingProductIdFromURI(gamingProductUri) == gamingProductId =>
          if (productOrder.participationPools.ids.contains(participationPoolId)) {
            CheckOk(PoolParticipationCheck)
          } else {
            CheckFailure(PoolParticipationCheck, "Order does not participate in pool.")
          }
      }
      if (matchingProductOrderResults.isEmpty)
        CheckFailure(PoolParticipationCheck, s"Order does not participate in pool (No product-order found for id=${gamingProductId})")
      else
        matchingProductOrderResults.head //ok, since there should be exactly one or no product-order per gamingProduct-URI
    }
  }

  /**
    * Compute hash of order.json and see if matches the directory hash.
    **/
  private[domain] def checkOrderHashMatchesDirectoryName(orderData: Try[IndexedSeq[Byte]], directoryName: String): CheckResult = {
    orderData match {
      case Success(data) =>
        val orderDirNameHasBASE64padding = hasBASE64StringPadding(directoryName)
        val orderSha = sha256AsBase64(data, urlSafe = true, withPadding = orderDirNameHasBASE64padding)
        if (directoryName == orderSha)
          CheckOk(PoolValidator.OrderHashMatchesOrderDirectoryCheck)
        else {
          CheckFailure(PoolValidator.OrderHashMatchesOrderDirectoryCheck,
            message = s"order.directoryName (${
              directoryName
            }) != orderSha (${
              orderSha
            })",
            exception = Some(new UnexpectedValueException(msg = "orderSha != directoryName",
              expected = Option(orderSha), actual = Option(directoryName)))
          )
        }
      case Failure(t) =>
        CheckFailure(PoolValidator.OrderHashMatchesOrderDirectoryCheck,
          message = s"order data is missing or invalid: ${t.getMessage}",
          exception = Some(t)
        )
    }
  }

  /**
    * Check signature in order.signature and check order using the retailer's public key
    *
    * @param orderSignature order signature (BASE64-encoded)
    **/
  private[domain] def checkRetailerSignature(orderSignature: Try[OrderSignature], order: Try[Order],
                                             retailerPublicKey: Try[PublicKey]): CheckResult = {

    if (orderSignature.isFailure || order.isFailure || retailerPublicKey.isFailure) {
      val errors = Seq(
        orderSignature.failed.toOption.map(_ => s"Missing resource: ${Filenames.OrderSignature}"),
        order.failed.toOption.map(_ => s"Missing resource: ${Filenames.Order}"),
        retailerPublicKey.failed.toOption.map(e => s"Missing retailer public key: ${e.getMessage}")
      )
      CheckFailure(RetailerOrderSignatureCheck, message = errors.mkString(", "))
    } else {
      try {
        val signature_raw = Base64_AC.decodeBase64(orderSignature.get.signature.toArray) //=> Base64_AC ignores " "
        val sig = Signature.getInstance("SHA256withRSA")
        sig.initVerify(retailerPublicKey.get)
        sig.update(order.get.rawData.toArray)
        if (sig.verify(signature_raw))
          CheckOk(RetailerOrderSignatureCheck)
        else
          CheckFailure(RetailerOrderSignatureCheck, message = "Verification of signature failed")
      } catch {
        case e: Exception => {
          CheckFailure(RetailerOrderSignatureCheck,
            message = s"Verification of signature failed: ${e.getMessage}", exception = Option(e))
        }
      }
    }
  }

  /**
    * Extract the hash in the order.result document and compare it to the actual order
    **/
  private[domain] def checkOrderResult(order: Try[Order], result: Try[OrderResult]): CheckResult = {
    val argsWithFailure = Seq(order, result).filter(_.isFailure)
    if (argsWithFailure.nonEmpty) {
      val errors = order.failed.toOption.map(_ => s"Missing resource: ${Filenames.Order}") ++
        result.failed.toOption.map(_ => s"Missing resource: ${Filenames.OrderResult}")
      CheckFailure(RetailerOrderSignatureCheck, message = errors.mkString(", "))
    } else {
      val orderDigest_noPrefix = new String(result.get.orderDigest.toArray, StandardCharsets.UTF_8).replaceAll("^sha256:", "").replaceAll("^sha256=", "")
      val orderDigest_hasPadding = hasBASE64StringPadding(orderDigest_noPrefix)
      val orderDigest_isUrlEncoded = isBase64StringURLEncoded(orderDigest_noPrefix)

      val orderDataHash = sha256AsBase64(order.get.rawData, urlSafe = orderDigest_isUrlEncoded, withPadding = orderDigest_hasPadding)

      if (orderDataHash != orderDigest_noPrefix) {
        CheckFailure(CheckOrderResultOperatorSignature, message = "signature(order) != result.orderDigest",
          exception = Some(new UnexpectedValueException(msg = "signature(order) != result.orderDigest",
            expected = Some(orderDataHash), actual = Some(orderDigest_noPrefix))))
      } else {
        CheckOk(CheckOrderResultOperatorSignature)
      }
    }
  }

  private[domain] def checkOrderIsAccepted(result: Try[OrderResult]): CheckResult = {
    result.failed.toOption.map(t => s"Document ${Filenames.OrderResult} is not available: ${t.getMessage}") match {
      case Some(error) => CheckFailure(OrderAcceptedCheck, message = error)
      case _ =>
        if (result.get.orderProcessingResult != OrderResult.Accepted) {
          CheckFailure(OrderAcceptedCheck, message = s"order processing result is ${result.get.orderProcessingResult}")
        } else {
          CheckOk(OrderAcceptedCheck)
        }
    }
  }

  /** Extract signature from order.result.signature
    *
    * @param signature signature bytes to be verified.
    * */
  private[domain] def checkOperatorSignature(signature: Try[IndexedSeq[Byte]],
                                             operatorPubKey: Try[PublicKey],
                                             data: Try[IndexedSeq[Byte]]): CheckResult = {
    val errors = signature.failed.toOption.map(t => s"Missing operator signature: ${t.getMessage}") ++
      operatorPubKey.failed.toOption.map(t => s"Missing operator public key: ${t.getMessage}") ++
      data.failed.toOption.map(t => s"Missing order data: ${t.getMessage}")
    if (errors.nonEmpty) {
      CheckFailure(OrderResultSignatureValidCheck, message = errors.mkString("\n"))
    } else {
      try {
        val sig = Signature.getInstance("SHA256withRSA")
        sig.initVerify(operatorPubKey.get)
        sig.update(data.get.toArray)
        if (sig.verify(signature.get.toArray))
          CheckOk(OrderResultSignatureValidCheck)
        else {
          CheckFailure(OrderResultSignatureValidCheck, message = "Verification of signature failed")
        }
      } catch {
        case e: Exception => CheckFailure(OrderResultSignatureValidCheck,
          message = s"Verification of signature failed:${e.getMessage}", exception = Option(e))
      }
    }
  }


  /**
    * Checks the validity of the timestamp of a single order.
    **/
  private[domain] def checkOrderTimestamp(timestampBase64: Try[IndexedSeq[Byte]],
                                          drawTime: Try[Instant],
                                          orderResultSignature: Try[OrderResultSignature],
                                          caCertificates: Seq[X509Certificate]): Seq[CheckResult] = {
    val signature = orderResultSignature.map(_.signature)
    checkTimestampImpl(timestampBase64 = timestampBase64, drawTime = drawTime, signature = signature, caCertificates = caCertificates,
      timestampAuthCheck = OrderResultSignatureTimestampAuthCheck,
      timestampResultSignatureCheck = OrderResultSignatureTimestampResultSignatureCheck,
      timestampBeforeDrawtimeCheck = OrderResultSignatureTimestampBeforeDrawTimeCheck
    )
  }

  /**
    * Checks the validity of the timestamp for a participation pool seal.
    **/
  private[domain] def checkPoolDigestTimestamp(timestampBase64: Try[IndexedSeq[Byte]],
                                               drawtime: Try[Instant],
                                               signature: Try[IndexedSeq[Byte]],
                                               caCertificates: Seq[X509Certificate]): Seq[CheckResult] = {
    checkTimestampImpl(timestampBase64 = timestampBase64, drawTime = drawtime, signature = signature, caCertificates = caCertificates,
      timestampAuthCheck = PoolDigestTimestampAuthCheck,
      timestampResultSignatureCheck = PoolDigestTimestampResultSignatureCheck,
      timestampBeforeDrawtimeCheck = PoolDigestTimestampBeforeDrawTimeCheck
    )
  }

  /**
    * Checks the authenticity of the timestamper respone similar to openssl ts:
    * `openssl ts -verify -data order.result.signature.raw \
    * -CAfile ca-cert.pem \
    * -in order.result.signature.timestamp.raw`
    *
    * @param timestampBase64 BASE64 encoded timestamp response (ASN.1)
    * @param signature       BASE64 encoded signature that should have been timestamped
    * @param caCertificates  a collection of trusted ca-certificates - the TSA-certificate used to create the timestamp
    *                        should be signed by at least one of them in order be be valid.
    **/
  private[domain] def checkTimestampImpl(timestampBase64: Try[IndexedSeq[Byte]],
                                         drawTime: Try[Instant],
                                         signature: Try[IndexedSeq[Byte]],
                                         caCertificates: Seq[X509Certificate],
                                         timestampAuthCheck: Check,
                                         timestampResultSignatureCheck: Check,
                                         timestampBeforeDrawtimeCheck: Check
                                        ): Seq[CheckResult] = {
    if (timestampBase64.isFailure) {
      val errors = {
        timestampBase64.failed.toOption.map(t => s"Missing TimestampResponse data: ${t.getMessage}") ++
          drawTime.failed.toOption.map(t => s"Could not obtain drawtime: ${t.getMessage}") ++
          signature.failed.toOption.map(t => s"Missing signature: ${t.getMessage}")
      } ++ (if (caCertificates.isEmpty) Some("No certificates provided") else None).toList
      val msg = errors.mkString(", ")
      Seq(
        CheckFailure(timestampResultSignatureCheck, message = msg),
        CheckFailure(timestampAuthCheck, message = msg),
        CheckFailure(timestampBeforeDrawtimeCheck, message = msg)
      )
    } else {
      try {

        //deserialize the tsaResponse (ASN.1 data)
        val tsResponse: TimeStampResponse = try {
          val tsaResponse: Array[Byte] = Base64_AC.decodeBase64(timestampBase64.get.toArray)
          new TimeStampResponse(tsaResponse.toArray)
        } catch {
          case t: Throwable => throw new Exception(s"Error parsing TimestampResponse data: ${t.getMessage}")
        }

        val timestampBeforeDrawTimeCheckResult = drawTime match {
          case Success(drawTime) =>
            val timestampTime = tsResponse.getTimeStampToken.getTimeStampInfo.getGenTime.toInstant
            if (timestampTime isAfter drawTime)
              CheckFailure(timestampBeforeDrawtimeCheck, s"timestamp-time: ${timestampTime}, draw time: ${drawTime}")
            else
              CheckOk(timestampBeforeDrawtimeCheck)
          case Failure(t) =>
            CheckFailure(timestampBeforeDrawtimeCheck, message = s"failed to obtain drawtime: ${t.getMessage}", exception = Some(t))
        }

        val timestampToken: TimeStampToken = tsResponse.getTimeStampToken

        //(Re-)create a TimeStampRequest based on the known data & parameters
        val tsRequest: Try[TimeStampRequest] = {
          signature.map { sig =>
            val hashAlgo = tsResponse.getTimeStampToken.getTimeStampInfo.getHashAlgorithm
            val messageDigest = MessageDigest.getInstance(hashAlgo.getAlgorithm.getId)
            val orderResultSignature_raw = Base64_AC.decodeBase64(sig.toArray)
            val hashedData = messageDigest.digest(orderResultSignature_raw.toArray)
            val gen = new TimeStampRequestGenerator()
            gen.generate(hashAlgo.getAlgorithm(), hashedData, tsResponse.getTimeStampToken.getTimeStampInfo.getNonce)
          }
        }

        //Validate the received TimeStampResponse against the recreated TimeStampRequest
        val timestampResultSignatureCheckResult = tsRequest match {
          case Success(tsReq) =>
            try {
              tsResponse.validate(tsReq)
              CheckOk(timestampResultSignatureCheck)
            } catch {
              case t: Throwable =>
                CheckFailure(timestampResultSignatureCheck, message = t.getMessage, exception = Some(t))
            }
          case Failure(t) =>
            CheckFailure(timestampResultSignatureCheck,
              message = s"Invalid or missing TimestampRequest: ${t.getMessage}",
              exception = Some(t))
        }

        //Get the certificate from the the TimestampToken
        val certFromTsResp: Try[X509CertificateHolder] = {

          val tsaSignerInfo: Try[SignerInformation] = {
            val cms = tsResponse.getTimeStampToken.toCMSSignedData
            val signers = cms.getSignerInfos().getSigners()
            if (signers.size != 1) {
              Failure(new Exception(s"Timestamp token signed by ${signers.size} signers, but it must contain just the TSA signature."))
            } else if (!signers.iterator().hasNext) {
              Failure(new Exception("no signers!"))
            } else {
              Success(signers.iterator().next().asInstanceOf[SignerInformation])
            }
          }

          tsaSignerInfo.flatMap { signerInfo =>
            //val certStore = timestampToken.getCertificates.asInstanceOf[Store[X509CertificateHolder]] //=> bouncycastle > 1.51
            val certStore = timestampToken.getCertificates
            //val selector = tsaSignerInfo.getSID.asInstanceOf[Selector[X509CertificateHolder]]         //=> bouncycastle > 1.51
            val selector = signerInfo.getSID.asInstanceOf[Selector]
            val certs = certStore.getMatches(selector)
            val foundTsCert = certs.iterator().asInstanceOf[util.Iterator[X509CertificateHolder]].asScala.toList.filter(x => {
              val extendedKeyUsage = x.getExtension(Extension.extendedKeyUsage)
              if (extendedKeyUsage == null) false
              else {
                extendedKeyUsage.getExtnValue == new DEROctetString(new ExtendedKeyUsage(KeyPurposeId.id_kp_timeStamping))
              }
            })

            if (foundTsCert.size != 1) {
              Failure(new Exception(s"Time-stamp token has by ${certs.size} certificates, but it must contain just one TSA certificate."))
            } else {
              Success(foundTsCert.head)
            }
          }
        }

        // Check the TimestampResponse's signature against a list of trusted tsa-ca-certificates
        // since we may use several TSAs (e.g. a primary and a fallback TSA)
        val matchingCaCert: Option[X509Certificate] = caCertificates.zipWithIndex.find { case (caCert, index) =>
          try {
            logger.debug(
              s"""checkTimestampImpl()..trying ca-caCert. ${index + 1} of ${caCertificates.size}
                  |caCert.getIssuerDN.getName: ${caCert.getIssuerDN.getName}
               """.stripMargin)

            val contentVerifierProvider = new JcaContentVerifierProviderBuilder().build(caCert)

            //check if the TSA certificate from the TimeStampResponse's SignerInformation has been signed by a trusted ca-caCert.

            val isTsaCertSignedByTrustedCaCert = try {
              certFromTsResp.toOption.map(_.isSignatureValid(contentVerifierProvider)).getOrElse(false)
            } catch {
              case t: Throwable =>
                logger.debug(s"tsCertFromResp.isSignatureValid()..failed:", t)
                false
            }
            logger.debug(s"isTsaCertSignedByTrustedCaCert:$isTsaCertSignedByTrustedCaCert")

            val isTimestampTokenValidWithCertFromTsResp: Boolean = certFromTsResp.map { certFromTsResp =>
              try {
                val sivBuilder = new JcaSimpleSignerInfoVerifierBuilder()
                val siv = sivBuilder.build(certFromTsResp)
                timestampToken.validate(siv)
                true
              } catch {
                case t: Throwable =>
                  logger.debug(s"timestampToken.validate() failed:", t)
                  false
              }
            }.getOrElse(false)
            logger.debug(s"isTimestampTokenValidWithCertFromTsResp: $isTimestampTokenValidWithCertFromTsResp")
            isTsaCertSignedByTrustedCaCert && isTimestampTokenValidWithCertFromTsResp
          } catch {
            case t: Throwable =>
              logger.debug(s"error using processing $caCert: ${t.getMessage}", t)
              false
          }
        }.map(_._1)


        val timestampAuthCheckResult = matchingCaCert match {
          case Some(trustedCert) =>
            //checks whether the given time is within the certificate's validity period
            val timestamp = tsResponse.getTimeStampToken.getTimeStampInfo.getGenTime
            certFromTsResp match {
              case Success(certificate) =>
                if (!certificate.isValidOn(timestamp)) {
                  CheckFailure(timestampAuthCheck,
                    message = s"The timestamp authority's certificate is expired or not yet valid.")
                } else {
                  if (tsResponse.getStatus == 0) {
                    // 0: Granted
                    // ==> Authentication OK
                    CheckOk(timestampAuthCheck)
                  } else {
                    CheckFailure(timestampAuthCheck, message = s"Timestamp response status was not GRANTED (status: ${tsResponse.getStatus})")
                  }
                }
              case _ =>
                CheckFailure(timestampAuthCheck, message = s"Timestamp response does not contain a appropriate timestamping certificate")
            }
          case _ =>
            CheckFailure(timestampAuthCheck,
              message = s"The timestamp authority's certificate is not signed by one ot the trusted CA-certificates.")
        }

        Seq(
          timestampResultSignatureCheckResult,
          timestampAuthCheckResult,
          timestampBeforeDrawTimeCheckResult
        )
      } catch {
        case t: Throwable =>
          Seq(
            CheckFailure(timestampAuthCheck, message = t.getMessage, exception = Option(t)),
            CheckFailure(timestampResultSignatureCheck, message = t.getMessage, exception = Some(t)),
            CheckFailure(timestampBeforeDrawtimeCheck, message = t.getMessage, exception = Some(t))
          )
      }
    }
  }

  override def validateOrder(orderDirPath: Path, drawtime: Try[Instant]): OrderValidationResult = {
    val order = resourceProvider.getOrder(orderDirPath)
    val archiveDir = orderDirPath.getParent
    val poolMetadata = resourceProvider.getPoolMetadata(poolDirPath = archiveDir)
    val orderSignature = resourceProvider.getOrderSignature(orderDirPath)
    val orderResult = resourceProvider.getOrderResult(orderDirPath)
    val orderResultSignature = resourceProvider.getOrderResultSignature(orderDirPath)
    val orderResultSignatureTimestamp = resourceProvider.getOrderResultSignatureTimestamp(orderDirPath)

    val validationResults = IndexedSeq.newBuilder[CheckResult]
    validationResults.sizeHint(PoolValidator.allChecks.size)

    def doCheck(aCheck: => CheckResult): Unit = {
      validationResults += aCheck
    }

    def doChecks(aCheck: => Seq[CheckResult]): Unit = {
      validationResults ++= aCheck
    }

    //get the credentials
    val retailerPublicKey = orderSignature.flatMap(os => credentialsProvider.getPublicKey(os.keyId, os.algorithm))
    val operatorPublicKey = orderResultSignature.flatMap(ors => credentialsProvider.getPublicKey(ors.keyId, ors.algorithm))
    val caCertificate = credentialsProvider.getCertificate(CredentialsProvider.RootCaCertificate)

    //execute the checks

    doCheck {
      val orderDir_lastSegment = orderDirPath.getName(orderDirPath.getNameCount - 1)
      checkOrderHashMatchesDirectoryName(orderData = order.map(_.rawData), directoryName = orderDir_lastSegment.toString)
    }

    doCheck {
      checkPoolParticipation(order, poolMetadata)
    }

    doCheck {
      checkRetailerSignature(orderSignature, order, retailerPublicKey)
    }

    doCheck {
      checkOrderResult(order, orderResult)
    }

    doCheck {
      checkOrderIsAccepted(orderResult)
    }

    doCheck {
      val errors: Seq[String] = Seq(
        order.failed.toOption.map(t => getErrorTxtMissingOrInvalidOrderDoc(PRType.Order, t)),
        orderSignature.failed.toOption.map(t => getErrorTxtMissingOrInvalidOrderDoc(PRType.OrderSignature, t)),
        orderResult.failed.toOption.map(t => getErrorTxtMissingOrInvalidOrderDoc(PRType.OrderResult, t)),
        orderResultSignature.failed.toOption.map(t => getErrorTxtMissingOrInvalidOrderDoc(PRType.OrderResultSignature, t)),
        orderResultSignatureTimestamp.failed.toOption.map(t => getErrorTxtMissingOrInvalidOrderDoc(PRType.OrderResultSignatureTimestamp, t))
      ).flatten

      errors.isEmpty match {
        case true => CheckOk(AllOrderDocumentsMustExistCheck)
        case false => CheckFailure(AllOrderDocumentsMustExistCheck, message = s"${errors.mkString("\n")}")
      }
    }

    doCheck {
      val signature = orderResultSignature.map(s => Base64_AC.decodeBase64(s.signature.toArray).toIndexedSeq)
      val orderResultData = orderResult.map(_.rawData)
      checkOperatorSignature(signature = signature, operatorPubKey = operatorPublicKey, data = orderResultData)
    }

    doChecks {
      //INFO the draw time is stated in the metadata, but it can be overridden in the UI.
      //     This override functionality is implemented in the drawtimeProvider.
      //val drawtime: Try[Instant] = poolMetadata.flatMap(metaData => drawtimeProvider.getDrawtime(metaData.participationPoolId))
      checkOrderTimestamp(
        orderResultSignatureTimestamp.map(_.rawData),
        drawtime,
        orderResultSignature = orderResultSignature,
        caCertificates = credentialsProvider.getTimestamperCertificates
      )
    }

    OrderValidationResult(orderDirPath.toAbsolutePath, validationResults.result())
  }

  private[domain] def getErrorTxtMissingOrInvalidOrderDoc(resource: PRType.Value, throwable: Throwable): String = {
    val fileName = PoolResource.docTypeToFileName(resource)
    s"Missing or invalid document ${fileName}: ${throwable.getMessage}"
  }

  override def validatePool(poolLocation: Path, drawTime: Try[Instant],
                            intermediateResultCallback: IntermediateResultCallback): Task[ArchiveValidationResult] = {

    val task = new TaskImpl[ArchiveValidationResult](info = "validatePool", new CancellableSupplierAI[ArchiveValidationResult] {

      override def supply(): ArchiveValidationResult = {
        val validatedCount = new AtomicInteger(0) //needed due to parallel execution

        resourceProvider.getOrderDirPaths(poolLocation.toFile.toPath) match {
          case Success(orderDirPaths) =>
            val orderValidationResults: ParSeq[OrderValidationResult] = orderDirPaths.par.map { orderDirPath =>
              if (isCancelled) {
                logger.info(s"Supplier[ArchiveValidationResult].get() ==> canceled!")
                throw new CancellationException("validatePool() was cancelled")
              }
              val ovResult = validateOrder(orderDirPath, drawTime)
              intermediateResultCallback.onOrderValidated(ovResult, validatedCount.incrementAndGet())
              ovResult
            }

            val poolMetadata = resourceProvider.getPoolMetadata(poolDirPath = poolLocation)
            val timestampBase64 = resourceProvider.getPoolDigestTimestamp(poolLocation).map(_.rawData)
            val poolValidationResults: Seq[CheckResult] = validatePoolSeal(orderDirPaths, poolMetadata, timestampBase64, drawTime)

            ArchiveValidationResult(poolLocation = poolLocation,
              poolValidationResults = poolValidationResults.toVector,
              orderValidationResults.seq.toIndexedSeq
            )

          case Failure(t) =>
            throw new Exception(s"Validation of participation pool not possible: the pool does not contain any orders.", t)
        }
      }
    })
    task
  }


  private[domain] def validatePoolSeal(orderDirPaths: IndexedSeq[Path],
                                       poolMetadata: Try[PoolMetadata],
                                       timestampBase64: Try[IndexedSeq[Byte]],
                                       drawTime: Try[Instant]): Seq[CheckResult] = {

    val poolDigest: Try[PoolDigest] = poolMetadata.flatMap { metaData =>
      metaData.poolDigest match {
        case Some(poolDigest) => Success(poolDigest)
        case _ => Failure(new Exception(s"PoolMetadata.poolDigest is undefined"))
      }
    }

    val poolDigestTimestampExistsCheckResult: CheckResult = {
      timestampBase64 match {
        case Success(timestampData) => CheckOk(PoolDigestTimestampExistsCheck)
        case Failure(t) => CheckFailure(PoolDigestTimestampExistsCheck,
          message = s"Pooldigest timestamp is not available: ${t.getMessage}", exception = Some(t))
      }
    }

    val poolDigestCheckResult: CheckResult = {
      poolDigest match {
        case Success(pooldigest) =>
          val allOrderIds = orderDirPaths.map(_.getFileName.toString).sorted // => the fileName is also the orderId
        val messageDigest = MessageDigest.getInstance(pooldigest.algorithm)
          allOrderIds.foreach { d => messageDigest.update(d.getBytes(StandardCharsets.UTF_8)) }
          val computedPoolDigest = messageDigest.digest()
          val metaDataPoolDigest_bytes = Base64_AC.decodeBase64(pooldigest.base64.toArray)
          (util.Arrays.equals(metaDataPoolDigest_bytes, computedPoolDigest)) match {
            case true => CheckOk(PoolDigestCheck)
            case false => CheckFailure(PoolDigestCheck, "the computed digest for all orders does not match the pool digest from metadata.json")
          }
        case Failure(t) =>
          CheckFailure(PoolDigestCheck, s"PoolMetadata.poolDigest is not available: ${t.getMessage}", exception = Some(t))
      }
    }

    val timestampCheckResults: Seq[CheckResult] = checkPoolDigestTimestamp(
      timestampBase64 = timestampBase64,
      drawtime = drawTime,
      signature = poolDigest.map(_.base64), //poolMetadata.flatMap(_.poolDigest.map(_.base64)),
      caCertificates = credentialsProvider.getTimestamperCertificates
    )

    val poolCheckResults: Seq[CheckResult] = Seq(poolDigestTimestampExistsCheckResult, poolDigestCheckResult) ++ timestampCheckResults
    poolCheckResults
  }

}

