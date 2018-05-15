package domain

import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}
import java.security.cert.X509Certificate
import java.security.{MessageDigest, PublicKey, Signature}
import java.time._
import java.util
import java.util.Base64
import java.util.concurrent.atomic.AtomicInteger

import domain.OrderCheck._
import domain.PoolMetadata.PoolDigest
import domain.PoolResource.{Filenames, PRType}
import domain.PoolSealCheck._
import domain.PoolValidator._
import domain.products.GamingProduct._
import domain.products.ParticipationPools.parseParticipationPoolId
import monix.eval.Task
import monix.execution.Scheduler
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
import scala.util.{Failure, Success, Try}


trait PoolValidator {

  /**
    * Validates a single order. The according order-documents are expected to be within the given `orderDirPath`.
    **/
  def validateOrder(orderId: OrderId, drawTime: Try[Instant]): Task[OrderValidationResult]

  def validateOrderDocs(orderDocs: OrderDocs, drawtime: Try[Instant], poolMetadata: Try[PoolMetadata]): OrderValidationResult

  def validatePoolSeal(
    orderIds: IndexedSeq[String], poolMetadata: Try[PoolMetadata], drawTime: Try[Instant], cbProgress: Option[ProgressIndicator] = None
  ): IndexedSeq[CheckResult]
  
  /**
    * Validates a participation pool at the given poolLocation.
    **/
  def validatePool(drawTime: Try[Instant],
    intermediateResultCallback: IntermediateResultCallback): Task[ArchiveValidationResult]
}


object PoolValidator {

  type ProgressIndicator = Double => Unit
  
  trait IntermediateResultCallback {

    def onOrderValidated(result: OrderValidationResult, countValidatedOrders: Int)

  }


  sealed trait ValidationResult {

    def isValid: Boolean

  }


  case class OrderValidationResult(orderLocation: Path, checkResults: IndexedSeq[CheckResult]) extends ValidationResult {

    override def isValid: Boolean = checkResults.forall(_.isOk)

    def orderId: OrderId = orderLocation.getFileName.toString

  }


  case class ArchiveValidationResult(poolLocation: Path,
    poolValidationResults: IndexedSeq[CheckResult],
    orderValidationResults: IndexedSeq[OrderValidationResult]) extends ValidationResult {

    override def isValid: Boolean = poolValidationResults.forall(_.isOk) && orderValidationResults.forall(_.isValid)

  }

  class UnexpectedValueException[T](msg: String, expected: Option[T], actual: Option[T]) extends Exception(msg)

  sealed trait CheckResult {
    def check: PoolArchiveCheck

    def isOk: Boolean
  }


  case class CheckOk(check: PoolArchiveCheck) extends CheckResult {
    override def isOk: Boolean = true
  }


  case class CheckFailure(check: PoolArchiveCheck,
    message: String,
    exception: Option[Throwable] = None) extends CheckResult {
    override val isOk: Boolean = false
  }

}


class PoolValidatorImpl(
  resourceProvider: PoolResourceProvider,
  private val credentialsProvider: CredentialsProvider,
  algorithmMapper: SignatureAlgorithmMapper)(
  implicit monixScheduler: Scheduler
) extends PoolValidator {

  private val logger = LoggerFactory.getLogger(getClass)

  private[domain] def sha256UrlSafe(data: String): String = {
    val md = MessageDigest.getInstance("SHA-256")
    val digest = md.digest(data.getBytes(StandardCharsets.UTF_8))
    new String(Base64.getUrlEncoder.encode(digest))
  }

  /**
    * @param data non-urlsafe encoded base64 data
    **/
  private[domain] def sha256AsBase64(data: IndexedSeq[Byte]): String = {
    val urlSafe = false
    val md = MessageDigest.getInstance("SHA-256")
    val digest = md.digest(data.toArray)
    new String(Base64.getEncoder.encode(digest))
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
        val signature_raw = Base64.getDecoder.decode(orderSignature.get.signature.toArray)

        algorithmMapper.mapAlgorithmName(orderSignature.get.algorithm) match {
          case Some(signatureAlg) =>
            val sig = Signature.getInstance(signatureAlg)
            sig.initVerify(retailerPublicKey.get)
            sig.update(order.get.rawData.toArray)
            if (sig.verify(signature_raw))
              CheckOk(RetailerOrderSignatureCheck)
            else
              CheckFailure(RetailerOrderSignatureCheck, message = "Verification of signature failed")
          case _ =>
            CheckFailure(RetailerOrderSignatureCheck, message = s"unsupported signature algorithm: '${orderSignature.get.algorithm}'")
        }
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
      val orderDataHash = sha256AsBase64(order.get.rawData)
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

  /**
    *
    **/
  private[domain] def checkOperatorSignature(orderResultSignature: Try[OrderResultSignature],
    operatorPubKey: Try[PublicKey],
    orderResult: Try[OrderResult]): CheckResult = {
    val errors = orderResultSignature.failed.toOption.map(t => s"Missing operator signature: ${t.getMessage}") ++
      operatorPubKey.failed.toOption.map(t => s"Missing operator public key: ${t.getMessage}") ++
      orderResult.failed.toOption.map(t => s"Missing order result data: ${t.getMessage}")
    if (errors.nonEmpty) {
      CheckFailure(OrderResultSignatureValidCheck, message = errors.mkString("\n"))
    } else {
      try {
        val signature = Base64.getDecoder.decode(orderResultSignature.get.signature.toArray).toIndexedSeq
        val orderResultData = orderResult.get.rawData
        algorithmMapper.mapAlgorithmName(orderResultSignature.get.algorithm) match {
          case Some(signatureAlg) =>
            val sig = Signature.getInstance(signatureAlg)
            sig.initVerify(operatorPubKey.get)
            sig.update(orderResultData.toArray)
            if (sig.verify(signature.toArray))
              CheckOk(OrderResultSignatureValidCheck)
            else {
              CheckFailure(OrderResultSignatureValidCheck, message = "Verification of signature failed")
            }
          case _ =>
            CheckFailure(OrderResultSignatureValidCheck, message = s"unsupported signature algorithm: '${orderResultSignature.get.algorithm}'")
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
    caCertificates: Seq[X509Certificate]): IndexedSeq[CheckResult] = {
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
    timestampAuthCheck: PoolArchiveCheck,
    timestampResultSignatureCheck: PoolArchiveCheck,
    timestampBeforeDrawtimeCheck: PoolArchiveCheck
  ): IndexedSeq[CheckResult] = {
    if (timestampBase64.isFailure) {
      val errors = {
        timestampBase64.failed.toOption.map(t => s"Missing TimestampResponse data: ${t.getMessage}") ++
          drawTime.failed.toOption.map(t => s"Could not obtain drawtime: ${t.getMessage}") ++
          signature.failed.toOption.map(t => s"Missing signature: ${t.getMessage}")
      } ++ (if (caCertificates.isEmpty) Some("No certificates provided") else None).toList
      val msg = errors.mkString(", ")
      IndexedSeq(
        CheckFailure(timestampResultSignatureCheck, message = msg),
        CheckFailure(timestampAuthCheck, message = msg),
        CheckFailure(timestampBeforeDrawtimeCheck, message = msg)
      )
    } else {
      try {

        //deserialize the tsaResponse (ASN.1 data)
        val tsResponse: TimeStampResponse = try {
          val tsaResponse: Array[Byte] = Base64.getDecoder.decode(timestampBase64.get.toArray)
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
            val orderResultSignature_raw = Base64.getDecoder.decode(sig.toArray)
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

        IndexedSeq(
          timestampResultSignatureCheckResult,
          timestampAuthCheckResult,
          timestampBeforeDrawTimeCheckResult
        )
      } catch {
        case t: Throwable =>
          IndexedSeq(
            CheckFailure(timestampAuthCheck, message = t.getMessage, exception = Option(t)),
            CheckFailure(timestampResultSignatureCheck, message = t.getMessage, exception = Some(t)),
            CheckFailure(timestampBeforeDrawtimeCheck, message = t.getMessage, exception = Some(t))
          )
      }
    }
  }

  override def validateOrder(orderId: OrderId, drawtime: Try[Instant]): Task[OrderValidationResult] = {
    Task (
      resourceProvider.getOrderDocs(orderId).map { orderDocs  => 
        validateOrderDocs(orderDocs, drawtime, resourceProvider.getPoolMetadata())
      } match { 
        case Success(result) => result
        case Failure(t) => throw t
      }
    )
  }

  override def validateOrderDocs(orderDocs: OrderDocs, drawtime: Try[Instant], poolMetadata: Try[PoolMetadata]): OrderValidationResult = {
    //get the credentials
    val retailerPublicKey = orderDocs.orderSignature.flatMap(os => credentialsProvider.getPublicKey(os.keyId, os.algorithm))
    val operatorPublicKey = orderDocs.orderResultSignature.flatMap(ors => credentialsProvider.getPublicKey(ors.keyId, ors.algorithm))
    val caCertificate = credentialsProvider.getCertificate(CredentialsProvider.RootCaCertificate)

    //execute the checks
    val validationResults = IndexedSeq(
      checkPoolParticipation(orderDocs.order, poolMetadata),
      checkRetailerSignature(orderDocs.orderSignature, orderDocs.order, retailerPublicKey),
      checkOrderResult(orderDocs.order, orderDocs.orderResult),
      checkOrderIsAccepted(orderDocs.orderResult),
      checkIfAllOrderDocumentsExist(orderDocs),
      checkOperatorSignature(orderDocs.orderResultSignature, operatorPubKey = operatorPublicKey, orderDocs.orderResult)
    ) ++ checkOrderTimestamp(
      orderDocs.orderResultSignatureTimestamp.map(_.rawData),
      drawtime,
      orderResultSignature = orderDocs.orderResultSignature,
      caCertificates = credentialsProvider.getTimestamperCertificates
    )
    OrderValidationResult(Paths.get(orderDocs.orderId), validationResults)
  }

  private[domain] def getErrorTxtMissingOrInvalidOrderDoc(resource: PRType.Value, throwable: Throwable): String = {
    val fileName = PoolResource.docTypeToFileName(resource)
    s"Missing or invalid document ${fileName}: ${throwable.getMessage}"
  }

  private def checkIfAllOrderDocumentsExist(orderDocs: OrderDocs): CheckResult = {
    val errors: Seq[String] = Seq(
      orderDocs.order.failed.toOption.map(t => getErrorTxtMissingOrInvalidOrderDoc(PRType.Order, t)),
      orderDocs.orderSignature.failed.toOption.map(t => getErrorTxtMissingOrInvalidOrderDoc(PRType.OrderSignature, t)),
      orderDocs.orderResult.failed.toOption.map(t => getErrorTxtMissingOrInvalidOrderDoc(PRType.OrderResult, t)),
      orderDocs.orderResultSignature.failed.toOption.map(t => getErrorTxtMissingOrInvalidOrderDoc(PRType.OrderResultSignature, t)),
      orderDocs.orderResultSignatureTimestamp.failed.toOption.map(t => getErrorTxtMissingOrInvalidOrderDoc(PRType.OrderResultSignatureTimestamp, t))
    ).flatten

    val r: CheckResult = errors.isEmpty match {
      case true => CheckOk(AllOrderDocumentsMustExistCheck)
      case false => CheckFailure(AllOrderDocumentsMustExistCheck, message = s"${errors.mkString("\n")}")
    }
    r
  }

  override def validatePool(drawTime: Try[Instant], intermediateResultCallback: IntermediateResultCallback): Task[ArchiveValidationResult] = {
    val validatedCount = new AtomicInteger(0) //needed due to parallel execution
    val poolMetadata = resourceProvider.getPoolMetadata()
    val ordersValidationTask = resourceProvider.getOrderDocsObservable()
      .mapParallelUnordered(parallelism = 8) { orderDocsTry =>
        Task {
          orderDocsTry.failed.foreach( t => throw new Exception(s"failed to load OrderDocs:${t.getMessage}", t)) //catched by the stream
          val orderDocs = orderDocsTry.get
          val ovResult = validateOrderDocs(orderDocs, drawTime, poolMetadata)
          intermediateResultCallback.onOrderValidated(ovResult, validatedCount.incrementAndGet())
          ovResult
        }
      }.toListL.map(_.toIndexedSeq)

    val resultFuture: monix.eval.Task[ArchiveValidationResult] = ordersValidationTask.map { orderValidationResults =>
      val allOrderIds = orderValidationResults.map(_.orderId)
      if (allOrderIds.isEmpty) {
        throw new Exception(s"Validation of participation pool not possible: the pool does not contain any orders.")
      } else {
        val poolValidationResults: Seq[CheckResult] = validatePoolSeal(allOrderIds, poolMetadata, drawTime)
        ArchiveValidationResult(poolLocation = Paths.get(""),
          poolValidationResults = poolValidationResults.toVector,
          orderValidationResults.seq.toIndexedSeq
        )
      }
    }
    resultFuture.onErrorRecoverWith { case t => 
      Task.raiseError[ArchiveValidationResult](new Exception(s"Validation of participation pool not possible; ${t.getMessage}", t))
    }
  }

  override def validatePoolSeal(
    orderIds: IndexedSeq[String], poolMetadata: Try[PoolMetadata], drawTime: Try[Instant], cbProgress: Option[ProgressIndicator] = None
  ): IndexedSeq[CheckResult] = {

    val timestampBase64 = resourceProvider.getPoolDigestTimestamp().map(_.rawData)

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
          val allOrderIds = orderIds.sorted
          val messageDigest = MessageDigest.getInstance(pooldigest.algorithm)
          allOrderIds.zipWithIndex.foreach { case (d, index) => 
            messageDigest.update(d.getBytes(StandardCharsets.UTF_8))
            cbProgress.foreach { progressUpdateCb =>
              val progressPercent: Double = Math.round((index / allOrderIds.size.toDouble) * 100.0) / 100.0
              progressUpdateCb(progressPercent) 
            }
          }
          val computedPoolDigest = messageDigest.digest()
          val metaDataPoolDigest_bytes = Base64.getDecoder.decode(pooldigest.base64.toArray)
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
      signature = poolDigest.map(_.base64),
      caCertificates = credentialsProvider.getTimestamperCertificates
    )

    IndexedSeq(poolDigestTimestampExistsCheckResult, poolDigestCheckResult) ++ timestampCheckResults
  }
}
