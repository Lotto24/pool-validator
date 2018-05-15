package domain

import java.io._
import java.nio.file.Paths
import java.security.PublicKey
import java.security.cert.{CertificateFactory, X509Certificate}
import java.time._
import java.time.temporal.ChronoUnit
import java.util.Base64

import domain.CredentialsManagerImpl.PublicKeyMapKey
import domain.OrderCheck.AllOrderDocumentsMustExistCheck
import domain.PoolSealCheck._
import domain.PoolMetadata.PoolDigest
import domain.PoolResource.{Filenames, PRType}
import domain.PoolValidator._
import domain.products.ParticipationPools.{formatParticipationPoolId, parseParticipationPoolId}
import domain.products.ejs.EjsGamingProductOrder
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.apache.commons.io.IOUtils
import org.bouncycastle.tsp.TimeStampResponse
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FeatureSpec, GivenWhenThen, Matchers}
import util.Utils
import util.Utils.{DirectoryFilter, fileInputStreamOf}

import scala.util.{Failure, Success, Try}


class PoolValidatorSpec extends FeatureSpec with Matchers with BeforeAndAfterAll with GivenWhenThen with ScalaFutures {

  val futureWaitTimeoutMs = 5000
  val workingDir = new File(System.getProperty("user.dir"))
  val defaultPoolLocation = new File(workingDir, "src/test/resources/testdata_qa/ejs-2016-02-26_dev_closed").toPath

  val defaultIntermediateResultHandler = new IntermediateResultCallback {
    override def onOrderValidated(result: OrderValidationResult, countValidatedOrders: Int): Unit = {}
  }

  val defaultResourceProvider = new PoolResourceProviderDirectoryImpl(poolDirPath = defaultPoolLocation,
    new OrderDocumentsParserPlayImpl(CompositeProductOrderFactory.allProductsOrderFactory)
  ) {
    def getAllOrderIds_synchronous(): Vector[String] = {
      whenReady(getOrderDocsObservable().map(_.get.orderId).toListL.runAsync.map(_.toVector))(identity)
    }
  }

  val defaultPoolDrawTime: Try[Instant] = defaultResourceProvider.getPoolMetadata().map(_.drawDate.toInstant)

  val credentialsDir = new File(workingDir, "src/test/resources/testdata_qa/credentials/")

  val caCertificate = Try {
    val cf = CertificateFactory.getInstance("X.509")
    val pemFile = new File(credentialsDir, "root-ca-cert.pem")
    cf.generateCertificate(pemFile).asInstanceOf[X509Certificate]
  }

  val defaultTimestamperCerts: Seq[X509Certificate] = Seq(caCertificate.get)

  val retailerPublicKey: Try[PublicKey] = {
    val retailerPublicKeyFile = new File(credentialsDir, "retailer_publicKey.pem")
    Utils.readPublicKeyFromPemFile(retailerPublicKeyFile)
  }

  val wrongRetailerPublicKey: Try[PublicKey] = {
    val retailerPublicKeyFile = new File(credentialsDir, "wrong/retailer-public_key.pem")
    Utils.readPublicKeyFromPemFile(retailerPublicKeyFile)
  }

  val operatorPublicKey: Try[PublicKey] = {
    val operatorPublicKeyFile = new File(credentialsDir, "operator_publicKey.pem")
    Utils.readPublicKeyFromPemFile(operatorPublicKeyFile)
  }

  val wrongRetailerCert = Try {
    val certFile = new File(credentialsDir, "wrong/retailer-cert.pem")
    val cf = CertificateFactory.getInstance("X.509")
    cf.generateCertificate(certFile).asInstanceOf[X509Certificate]
  }

  val wrongTimestamperCert = Try {
    val certFile = new File(credentialsDir, "mylotto-tsa-cert.pem")
    val cf = CertificateFactory.getInstance("X.509")
    cf.generateCertificate(certFile).asInstanceOf[X509Certificate]
  }

  val credentialsProvider = new CredentialsManagerImpl(
    keys = Map(
      PublicKeyMapKey(keyId = "ZOE_signkey_01", algorithm = "rsa-sha256") -> operatorPublicKey,
      PublicKeyMapKey(keyId = "unknown", algorithm = "rsa-sha256") -> operatorPublicKey,
      PublicKeyMapKey(keyId = "retailer", algorithm = "rsa-sha256") -> retailerPublicKey
    ),
    certificates = Map(CredentialsProvider.RootCaCertificate -> caCertificate),
    timestamperCertificates = Seq(caCertificate.get)
  )

  override def beforeAll() {
    withClue(s"workingDir.exists(): '$workingDir'")(workingDir.exists() shouldBe true)
  }

  def getOrderFile(resource: PRType.Value, orderIndex: Int = 0): File = {
    val extactedArchiveDir = defaultPoolLocation.toFile
    val orderDir = extactedArchiveDir.listFiles(DirectoryFilter).sortBy(_.getName).apply(orderIndex)

    val f = resource match {
      case PRType.MetaData => new File(orderDir.getParentFile, PoolResource.docTypeToFileName(resource))
      case x => new File(orderDir, PoolResource.docTypeToFileName(resource))
    }

    Utils.isFileUnreadable(f).foreach(e =>
      fail(s"Error with File $f: $e")
    )
    f
  }

  feature("Retailer signature check") {
    val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)


    val orderFile = getOrderFile(PRType.Order)
    val orderSignatureFile = getOrderFile(PRType.OrderSignature)
    val validOrder = defaultResourceProvider.getOrder(orderFile.toPath.getParent.getFileName.toString)

    scenario("Check valid order") {
      val orderData = Utils.getAsSeq(orderFile)
      val orderSignature = defaultResourceProvider.getOrderSignature(orderSignatureFile.toPath.getParent.getFileName.toString)
      withClue("validator.checkRetailerSignature") {
        v.checkRetailerSignature(orderSignature, validOrder, retailerPublicKey).isOk shouldEqual true
      }
    }

    scenario("Check corrupted order") {
      val corruptedOrder = validOrder.map { o =>
        o.copy(rawData = corruptByte(origData = o.rawData, position = 8, corruptionFnct = _ => 9))
      }
      val orderSignature = defaultResourceProvider.getOrderSignature(orderSignatureFile.toPath.getParent.getFileName.toString)
      withClue("validator.checkRetailerSignature") {
        v.checkRetailerSignature(orderSignature, validOrder, wrongRetailerPublicKey).isOk shouldEqual false
      }
    }
  }

  feature("checkPoolParticipation") {

    val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)

    val orderFile = getOrderFile(PRType.Order)
    val metainfoFile = getOrderFile(PRType.MetaData)

    val validOrder = defaultResourceProvider.getOrder(orderFile.toPath.getParent.getFileName.toString)
    val validMetainfo = defaultResourceProvider.getPoolMetadata()

    scenario("Check a valid order") {
      withClue("validator.checkOrderResult")(
        v.checkPoolParticipation(validOrder, validMetainfo).isOk shouldEqual true
      )
    }

    scenario("Check order with falsified participation-pool-id") {

      val falsifiedMetadata = {
        val (gamingProductId, drawDate) = parseParticipationPoolId(validMetainfo.get.participationPoolId)
        val partPoolId_mod = formatParticipationPoolId(gamingProductId, drawDate.plus(1, ChronoUnit.DAYS))
        validMetainfo.get.copy(participationPoolId = partPoolId_mod)
      }
      falsifiedMetadata should not be validMetainfo
      withClue("validator.checkOrderResult")(
        v.checkPoolParticipation(validOrder, Try(falsifiedMetadata)).isOk shouldEqual false
      )
    }

    scenario("Check order with falsified productOrder.metadata.first-date") {
      val falsifiedOrder: Order = {
        validOrder.get.gamingProductOrders.size shouldBe 1
        val ejsOrder = validOrder.get.gamingProductOrders.values.head.asInstanceOf[EjsGamingProductOrder]
        val ejsOrder_mod = {
          val falsifiedPools = ejsOrder.participationPools.copy(firstDate = ejsOrder.participationPools.firstDate.plus(1, ChronoUnit.DAYS))
          ejsOrder.copy(participationPools = falsifiedPools)
        }
        val gamingProductOrders_mod = Map(validOrder.get.gamingProductOrders.keys.head -> ejsOrder_mod)

        gamingProductOrders_mod should not be validOrder.get.gamingProductOrders

        validOrder.get.copy(gamingProductOrders = gamingProductOrders_mod)
      }

      withClue("validator.checkOrderResult")(v.checkPoolParticipation(Try(falsifiedOrder), validMetainfo).isOk shouldEqual false)
    }
  }


  feature("checkOrderIsAccepted") {
    val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)

    val orderResultFile = getOrderFile(PRType.OrderResult)

    scenario("Check valid order") {
      val orderResult = defaultResourceProvider.getOrderResult(orderResultFile.toPath.getParent.getFileName.toString)
      withClue("validator.checkOrderResult")(v.checkOrderIsAccepted(orderResult).isOk shouldEqual true)
    }

    scenario("Check non-accepted order") {
      val resourceProvider = new PoolResourceProviderDirectoryImpl(defaultPoolLocation,
        new OrderDocumentsParserPlayImpl(CompositeProductOrderFactory.allProductsOrderFactory)
      ) {
        override def getOrderResult(orderId: OrderId): Try[OrderResult] = {
          readFromFile[OrderResult](poolDirPath.resolve(orderId).resolve(Filenames.OrderResult)) { (data, filePath) =>
            orderDocsParser.parseOrderResult(data, filePath).map(x => x.copy(orderProcessingResult = "rejected"))
          }
        }
      }
      IOUtils.toString(orderResultFile).contains(OrderResult.Accepted) shouldEqual true
      val orderResult = resourceProvider.getOrderResult(orderId = "dummy-order-id") //--> PoolResourceProviderImpl delivers a static value here
      withClue("validator.checkOrderResult")(v.checkOrderIsAccepted(orderResult).isOk shouldEqual false)
    }
  }

  /*
   * Extract the hash in the order.result document and compare it to the actual order
   * private[domain] def checkOrderResult(result: OrderResult): Boolean = {
  * */
  feature("Compare order.result->orderDigest to the actual order's directory name") {
    val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)

    val validOrderResult = {
      val orderResultFile = getOrderFile(PRType.OrderResult)
      defaultResourceProvider.getOrderResult(orderResultFile.toPath.getParent.getFileName.toString)
    }

    val validOrder = {
      val orderFile = getOrderFile(PRType.Order)
      defaultResourceProvider.getOrder(orderFile.toPath.getParent.getFileName.toString)
    }

    scenario("Check valid order") {
      val r = v.checkOrderResult(validOrder, validOrderResult)
      withClue(s"validator.checkOrderResult(): \n$r")(
        r.isOk shouldEqual true
      )
    }

    scenario("Check corrupted order data") {
      val corruptedOrder = validOrder.map { o =>
        o.copy(rawData = corruptByte(o.rawData, position = 4, corruptionFnct = _ => 0))
      }
      validOrder should not be corruptedOrder
      withClue("validator.checkOrderResult")(v.checkOrderResult(corruptedOrder, validOrderResult).isOk shouldEqual false)
    }

    scenario("Check corrupted order.result-orderDigest") {
      val corruptedOrderResult = validOrderResult.get.copy(orderDigest = corruptByte(validOrderResult.get.orderDigest, position = 3,
        corruptionFnct = _ => 'Z'))
      corruptedOrderResult should not be validOrderResult
      withClue("validator.checkOrderResult")(
        v.checkOrderResult(validOrder, Try(corruptedOrderResult)).isOk shouldEqual false
      )
    }
  }

  feature("checkOperatorSignature") {
    val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)
    val orderResultSignatureFile = getOrderFile(PRType.OrderResultSignature)
    val orderResultSignature = defaultResourceProvider.getOrderResultSignature(orderResultSignatureFile.toPath.getParent.getFileName.toString).get

    val orderResultFile = getOrderFile(PRType.OrderResult)
    val orderResult = defaultResourceProvider.getOrderResult(orderResultFile.toPath.getParent.getFileName.toString)

    scenario("Check valid order") {
      withClue("validator.checkOperatorSignature") {
        v.checkOperatorSignature(Try(orderResultSignature), //signature = Try(signature),
          operatorPubKey = operatorPublicKey, orderResult = orderResult).isOk shouldEqual true
      }
    }

    scenario("Check order with invalid orderResultSignature.signature") {
      val orderResultSignature_corrupted: OrderResultSignature = {
        val signature_corrupted = corruptByte(origData = orderResultSignature.signature, position = 5, corruptionFnct = _ => 'Z')
        orderResultSignature.copy(signature = signature_corrupted)
      }

      orderResultSignature_corrupted should not be orderResultSignature

      withClue("validator.checkOperatorSignature") {
        v.checkOperatorSignature(Try(orderResultSignature_corrupted),
          operatorPubKey = operatorPublicKey, orderResult = orderResult).isOk shouldEqual false
      }
    }
  }

  feature("validator.checkTimestamp_fromCaCert") {
    val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)

    val orderResultSignatureFile = getOrderFile(PRType.OrderResultSignature)
    val validOrderResultSignature = defaultResourceProvider.getOrderResultSignature(orderResultSignatureFile.toPath.getParent.getFileName.toString)


    val orderResultSignatureTimestampFile = getOrderFile(PRType.OrderResultSignatureTimestamp)

    val validTimestamp = defaultResourceProvider.getOrderResultSignatureTimestamp(orderResultSignatureTimestampFile.toPath.getParent.getFileName.toString)

    val poolMetadata = {
      val metadataFile = getOrderFile(PRType.MetaData)
      defaultResourceProvider.getPoolMetadata()
    }

    scenario("Check valid order") {
      val r = v.checkOrderTimestamp(validTimestamp.map(_.rawData), defaultPoolDrawTime, validOrderResultSignature, defaultTimestamperCerts)
      withClue(s"validator.checkTimestamp ${r.mkString("\n")}") {
        r.forall(_.isOk) shouldEqual true
      }
    }
  }

  feature("validator.checkTimestamp") {

    val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)

    val orderResultSignatureFile = getOrderFile(PRType.OrderResultSignature)
    val validOrderResultSignature = defaultResourceProvider.getOrderResultSignature(orderResultSignatureFile.toPath.getParent.getFileName.toString)

    val orderResultSignatureTimestampFile = getOrderFile(PRType.OrderResultSignatureTimestamp)

    val poolMetadata = {
      val metadataFile = getOrderFile(PRType.MetaData)
      defaultResourceProvider.getPoolMetadata()
    }

    val validTimestamp = defaultResourceProvider.getOrderResultSignatureTimestamp(orderResultSignatureTimestampFile.toPath.getParent.getFileName.toString)

    scenario("Check valid order") {
      val r = v.checkOrderTimestamp(validTimestamp.map(_.rawData), defaultPoolDrawTime, validOrderResultSignature, defaultTimestamperCerts)
      withClue(s"validator.checkTimestamp $r") {
        r.forall(_.isOk) shouldEqual true
      }
    }

    scenario("Check corrupted orderResultSignature") {

      val orderResultSignature_corrupted = {
        val sig_mod = corruptByte(origData = validOrderResultSignature.get.signature, position = 5, corruptionFnct = _ => '5')
        validOrderResultSignature.get.copy(signature = sig_mod)
      }

      validOrderResultSignature.get should not equal orderResultSignature_corrupted

      //TimeStampResponse from a byte array containing an ASN.1 encoding
      val timestamp_raw = Base64.getDecoder.decode(Utils.getAsSeq(orderResultSignatureTimestampFile).get.toArray)
      val r = v.checkOrderTimestamp(validTimestamp.map(_.rawData), defaultPoolDrawTime, Try(orderResultSignature_corrupted), defaultTimestamperCerts)
      withClue(s"validator.checkTimestamp()..results: ${r.mkString("\n")}") {
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampResultSignatureCheck).get.isOk shouldBe false
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampAuthCheck).get.isOk shouldBe true
      }
    }

    scenario("checkTimestamp with wrong tsa-cert (timestamper-cert)") {
      val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)
      val r = v.checkOrderTimestamp(validTimestamp.map(_.rawData), defaultPoolDrawTime, validOrderResultSignature,
        Seq(wrongTimestamperCert.get))
      withClue(s"validator.checkTimestamp $r") {
        r.exists(!_.isOk) shouldEqual true
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampAuthCheck).get.isOk shouldBe false
        r.count(!_.isOk) shouldBe 1
      }
    }

    scenario("checkTimestamp with wrong tsa-cert (retailer-cert)") {
      val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)
      val r = v.checkOrderTimestamp(validTimestamp.map(_.rawData), defaultPoolDrawTime, validOrderResultSignature,
        Seq(wrongRetailerCert.get))
      withClue(s"validator.checkTimestamp ${r.mkString("\n")}") {
        r.count(!_.isOk) shouldBe 1
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampAuthCheck) match {
          case Some(result: CheckFailure) =>
            result.isOk shouldBe false
            result.message shouldEqual "The timestamp authority's certificate is not signed by one ot the trusted CA-certificates."
          case _ => fail("OrderResultSignatureTimestampAuthCheck should be failed")
        }
      }
    }

    scenario("check order with timestamp-time after draw time") {

      val timestampTime = {
        val resp = new TimeStampResponse(Base64.getDecoder.decode(validTimestamp.get.rawData.toArray))
        resp.getTimeStampToken.getTimeStampInfo.getGenTime.toInstant
      }

      val drawTime_beforeTs = Try(timestampTime.minus(1, ChronoUnit.SECONDS))

      val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)

      val r = v.checkOrderTimestamp(validTimestamp.map(_.rawData), drawTime_beforeTs, validOrderResultSignature,
        defaultTimestamperCerts)
      withClue(s"validator.checkTimestamp $r") {
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampResultSignatureCheck).get.isOk shouldBe true
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampAuthCheck).get.isOk shouldBe true
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampBeforeDrawTimeCheck).get.isOk shouldBe false
      }
    }

    scenario("apply check with no timestampResponseData available") {
      val r = v.checkOrderTimestamp(timestampBase64 = Failure(new Exception("no timestampResponseData available")),
        defaultPoolDrawTime, validOrderResultSignature, defaultTimestamperCerts)

      withClue(s"validator.checkTimestamp ${r.mkString("\n")}") {
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampResultSignatureCheck).get.isOk shouldBe false
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampAuthCheck).get.isOk shouldBe false
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampBeforeDrawTimeCheck).get.isOk shouldBe false
      }
    }

    scenario("apply check with corrupted timestampResponseData ") {
      val corruptedTsRespData = corruptByte(origData = validTimestamp.map(_.rawData).get, position = 4, corruptionFnct = _ => 2)
      corruptedTsRespData should not be validTimestamp.map(_.rawData).get
      val r = v.checkOrderTimestamp(timestampBase64 = Try(corruptedTsRespData),
        defaultPoolDrawTime, validOrderResultSignature, defaultTimestamperCerts)

      withClue(s"validator.checkTimestamp ${r.mkString("\n")}") {
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampResultSignatureCheck).get.isOk shouldBe false
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampAuthCheck).get.isOk shouldBe false
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampBeforeDrawTimeCheck).get.isOk shouldBe false
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampResultSignatureCheck) match {
          case Some(f: CheckFailure) => f.message.startsWith("Error parsing TimestampResponse data") shouldBe true
        }
      }
    }

    scenario("apply check with no draw time available") {
      val r = v.checkOrderTimestamp(timestampBase64 = validTimestamp.map(_.rawData),
        drawTime = Failure(new Exception("no drawtime available!")),
        orderResultSignature = validOrderResultSignature,
        defaultTimestamperCerts
      )

      withClue(s"validator.checkTimestamp ${r.mkString("\n")}") {
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampResultSignatureCheck).get.isOk shouldBe true
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampAuthCheck).get.isOk shouldBe true
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampBeforeDrawTimeCheck).get.isOk shouldBe false
      }
    }

    scenario("apply check with no no signatureData available") {
      val r = v.checkOrderTimestamp(timestampBase64 = validTimestamp.map(_.rawData),
        defaultPoolDrawTime,
        orderResultSignature = Failure(new Exception("no OrderResultSignature available!")),
        defaultTimestamperCerts
      )

      withClue(s"validator.checkTimestamp ${r.mkString("\n")}") {
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampResultSignatureCheck).get.isOk shouldBe false
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampAuthCheck).get.isOk shouldBe true
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampBeforeDrawTimeCheck).get.isOk shouldBe true
      }
    }

    scenario("apply check with empty timestamper ca-certificate list") {
      val r = v.checkOrderTimestamp(timestampBase64 = validTimestamp.map(_.rawData),
        defaultPoolDrawTime,
        orderResultSignature = validOrderResultSignature,
        caCertificates = Seq.empty
      )

      withClue(s"validator.checkTimestamp ${r.mkString("\n")}") {
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampResultSignatureCheck).get.isOk shouldBe true
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampAuthCheck).get.isOk shouldBe false
        r.find(_.check == OrderCheck.OrderResultSignatureTimestampBeforeDrawTimeCheck).get.isOk shouldBe true
      }
    }
  }

  feature("validator.validateOrder(order)") {
    val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)
    val orderFile = getOrderFile(PRType.Order)
    scenario("Check valid order") {
      whenReady(v.validateOrder(orderFile.toPath.getParent.getFileName.toString, defaultPoolDrawTime).runAsync) { r =>
        withClue(s"v.validateOrder result:\n${r.checkResults.mkString("\n")}") {
          r.checkResults.forall(_.isOk) shouldBe true
          r.checkResults.map(_.check).toSet.diff(Check.allChecks.toSet) shouldEqual Set.empty
          OrderCheck.values.toSet.diff(r.checkResults.map(_.check).toSet) shouldEqual Set.empty
          r.checkResults.map(_.check).toSet shouldEqual OrderCheck.values.toSet
          r.checkResults.size shouldBe OrderCheck.values.size
        }
      }
    }

    //INFO ignored since the root-ca-cert is currently not used for validation
    ignore("Check order - wrong ca-cert (retailerCert expected as ca-cert)") {
      val credProvider_wrongCert = credentialsProvider.withCertificate(CredentialsProvider.RootCaCertificate, wrongRetailerCert)
      val v = new PoolValidatorImpl(defaultResourceProvider, credProvider_wrongCert, DefaultSignatureAlgMapper)
      whenReady(v.validateOrder(orderFile.toPath.getParent.getFileName.toString, defaultPoolDrawTime).runAsync) { r =>
        withClue(s"v.validateOrder result:\n${r.checkResults.mkString("\n")}") {
          r.checkResults.count(!_.isOk) shouldBe 0
          r.checkResults.collect { case x@CheckFailure(OrderCheck.OrderResultSignatureTimestampResultSignatureCheck, _, _) => x }.size shouldEqual 1
          r.checkResults.collect { case x@CheckFailure(OrderCheck.OrderResultSignatureTimestampAuthCheck, _, _) => x }.size shouldEqual 1
          r.checkResults.collect { case x@CheckFailure(OrderCheck.OrderResultSignatureTimestampBeforeDrawTimeCheck, _, _) => x }.size shouldEqual 0
          r.checkResults.size shouldBe OrderCheck.values.size
        }
      }
    }

    //INFO ignored since the root-ca-cert is currently not used for validation
    ignore("Check order - wrong ca-cert (timestamperCert expected as ca-cert)") {
      val credProvider_wrongCert = credentialsProvider.withCertificate(CredentialsProvider.RootCaCertificate, wrongTimestamperCert)
      val v = new PoolValidatorImpl(defaultResourceProvider, credProvider_wrongCert, DefaultSignatureAlgMapper)
      whenReady(v.validateOrder(orderFile.toPath.getParent.getFileName.toString, defaultPoolDrawTime).runAsync) { r =>
        withClue(s"v.validateOrder result:\n${r.checkResults.mkString("\n")}") {
          r.checkResults.count(!_.isOk) shouldBe 2
          r.checkResults.collect { case x@CheckFailure(OrderCheck.OrderResultSignatureTimestampResultSignatureCheck, _, _) => x }.size shouldEqual 1
          r.checkResults.collect { case x@CheckFailure(OrderCheck.OrderResultSignatureTimestampAuthCheck, _, _) => x }.size shouldEqual 1
          r.checkResults.collect { case x@CheckFailure(OrderCheck.OrderResultSignatureTimestampBeforeDrawTimeCheck, _, _) => x }.size shouldEqual 0
          r.checkResults.size shouldBe OrderCheck.values.size
        }
      }
    }
  }


  feature("validator.validatePoolSeal()") {

    scenario("apply validation to valid pool") {
      val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)
      val vResults = v.validatePoolSeal(
        defaultResourceProvider.getAllOrderIds_synchronous(),
        defaultResourceProvider.getPoolMetadata(),
        defaultPoolDrawTime
      )
      withClue(s"validatePoolSeal() results:\n${vResults.mkString("\n")}") {
        vResults.count(!_.isOk) shouldBe 0
      }
    }

    scenario("apply validation to pool with corrupted orderDirPaths (one orderDir omitted)") {
      val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)
      val allOrderIdsCorrupted = defaultResourceProvider.getAllOrderIds_synchronous().tail

      val vResults = v.validatePoolSeal(
        orderIds = allOrderIdsCorrupted,
        poolMetadata = defaultResourceProvider.getPoolMetadata(),
        defaultPoolDrawTime
      )
      withClue(s"validatePoolSeal() results:\n${vResults.mkString("\n")}") {
        vResults.count(!_.isOk) should be > 0
        vResults.find(_.check == PoolDigestCheck).get.isOk shouldBe false
        vResults.find(_.check == PoolDigestTimestampExistsCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampAuthCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampResultSignatureCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampBeforeDrawTimeCheck).get.isOk shouldBe true
      }
    }

    scenario("apply validation to pool with one corrupted orderDir name") {

      val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)
      val allOrderIdsDirs = defaultResourceProvider.getAllOrderIds_synchronous()

      val allOrderIdsCorrupted: IndexedSeq[String] = {
        val orderDir0_mod: String = {
          val orderDir0_name = allOrderIdsDirs.head
          val orderDir0_nameMod = orderDir0_name.updated(5, '0')
          orderDir0_nameMod should not be orderDir0_name
          orderDir0_nameMod
        }
        orderDir0_mod +: allOrderIdsDirs.tail
      }

      val vResults = v.validatePoolSeal(
        orderIds = allOrderIdsCorrupted,
        poolMetadata = defaultResourceProvider.getPoolMetadata(),
        defaultPoolDrawTime
      )
      withClue(s"validatePoolSeal() results:\n${vResults.mkString("\n")}") {
        vResults.count(!_.isOk) should be > 0
        vResults.find(_.check == PoolDigestCheck).get.isOk shouldBe false
        vResults.find(_.check == PoolDigestTimestampExistsCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampAuthCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampResultSignatureCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampBeforeDrawTimeCheck).get.isOk shouldBe true
      }
    }

    scenario("apply validation to pool with unavailable poolMetadata") {
      val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)
      val vResults = v.validatePoolSeal(
        defaultResourceProvider.getAllOrderIds_synchronous(),
        poolMetadata = Failure(new Exception("poolMetadata not available!")),
        defaultPoolDrawTime
      )
      withClue(s"validatePoolSeal() results:\n${vResults.mkString("\n")}") {
        vResults.count(!_.isOk) should be > 0
        vResults.find(_.check == PoolDigestCheck).get.isOk shouldBe false
        vResults.find(_.check == PoolDigestTimestampExistsCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampAuthCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampResultSignatureCheck).get.isOk shouldBe false
        vResults.find(_.check == PoolDigestTimestampBeforeDrawTimeCheck).get.isOk shouldBe true
      }
    }

    scenario("apply validation to pool with unavailable drawTime") {
      val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)
      val vResults = v.validatePoolSeal(
        defaultResourceProvider.getAllOrderIds_synchronous(),
        poolMetadata = defaultResourceProvider.getPoolMetadata(),
        drawTime = Failure(new Exception("drawTime is not available!"))
      )
      withClue(s"validatePoolSeal() results:\n${vResults.mkString("\n")}") {
        vResults.count(!_.isOk) should be > 0
        vResults.find(_.check == PoolDigestCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampExistsCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampAuthCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampResultSignatureCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampBeforeDrawTimeCheck).get.isOk shouldBe false
      }
    }

    scenario("apply validation to pool with corrupted poolMetadata") {
      val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)
      val corruptedPoolMetadata: Try[PoolMetadata] = {
        val poolMetadata_ok = defaultResourceProvider.getPoolMetadata().get
        val poolDigest_corrupted: Option[PoolDigest] = {
          val base64_ok = poolMetadata_ok.poolDigest.get.base64
          val base64_corrupted = corruptByte(origData = base64_ok, position = 8, corruptionFnct = _ => 'C')
          poolMetadata_ok.poolDigest.map(_.copy(base64 = base64_corrupted))
        }
        poolDigest_corrupted should not be poolMetadata_ok.poolDigest
        Try(poolMetadata_ok.copy(poolDigest = poolDigest_corrupted))
      }

      val vResults = v.validatePoolSeal(
        defaultResourceProvider.getAllOrderIds_synchronous(),
        poolMetadata = corruptedPoolMetadata,
        defaultPoolDrawTime
      )
      withClue(s"validatePoolSeal() results:\n${vResults.mkString("\n")}") {
        vResults.count(!_.isOk) should be > 0
        vResults.find(_.check == PoolDigestCheck).get.isOk shouldBe false
        vResults.find(_.check == PoolDigestTimestampExistsCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampAuthCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampResultSignatureCheck).get.isOk shouldBe false
        vResults.find(_.check == PoolDigestTimestampBeforeDrawTimeCheck).get.isOk shouldBe true
      }

    }

    scenario("apply validation to pool with unavailable timestamp-data") {

      val poolResourceProvider = new PoolResourceProviderDirectoryImpl(defaultPoolLocation,
        new OrderDocumentsParserPlayImpl(CompositeProductOrderFactory.allProductsOrderFactory)
      ) {
        override def getPoolDigestTimestamp(): Try[PoolDigestTimestamp] = Failure(new Exception("pool timestamp not found"))
      }
      val v = new PoolValidatorImpl(poolResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)

      val vResults = v.validatePoolSeal(
        defaultResourceProvider.getAllOrderIds_synchronous(),
        poolMetadata = defaultResourceProvider.getPoolMetadata(),
        defaultPoolDrawTime
      )
      withClue(s"validatePoolSeal() results:\n${vResults.mkString("\n")}") {
        vResults.count(!_.isOk) should be > 0
        vResults.find(_.check == PoolDigestCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampExistsCheck).get.isOk shouldBe false
        vResults.find(_.check == PoolDigestTimestampAuthCheck).get.isOk shouldBe false
        vResults.find(_.check == PoolDigestTimestampResultSignatureCheck).get.isOk shouldBe false
        vResults.find(_.check == PoolDigestTimestampBeforeDrawTimeCheck).get.isOk shouldBe false
      }
    }

    scenario("apply validation to pool with corrupted timestamp-data") {
      val poolResourceProvider = new PoolResourceProviderDirectoryImpl(defaultPoolLocation,
        new OrderDocumentsParserPlayImpl(CompositeProductOrderFactory.allProductsOrderFactory)
      ) {
        override def getPoolDigestTimestamp(): Try[PoolDigestTimestamp] = {
          val timestamp_ok = super.getPoolDigestTimestamp().get
          val timestamp_corrupted = timestamp_ok.copy(rawData = corruptByte(origData = timestamp_ok.rawData, position = 5, corruptionFnct = _ => 0))
          Success(timestamp_corrupted)
        }
      }
      val v = new PoolValidatorImpl(poolResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)

      val vResults = v.validatePoolSeal(
        defaultResourceProvider.getAllOrderIds_synchronous(),
        poolMetadata = defaultResourceProvider.getPoolMetadata(),
        defaultPoolDrawTime
      )
      withClue(s"validatePoolSeal() results:\n${vResults.mkString("\n")}") {
        vResults.count(!_.isOk) should be > 0
        vResults.find(_.check == PoolDigestCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampExistsCheck).get.isOk shouldBe true
        vResults.find(_.check == PoolDigestTimestampAuthCheck).get.isOk shouldBe false
        vResults.find(_.check == PoolDigestTimestampResultSignatureCheck).get.isOk shouldBe false
        vResults.find(_.check == PoolDigestTimestampBeforeDrawTimeCheck).get.isOk shouldBe false
      }
    }
  }

  feature("validator.validatePool()") {

    scenario("a valid pool location is validated") {
      val intermediateResults = Seq.newBuilder[OrderValidationResult]

      val intermediateResultCb = new IntermediateResultCallback {
        override def onOrderValidated(result: OrderValidationResult, countValidatedOrders: Int): Unit = {
          this.synchronized { // ==> needed since v.validatePool() uses parallel collections
            intermediateResults += result
          }
        }
      }

      val v = new PoolValidatorImpl(defaultResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)

      whenReady(v.validatePool(defaultPoolDrawTime, intermediateResultCb).runAsync) { result =>
        result.poolLocation shouldBe Paths.get("") //relative path
        result.poolValidationResults.size shouldBe PoolSealCheck.values.size
        result.poolValidationResults.map(_.check).toSet shouldBe PoolSealCheck.values.toSet

        val ordersCount = defaultResourceProvider.getOrdersCount().get
        ordersCount should be > 0

        withClue(s"#result.orderValidationResults\n${result.orderValidationResults.mkString("\n")}") {
          result.orderValidationResults.size shouldBe ordersCount
          result.orderValidationResults.forall(_.checkResults.size == ordersCount * Check.allChecks.size)
        }
        intermediateResults.result().size shouldBe ordersCount
      }
    }

    scenario("pool location contains no order-directories") {
      val poolResourceProvider = new PoolResourceProviderDirectoryImpl(defaultPoolLocation,
        new OrderDocumentsParserPlayImpl(CompositeProductOrderFactory.allProductsOrderFactory)
      ) {
        override def getOrderDocsObservable(): Observable[Try[OrderDocs]] = {
          Observable.fromIterator(Seq.empty[Try[OrderDocs]].toIterator)
        }
      }
      val v = new PoolValidatorImpl(poolResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)
      whenReady(v.validatePool(defaultPoolDrawTime, defaultIntermediateResultHandler).runAsync.failed) { t =>
        t.getMessage should include("Validation of participation pool not possible: the pool does not contain any orders.")
      }
    }

    scenario("order-directories cannot be obtained") {
      val poolResourceProvider = new PoolResourceProviderDirectoryImpl(defaultPoolLocation,
        new OrderDocumentsParserPlayImpl(CompositeProductOrderFactory.allProductsOrderFactory)
      ) {
        override def getOrderDocsObservable(): Observable[Try[OrderDocs]] = {
          Observable.raiseError(new Exception("an error occurred"))
        }
      }
      val v = new PoolValidatorImpl(poolResourceProvider, credentialsProvider, DefaultSignatureAlgMapper)
      whenReady(v.validatePool(defaultPoolDrawTime, defaultIntermediateResultHandler).runAsync.failed) { t =>
        t.getMessage should include("Validation of participation pool not possible; an error occurred")
      }
    }

    scenario("an orderDir does not contain an order-document") {
      testPoolDoesNotContainCertainOrderDocInNthOrderDir(orderDirIndex = 2, PRType.Order)
    }

    scenario("an orderDir does not contain an order.signature-document") {
      testPoolDoesNotContainCertainOrderDocInNthOrderDir(orderDirIndex = 2, PRType.OrderSignature)
    }

    scenario("an orderDir does not contain an order.result-document") {
      testPoolDoesNotContainCertainOrderDocInNthOrderDir(orderDirIndex = 2, PRType.OrderResult)
    }

    scenario("an orderDir does not contain an order.result.signature-document") {
      testPoolDoesNotContainCertainOrderDocInNthOrderDir(orderDirIndex = 2, PRType.OrderResultSignature)
    }

    scenario("an orderDir does not contain an order.result.signature.timestamp-document") {
      testPoolDoesNotContainCertainOrderDocInNthOrderDir(orderDirIndex = 2, PRType.OrderResultSignatureTimestamp)
    }
  }

  private def testPoolDoesNotContainCertainOrderDocInNthOrderDir(orderDirIndex: Int, missingOrderDoc: PRType.Value): Unit = {
    val allOrderIds = defaultResourceProvider.getAllOrderIds_synchronous()

    val orderIdWithMissingDoc = allOrderIds(orderDirIndex)

    val orderDocFilename = PoolResource.docTypeToFileName(missingOrderDoc)

    val orderDocNotFoundException = new FileNotFoundException(s"${orderDocFilename} doc not found!")
    val prov = new PoolResourceProviderDirectoryImpl(defaultPoolLocation,
      new OrderDocumentsParserPlayImpl(CompositeProductOrderFactory.allProductsOrderFactory)
    ) {

      /** Overridden to simulate a certain file missing */
      override def getOrderDocsObservable(): Observable[Try[OrderDocs]] = {
        if (poolDirPath.toFile.isFile) Observable.raiseError(new IllegalArgumentException(s"$poolDirPath is a File (directory expected)")) else {
          Observable.fromIterator(poolDirPath.toFile.listFiles(DirectoryFilter).toIterator)
            .map(orderDir => getOrderDocs(orderDir.getName)).map { orderDocs =>
            if (orderDocs.get.orderId != orderIdWithMissingDoc) orderDocs else {
              missingOrderDoc match {
                case PRType.MetaData => orderDocs.map(_.copy(orderMetadata = Failure(orderDocNotFoundException)))
                case PRType.Order => orderDocs.map(_.copy(order = Failure(orderDocNotFoundException)))
                case PRType.OrderResult => orderDocs.map(_.copy(orderResult = Failure(orderDocNotFoundException)))
                case PRType.OrderResultSignature => orderDocs.map(_.copy(orderResultSignature = Failure(orderDocNotFoundException)))
                case PRType.OrderResultSignatureTimestamp => orderDocs.map(_.copy(orderResultSignatureTimestamp = Failure(orderDocNotFoundException)))
                case PRType.OrderSignature => orderDocs.map(_.copy(orderSignature = Failure(orderDocNotFoundException)))
                case PRType.OrderMetadata => orderDocs.map(_.copy(orderMetadata = Failure(orderDocNotFoundException)))
                case x => sys.error(s"unexpected arg: $x")
              }
            }
          }
        }
      }
    }

    val v = new PoolValidatorImpl(prov, credentialsProvider, DefaultSignatureAlgMapper)
    whenReady(v.validatePool(defaultPoolDrawTime, defaultIntermediateResultHandler).runAsync) { vResult =>
      val allOrderVResults = vResult.orderValidationResults.flatMap(_.checkResults)
      withClue(s"order validation results:\n${allOrderVResults.mkString("\n")}") {
        val orderVResultsWithErrors = vResult.orderValidationResults.filter(_.checkResults.exists(!_.isOk))
        withClue("#orderVResultsWithErrors with validation errors")(orderVResultsWithErrors.size shouldBe 1)
        orderVResultsWithErrors.head.checkResults.find(_.check == AllOrderDocumentsMustExistCheck) match {
          case Some(failure: CheckFailure) => failure.message shouldBe v.getErrorTxtMissingOrInvalidOrderDoc(missingOrderDoc, orderDocNotFoundException)
          case x => fail(s"check AllOrderDocumentsMustExistCheck should fail")
        }
        vResult.poolValidationResults.map(_.check).toSet shouldBe PoolSealCheck.values.toSet
      }
    }
  }

  private def corruptByte(origData: IndexedSeq[Byte], position: Int, corruptionFnct: Byte => Byte): IndexedSeq[Byte] = {
    position should be >= 0
    position should be < origData.length
    origData.updated(position, corruptionFnct(origData(position)))
  }
}
