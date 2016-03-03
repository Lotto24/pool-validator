package domain

import java.io.{ByteArrayInputStream, File, InputStream}
import java.nio.charset.StandardCharsets
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Path, Paths}
import java.time.format.DateTimeParseException
import java.time.{LocalDate, LocalTime, ZoneOffset, ZonedDateTime}

import domain.PoolResource.Filenames
import domain.products.ML24GamingProduct
import domain.products.ejs.{EjsBet, EjsGamingProductOrder, EjsParticipationPools, EjsProductOrderFactory}
import org.scalatest.{FeatureSpec, Matchers}
import play.api.libs.json.Json
import util.Utils
import util.Utils.fileInputStreamOf

import scala.util.{Failure, Success, Try}


class PoolResourceProviderSpec extends FeatureSpec with Matchers {

  val workingDir = new File(System.getProperty("user.dir"))

  feature("Read pool archive metadata") {

    val metadataFile = new File(workingDir, "src/test/resources/validorders/metadata.json")
    scenario("obtain metadata from well-named pool directory ('-'-separated)") {
      val provider = new PoolResourceProviderImpl(new EjsProductOrderFactory)
      provider.getPoolMetadata(poolDirPath=metadataFile.toPath.getParent) match {
        case Success(poolInfo: PoolMetadata) =>
          poolInfo.drawDate shouldBe ZonedDateTime.of(LocalDate.of(2015, 12, 25), LocalTime.of(18, 0, 0), ZoneOffset.UTC)
          poolInfo.participationPoolId shouldBe "ejs/2015-12-25"
          poolInfo.productId shouldBe ML24GamingProduct.EJS.id
          poolInfo.docPath shouldBe metadataFile.toPath
        case Failure(t) => fail(t)
      }
    }

    scenario("obtain metadata from mal-named pool directory (invalid draw-date)") {
      val provider = new PoolResourceProviderImpl(new EjsProductOrderFactory){
        override protected def getInputStream(baseDir: Path, resourceName: String) : Try[InputStream] = Try{
          resourceName match {
            case Filenames.Metadata =>
              val metaDataBytes = Json.obj(
                "gaming-product" -> "http://zoe.mylotto24.co.uk/gaming-products/mylotto24/ejs",
                "participation-pool-id" -> "ejs/2016-01-29",
                "participation-pool-digest" -> Map(
                  "base64" -> "mevsH/v7+OdQwto4gzGD713EfsR3+ZQriDSWHB3mllM=",
                  "algorithm" -> "SHA-256"
                ),
                "draw-time" -> "2016-01-32T18:00:00Z"
              ).toString.getBytes(UTF_8)
              new ByteArrayInputStream(metaDataBytes)
          }
        }
      }

      provider.getPoolMetadata(Paths.get("dummyPath")) match {
        case Success(poolInfo: PoolMetadata) => fail("should be Failure()")
        case Failure(t) => t.getClass shouldBe classOf[DateTimeParseException]
      }
    }
  }

  feature("Parsing of an order") {
    scenario("Parsing of a valid order should succeed") {
      info(s"workingDir: $workingDir")

      val orderFile = new File(workingDir, "src/test/resources/validorders/07ggP6UWr8P_KAXApgtnnoLxu-_LclmsUTq7nvBzWcw/order")
      val provider = new PoolResourceProviderImpl(new EjsProductOrderFactory)
      val o: Order = provider.getOrder(orderFile.toPath.getParent).get

      withClue("docPath")(o.docPath shouldEqual orderFile.toPath)
      withClue("directoryName")(o.directoryName shouldEqual "07ggP6UWr8P_KAXApgtnnoLxu-_LclmsUTq7nvBzWcw")
      withClue("rawData")(o.rawData shouldEqual Utils.getAsSeq(orderFile).get)
      withClue("metaData.creationDate")(o.metaData.creationDate.toString shouldEqual "2015-12-13T01:26:35.101Z[UTC]")
      withClue("metaData.retailCustomer")(o.metaData.retailCustomer shouldEqual "4ce49757-5a95-42a4-9e2f-db59d4f06c7b")
      withClue("metaData.retailerHref")(o.metaData.retailerHref shouldEqual "http://www.operator.com/entities/retailer")
      withClue("metaData.retailerOrderReference")(o.metaData.retailerOrderReference shouldEqual "3581044c-be0d-4f4d-b86e-69c3e7c05cb2")
      withClue("metaData.retailerOrderReference")(o.gamingProductOrders.size shouldEqual 1)

      val expectedBets =
        Seq(
          EjsBet(numbers = Set(1, 2, 3, 4, 5), euroNumbers = Set(1, 8)),
          EjsBet(numbers = Set(2, 4, 6, 29, 32), euroNumbers = Set(4, 5))
        )

      val expectedPartPools = EjsParticipationPools(LocalDate.of(2015, 12, 18), drawCount = 8)

      val expectedOrder = EjsGamingProductOrder(expectedBets, participationPools = expectedPartPools)

      withClue(s"metaData.gamingProductOrders(${EjsGamingProductOrder.productURI})") {
        o.gamingProductOrders(EjsGamingProductOrder.productURI) shouldEqual expectedOrder
      }
      withClue("metaData.retailerOrderReference")(o.gamingProductOrders(EjsGamingProductOrder.productURI) shouldEqual expectedOrder)
    }
  }

  feature("Parsing of an order.result") {
    scenario("Parsing of a valid order.result should succeed") {
      val provider = new PoolResourceProviderImpl(new EjsProductOrderFactory)
      val file = new File(workingDir, "src/test/resources/validorders/07ggP6UWr8P_KAXApgtnnoLxu-_LclmsUTq7nvBzWcw/order.result")
      val o: OrderResult = provider.getOrderResult(file.toPath.getParent).get
      withClue("docPath")(o.docPath shouldEqual file.toPath)
      withClue("rawData")(o.rawData shouldEqual Utils.getAsSeq(file).get)
      withClue("creationTime")(o.creationTime.format(OrderResult.dateTimeFormat) shouldEqual "2015-12-13T01:26:35.934Z")
      withClue("orderDigest")(o.orderDigest shouldEqual "sha256=07ggP6UWr8P/KAXApgtnnoLxu+/LclmsUTq7nvBzWcw=".getBytes(UTF_8))
      withClue("orderProcessingResult")(o.orderProcessingResult shouldEqual OrderResult.Accepted)
      withClue("retailerHref")(o.retailerHref shouldEqual "http://www.operator.com/entities/retailer")
      withClue("retailerOrderReference")(o.retailerOrderReference shouldEqual "3581044c-be0d-4f4d-b86e-69c3e7c05cb2")
    }
  }

  feature("Parsing of an order.result.signature") {
    scenario("Parsing of a valid order.result.signature should succeed") {
      val provider = new PoolResourceProviderImpl(new EjsProductOrderFactory)
      val file = new File(workingDir, "src/test/resources/validorders/07ggP6UWr8P_KAXApgtnnoLxu-_LclmsUTq7nvBzWcw/order.result.signature")

      val o: OrderResultSignature = provider.getOrderResultSignature(file.toPath.getParent).get

      withClue("algorithm")(o.algorithm shouldEqual "rsa-sha256")
      withClue("keyId")(o.keyId shouldEqual "ZOE_signkey_01")
      withClue("signature") {
        new String(o.signature.toArray, UTF_8) shouldEqual
          "YlT+3bPwdmsTAZgLrr2WlXDAWuwQYzHcR2j5nq/tXuSC0mBDh/Q51+utvLj8Y9QcTOLCWLGUr5hDJySgx55WzZNLyziDu38l+Eqwg4HK6RtTgw0BI47GshQ0acfHcP6iRyPjEUD3GEMP75WAVIiCvj8G3ZcyXk10J4FEecPMZZE="
      }
      withClue("docPath")(o.docPath shouldBe file.toPath)
    }
  }

  feature("Parsing of an order.result.signature.timestamp") {
    scenario("Parsing of a order.result.signature.timestamp should succeed") {
      val provider = new PoolResourceProviderImpl(new EjsProductOrderFactory)
      val file = new File(workingDir, "src/test/resources/validorders/07ggP6UWr8P_KAXApgtnnoLxu-_LclmsUTq7nvBzWcw/order.result.signature.timestamp")
      val o: OrderResultSignatureTimestamp = provider.getOrderResultSignatureTimestamp(file.toPath.getParent).get

      withClue("docPath")(o.docPath shouldEqual file.toPath)
      withClue("rawData")(o.rawData shouldEqual Utils.getAsSeq(file).get)
      withClue("value")(o.value shouldEqual "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")
    }
  }

  feature("Parsing of an order.signature") {
    scenario("Parsing of a order.signature should succeed") {
      val provider = new PoolResourceProviderImpl(new EjsProductOrderFactory)
      val file = new File(workingDir, "src/test/resources/validorders/07ggP6UWr8P_KAXApgtnnoLxu-_LclmsUTq7nvBzWcw/order.signature")

      val o: OrderSignature = provider.getOrderSignature(file.toPath.getParent).get

      withClue("docPath")(o.docPath shouldEqual file.toPath)
      withClue("rawData")(o.rawData shouldEqual Utils.getAsSeq(file).get)
      withClue("algorithm")(o.algorithm shouldEqual "rsa-sha256")
      withClue("keyId")(o.keyId shouldEqual "unknown")
      withClue("signature")(o.signature shouldEqual
        "C0E1L2UcS1SfilFTZTr7Ya2Nk8mxC9O2l8XJnXVI4txgdX9cLoitJ0AKenY0nrqoycLt9qmNUPzNk7wGlOZFjnZJRP+UhInba5lbpBnG49/wT+XrWnZd7ImMofxzBKuZRsJW7zX4QW3OIzt6uB65WkLZbW+2Zclfi2AmGYVDqvk="
          .getBytes(StandardCharsets.UTF_8))
    }
  }
}