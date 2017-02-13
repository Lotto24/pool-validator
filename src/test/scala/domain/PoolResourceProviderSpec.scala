package domain

import java.io.{ByteArrayInputStream, File, InputStream}
import java.nio.charset.StandardCharsets
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Path, Paths}
import java.time._
import java.time.DayOfWeek._
import java.time.format.DateTimeParseException

import domain.OrderMetadata.{DrawHedgingData, OrderHedgingData}
import domain.PoolResource.Filenames
import domain.products.GamingProduct.GamingProductId
import domain.products.ML24GamingProduct
import domain.products.amls.{AmlsBet, AmlsParticipationPools}
import domain.products.aols.{AolsBet, AolsParticipationPools}
import domain.products.apls.{AplsBet, AplsParticipationPools}
import domain.products.asls.{AslsBet, AslsParticipationPools}
import domain.products.awls.{AwlsBet, AwlsParticipationPools}
import domain.products.ejs.{EjsBet, EjsGamingProductOrder, EjsParticipationPools}
import domain.products.ems.{EmsBet, EmsParticipationPools}
import domain.products.emsplus.{EmsPlusBet, EmsPlusParticipationPools}
import domain.products.fls.{FlsBet, FlsParticipationPools}
import domain.products.gls.{GlsBet, GlsParticipationPools}
import domain.products.glss.{GlsSBet, GlsSParticipationPools}
import domain.products.irishraffle.{IrishRaffleBet, IrishRaffleParticipationPools}
import domain.products.irls.p1.IrlsP1ParticipationPools
import domain.products.irls.{IrlsBet, IrlsParticipationPools}
import domain.products.pls.{PlsBet, PlsParticipationPools}
import domain.products.plus5.{Plus5Bet, Plus5ParticipationPools}
import domain.products.s6.{S6Bet, S6ParticipationPools}
import domain.products.s77.{S77Bet, S77ParticipationPools}
import domain.products.sls.{SlsBet, SlsParticipationPools}
import domain.products.ukls.{UklsBet, UklsParticipationPools}
import domain.products.uktbls.{UktblsBet, UktblsParticipationPools}
import domain.products.xmasl.{XmaslBet, XmaslParticipationPools}
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
      val provider = new PoolResourceProviderImpl
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
      val provider = new PoolResourceProviderImpl{

        override protected def getInputStream(filePath: Path) : Try[InputStream] = Try{
          val resourceName = filePath.getFileName.toString
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
      val provider = new PoolResourceProviderImpl
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
          EjsBet(numbers = Seq(1, 2, 3, 4, 5), euroNumbers = Seq(1, 8)),
          EjsBet(numbers = Seq(2, 4, 6, 29, 32), euroNumbers = Seq(4, 5))
        )

      val expectedPartPools = EjsParticipationPools(LocalDate.of(2015, 12, 18), drawCount = 8)

      val expectedOrder = EjsGamingProductOrder(expectedBets, expectedPartPools, variant = None, json= play.api.libs.json.JsObject(Seq.empty))

      withClue(s"metaData.gamingProductOrders(${EjsGamingProductOrder.productURI})") {
        o.gamingProductOrders(EjsGamingProductOrder.productURI).withEmptyJson() shouldEqual expectedOrder
      }
    }

    scenario("parsing order with all products"){
      val orderFile = new File(workingDir, "src/test/resources/orderdocs/order/orderWithAllProducts.json")
      val provider = new PoolResourceProviderImpl
      val o: Order = provider.getOrderForFilePath(orderFile.toPath).get

      withClue("docPath")(o.docPath shouldEqual orderFile.toPath)

      withClue("metaData.creationDate")(o.metaData.creationDate.toString shouldEqual "2017-02-05T13:22:47.123Z")
      withClue("metaData.retailCustomer")(o.metaData.retailCustomer shouldEqual "ff97acdb-d79a-4f8b-b6ae-8ada63d9ea38")
      withClue("metaData.retailerHref")(o.metaData.retailerHref shouldEqual "http://zoe.mylotto24.co.uk/entities/mylotto24")
      withClue("metaData.retailerOrderReference")(o.metaData.retailerOrderReference shouldEqual "8e0c2944-6303-4ed2-8342-2f3bbfe921bd")
      withClue("order.gamingProductOrders"){
        val productIdsInOrder: Set[GamingProductId] = o.gamingProductOrders.keys.map{uri =>
          domain.products.GamingProduct.gamingProductIdFromURI(uri)
        }.toSet
        val allProductIds = ML24GamingProduct.All.map(_.id).toSet
        withClue(s"order.gamingProductOrders difference: ${ allProductIds diff productIdsInOrder}"){
          productIdsInOrder shouldEqual allProductIds 
        }
      }
      withClue("order.gamingProductOrders.size")(o.gamingProductOrders.size shouldEqual ML24GamingProduct.All.size)

      import ML24GamingProduct._
      import domain.products.GamingProduct.gamingProductIdToURI

      val ejsGpo = o.gamingProductOrders(gamingProductIdToURI(EJS.id))
      withClue(s"ejsGpo $ejsGpo"){
        ejsGpo.variant shouldBe Some("variant_1")
        ejsGpo.bets shouldBe Seq(
          EjsBet(numbers = Seq(1, 2, 3, 4, 5), euroNumbers = Seq(1, 8)),
          EjsBet(numbers = Seq(2, 4, 6, 29, 32), euroNumbers = Seq(4, 5))
        )
        ejsGpo.participationPools shouldBe EjsParticipationPools(firstDate = LocalDate.of(2017, 2, 10), drawCount = 1)
      }
      
      val glsGpo = o.gamingProductOrders(gamingProductIdToURI(GLS.id))
      withClue(s"glsGpo:$glsGpo"){
        glsGpo.variant shouldBe Some("variant_1")
        glsGpo.bets shouldBe Seq(
          GlsBet(numbers = Seq(1, 2, 3, 4, 5, 6), supernumber = 7, system = None),
          GlsBet(numbers = Seq(1, 2, 4, 6, 29, 32, 49), supernumber = 0, system = Some("full"))
        )
        glsGpo.participationPools shouldBe GlsParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
      }
      
      val s6Gpo = o.gamingProductOrders(gamingProductIdToURI(S6.id))
      withClue(s"s6Gpo:$s6Gpo"){
        s6Gpo.variant shouldBe Some("variant_1")
        s6Gpo.bets shouldBe Seq(
          S6Bet(numbers = Seq(0, 8, 1, 8, 3, 6)),
          S6Bet(numbers = Seq(3, 6, 2, 8, 1, 9))
        )
        s6Gpo.participationPools shouldBe S6ParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
      }
      
      
      val s77Gpo = o.gamingProductOrders(gamingProductIdToURI(S77.id))
      withClue(s"s77Gpo:$s77Gpo"){
        s77Gpo.variant shouldBe Some("variant_1")
        s77Gpo.bets shouldBe Seq(
          S77Bet(numbers = Seq(7, 0, 8, 1, 8, 3, 6)),
          S77Bet(numbers = Seq(3, 6, 1, 8, 1, 9, 7))
        )
        s77Gpo.participationPools shouldBe S77ParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
      }

      
      val emsGpo = o.gamingProductOrders(gamingProductIdToURI(EMS.id))
      withClue(s"emsGpo:$emsGpo"){
        emsGpo.variant shouldBe Some("variant_1")
        emsGpo.bets shouldBe Seq(
          EmsBet(numbers=Seq(1, 2, 3, 4, 5), starnumbers = Seq(1, 11)),
          EmsBet(numbers=Seq(2, 4, 6, 29, 32), starnumbers = Seq(2, 10))
        )
        emsGpo.participationPools shouldBe EmsParticipationPools(firstDate = LocalDate.of(2017, 2, 7), drawDays = Set(TUESDAY), drawCount = 1)
      }


      val glssGpo = o.gamingProductOrders(gamingProductIdToURI(GLSS.id))
      withClue(s"glssGpo:$glssGpo"){
        glssGpo.variant shouldBe Some("variant_1")
        glssGpo.bets shouldBe Seq( 
          GlsSBet(numbers = Seq(8, 1, 9, 2, 9, 4, 7)), 
          GlsSBet(numbers = Seq(2, 5, 0, 7, 0, 8, 6))
        )
        glssGpo.participationPools shouldBe GlsSParticipationPools(firstDate = LocalDate.of(2017, 2, 11), drawCount = 1)
      }


      val emsplusGpo = o.gamingProductOrders(gamingProductIdToURI(EMSPLUS.id))
      withClue(s"emsplusGpo:$emsplusGpo"){
        emsplusGpo.variant shouldBe Some("variant_1")
        emsplusGpo.bets shouldBe Seq(
          EmsPlusBet(numbers = Seq(1, 2, 3, 4, 5)),
          EmsPlusBet(numbers = Seq(2, 4, 6, 29, 32))
        )
        emsplusGpo.participationPools shouldBe EmsPlusParticipationPools(firstDate = LocalDate.of(2017, 2, 7), drawDays = Set(TUESDAY), drawCount = 1)
      }
      
      val irlsGpo = o.gamingProductOrders(gamingProductIdToURI(IRLS.id))
      withClue(s"irlsGpo:$irlsGpo"){
        irlsGpo.variant shouldBe Some("variant_1")
        irlsGpo.bets shouldBe Seq(IrlsBet(numbers = Seq(8, 1, 9, 2, 7, 4)))
        irlsGpo.participationPools shouldBe IrlsParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
      }

      val irlsp1Gpo = o.gamingProductOrders(gamingProductIdToURI(IRLSP1.id))
      withClue(s"irlsp1Gpo:$irlsp1Gpo"){
        irlsp1Gpo.variant shouldBe Some("variant_1")
        irlsp1Gpo.bets shouldBe Seq(IrlsBet(numbers = Seq(8, 1, 9, 2, 7, 4)))
        irlsp1Gpo.participationPools shouldBe IrlsP1ParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
      }

      val irlsp2Gpo = o.gamingProductOrders(gamingProductIdToURI(IRLSP2.id))
      withClue(s"irlsp2Gpo:$irlsp2Gpo"){
        irlsp2Gpo.variant shouldBe Some("variant_1")
        irlsp2Gpo.bets shouldBe Seq(
          IrlsBet(numbers = Seq(8, 1, 9, 2, 7, 4))
        )
        irlsp1Gpo.participationPools shouldBe IrlsP1ParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
      }

      val irishraffleGpo = o.gamingProductOrders(gamingProductIdToURI(IRISHRAFFLE.id))
      withClue(s"irishraffleGpo:$irishraffleGpo"){
        irishraffleGpo.variant shouldBe Some("variant_1")
        irishraffleGpo.bets shouldBe Seq(
          IrishRaffleBet(numbers = Seq(1, 2, 3, 4)),
          IrishRaffleBet(numbers = Seq(0, 4, 6, 9))
        )
        irishraffleGpo.participationPools shouldBe IrishRaffleParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
      }

      val uklsGpo = o.gamingProductOrders(gamingProductIdToURI(UKLS.id))
      withClue(s"uklsGpo:$uklsGpo"){
        uklsGpo.variant shouldBe Some("variant_1")
        uklsGpo.bets shouldBe Seq(UklsBet(numbers = Seq(8, 1, 9, 2, 7, 4)))
        uklsGpo.participationPools shouldBe UklsParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
      }

      val aolsGpo = o.gamingProductOrders(gamingProductIdToURI(AOLS.id))
      withClue(s"aolsGpo:$aolsGpo"){
        aolsGpo.variant shouldBe Some("variant_1")
        aolsGpo.bets shouldBe Seq(AolsBet(numbers = Seq(2, 3, 4, 5, 6, 7, 8)))
        aolsGpo.participationPools shouldBe AolsParticipationPools(firstDate = LocalDate.of(2017, 2, 7), drawCount = 1)
      }

      val aplsGpo = o.gamingProductOrders(gamingProductIdToURI(APLS.id))
      withClue(s"aplsGpo:$aplsGpo"){
        aplsGpo.variant shouldBe Some("variant_1")
        aplsGpo.bets shouldBe Seq(
          AplsBet(numbers = Seq(1, 2, 3, 4, 5, 6), powerball = 2),
          AplsBet(numbers = Seq(2, 3, 4, 5, 6, 7), powerball = 3)
        )
        aplsGpo.participationPools shouldBe AplsParticipationPools(firstDate = LocalDate.of(2017, 2, 9), drawCount = 1)
      }

      val aslsGpo = o.gamingProductOrders(gamingProductIdToURI(ASLS.id))
      withClue(s"aslsGpo:$aslsGpo"){
        aslsGpo.variant shouldBe Some("variant_1")
        aslsGpo.bets shouldBe Seq(
          AslsBet(numbers = Seq(1, 2, 3, 4, 5, 6)),
          AslsBet(numbers = Seq(2, 3, 4, 5, 6, 7))
        )
        aslsGpo.participationPools shouldBe AslsParticipationPools(firstDate = LocalDate.of(2017, 2, 11), drawCount = 1)
      }
      
      val amlsGpo = o.gamingProductOrders(gamingProductIdToURI(AMLS.id))
      withClue(s"amlsGpo:$amlsGpo"){
        amlsGpo.variant shouldBe Some("variant_1")
        amlsGpo.bets shouldBe Seq(AmlsBet(numbers = Seq(1, 2, 3, 4, 5, 6)))
        amlsGpo.participationPools shouldBe AmlsParticipationPools(firstDate = LocalDate.of(2017, 2, 13), drawCount = 1)
      }

      val awlsGpo = o.gamingProductOrders(gamingProductIdToURI(AWLS.id))
      withClue(s"awlsGpo:$awlsGpo"){
        awlsGpo.variant shouldBe Some("variant_1")
        awlsGpo.bets shouldBe Seq(AwlsBet(numbers = Seq(1, 2, 3, 4, 5, 6)))
        awlsGpo.participationPools shouldBe AwlsParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawCount = 1)
      }

      val slsGpo = o.gamingProductOrders(gamingProductIdToURI(SLS.id))
      withClue(s"slsGpo:$slsGpo"){
        slsGpo.variant shouldBe Some("variant_1")
        slsGpo.bets shouldBe Seq(SlsBet(numbers = Seq(1, 2, 3, 4, 5, 6, 7)))
        slsGpo.participationPools shouldBe SlsParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
      }

      val uktblsGpo = o.gamingProductOrders(gamingProductIdToURI(UKTBLS.id))
      withClue(s"uktblsGpo:$uktblsGpo"){
        uktblsGpo.variant shouldBe Some("variant_1")
        uktblsGpo.bets shouldBe Seq(
          UktblsBet(numbers = Seq(1, 2, 3, 4, 5), thunderball = 10),
          UktblsBet(numbers = Seq(6, 7, 8, 9, 10), thunderball = 11)
        )
        uktblsGpo.participationPools shouldBe UktblsParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
      }

      val flsGpo = o.gamingProductOrders(gamingProductIdToURI(FLS.id))
      withClue(s"flsGpo:$flsGpo"){
        flsGpo.variant shouldBe Some("variant_1")
        flsGpo.bets shouldBe Seq(
          FlsBet(numbers = Seq(1, 2, 3, 4, 5), chancenumber = 2),
          FlsBet(numbers = Seq(2, 3, 4, 5, 6), chancenumber = 3)
        )
        flsGpo.participationPools shouldBe FlsParticipationPools(firstDate = LocalDate.of(2017, 2, 6), drawDays = Set(MONDAY), drawCount = 1)
      }

      val plsGpo = o.gamingProductOrders(gamingProductIdToURI(PLS.id))
      withClue(s"plsGpo:$plsGpo"){
        plsGpo.variant shouldBe Some("variant_1")
        plsGpo.bets shouldBe Seq(PlsBet(numbers = Seq(1, 2, 3, 4, 5, 6)))
        plsGpo.participationPools shouldBe PlsParticipationPools(firstDate = LocalDate.of(2017, 2, 7), drawDays = Set(TUESDAY), drawCount = 1)
      }

      val xmaslGpo = o.gamingProductOrders(gamingProductIdToURI(XMASL.id))
      withClue(s"xmaslGpo:$xmaslGpo"){
        xmaslGpo.variant shouldBe Some("variant_1")
        xmaslGpo.bets shouldBe Seq(XmaslBet(numbers = Seq(1, 2, 3, 4, 5)))
        xmaslGpo.participationPools shouldBe XmaslParticipationPools(firstDate = LocalDate.of(2017, 12, 22))
      }

      val plus5Gpo = o.gamingProductOrders(gamingProductIdToURI(PLUS5.id))
      withClue(s""){
        plus5Gpo.variant shouldBe Some("variant_1")
        plus5Gpo.bets shouldBe Seq(
          Plus5Bet(numbers = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9)),
          Plus5Bet(numbers = Seq(2, 3, 4, 5, 6, 7, 8, 9, 10))
        )
        plus5Gpo.participationPools shouldBe Plus5ParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawCount = 1)
      }
    }
  }

  feature("Parsing of an order.result") {
    scenario("Parsing of a valid order.result should succeed") {
      val provider = new PoolResourceProviderImpl
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
      val provider = new PoolResourceProviderImpl
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
      val provider = new PoolResourceProviderImpl
      val file = new File(workingDir, "src/test/resources/validorders/07ggP6UWr8P_KAXApgtnnoLxu-_LclmsUTq7nvBzWcw/order.result.signature.timestamp")
      val o: OrderResultSignatureTimestamp = provider.getOrderResultSignatureTimestamp(file.toPath.getParent).get

      withClue("docPath")(o.docPath shouldEqual file.toPath)
      withClue("rawData")(o.rawData shouldEqual Utils.getAsSeq(file).get)
      withClue("value")(o.value shouldEqual "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")
    }
  }

  feature("Parsing of an order.signature") {
    scenario("Parsing of a order.signature should succeed") {
      val provider = new PoolResourceProviderImpl
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

  feature("Parsing of an order.metadata"){
    scenario("Parsing of a order.metadata should succeed") {
      val provider = new PoolResourceProviderImpl
      val file = new File(workingDir, "src/test/resources/orderdocs/order.metadata/order.metadata")
      val m: OrderMetadata = provider.getOrderMetadataForFilePath(file.toPath).get

      withClue("docPath")(m.docPath shouldEqual file.toPath)

      val expectedHedgingData = OrderHedgingData(
        Map(
          "ems" -> Seq(DrawHedgingData(poolId="ems/2016-08-19", LocalDate.of(2016, 8, 19), hedgingChannel=Some("ILS")))
        )
      )
      withClue("hedgingData")(m.hedgingData shouldEqual expectedHedgingData)
    }
    
    scenario("parse order.metadata with syntax error"){
      val provider = new PoolResourceProviderImpl
      val file = new File(workingDir, "src/test/resources/orderdocs/order.metadata/order.metadata_withSyntaxError")
      val r = provider.getOrderMetadataForFilePath(file.toPath)
      r.isFailure shouldEqual true
    }
  }
}