package domain

import java.io.File
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Paths
import java.time.DayOfWeek.{MONDAY, THURSDAY, TUESDAY, WEDNESDAY}
import java.time.{DayOfWeek, LocalDate, ZoneId, ZonedDateTime}

import domain.OrderMetadata.{DrawHedgingData, OrderHedgingData}
import domain.PoolMetadata.PoolDigest
import domain.products.GamingProduct.{GamingProductId, gamingProductIdToURI}
import domain.products.ML24GamingProduct._
import domain.products.amls.{AmlsBet, AmlsGamingProductOrder, AmlsParticipationPools}
import domain.products.aols.{AolsBet, AolsGamingProductOrder, AolsParticipationPools}
import domain.products.apls.{AplsBet, AplsGamingProductOrder, AplsParticipationPools}
import domain.products.asls.{AslsBet, AslsGamingProductOrder, AslsParticipationPools}
import domain.products.awls.{AwlsBet, AwlsGamingProductOrder, AwlsParticipationPools}
import domain.products.c4ls.{C4lsBet, C4lsGamingProductOrder, C4lsParticipationPools}
import domain.products.ejs.{EjsBet, EjsGamingProductOrder, EjsParticipationPools}
import domain.products.ems.{EmsBet, EmsGamingProductOrder, EmsParticipationPools}
import domain.products.emsplus.{EmsPlusBet, EmsPlusGamingProductOrder, EmsPlusParticipationPools}
import domain.products.fls.{FlsBet, FlsGamingProductOrder, FlsParticipationPools}
import domain.products.gls.{GlsBet, GlsGamingProductOrder, GlsParticipationPools}
import domain.products.glss.{GlsSBet, GlsSGamingProductOrder, GlsSParticipationPools}
import domain.products.irishraffle.{IrishRaffleBet, IrishRaffleGamingProductOrder, IrishRaffleParticipationPools}
import domain.products.irls.p1.{IrlsP1GamingProductOrder, IrlsP1ParticipationPools}
import domain.products.irls.p2.{IrlsP2GamingProductOrder, IrlsP2ParticipationPools}
import domain.products.irls.{IrlsBet, IrlsGamingProductOrder, IrlsParticipationPools}
import domain.products.keno.{KenoBet, KenoGamingProductOrder, KenoParticipationPools}
import domain.products.mmls.{MmlsBet, MmlsGamingProductOrder, MmlsParticipationPools}
import domain.products.pls.{PlsBet, PlsGamingProductOrder, PlsParticipationPools}
import domain.products.plus5.{Plus5Bet, Plus5GamingProductOrder, Plus5ParticipationPools}
import domain.products.s6.{S6Bet, S6GamingProductOrder, S6ParticipationPools}
import domain.products.s77.{S77Bet, S77GamingProductOrder, S77ParticipationPools}
import domain.products.sls.{SlsBet, SlsGamingProductOrder, SlsParticipationPools}
import domain.products.ukls.{UklsBet, UklsGamingProductOrder, UklsParticipationPools}
import domain.products.uktbls.{UktblsBet, UktblsGamingProductOrder, UktblsParticipationPools}
import domain.products.uspbls.{UspblsBet, UspblsGamingProductOrder, UspblsParticipationPools}
import domain.products.xmasl.{XmaslBet, XmaslGamingProductOrder, XmaslParticipationPools}
import domain.products.{GamingProductOrder, ML24GamingProduct}
import org.scalatest.{Assertions, FunSpec, Inside, Matchers}
import util.Utils

import scala.reflect.{ClassTag, classTag}
import scala.util.{Failure, Success}

class OrderDocumentsParserPlayImplTest extends FunSpec with Assertions with Matchers with Inside {

  private val workingDir = new File(System.getProperty("user.dir"))
  private val parser = new OrderDocumentsParserPlayImpl(CompositeProductOrderFactory.allProductsOrderFactory)
  
  describe("Order Parsing") {

    describe("Parsing of an order") {

      it("parsing of a valid order should succeed") {
        val data = jsonBytes(
          """
            |{
            |  "metadata": {
            |    "retailer": {
            |      "href": "http://www.operator.com/entities/retailer"
            |    },
            |    "retail-customer": "4ce49757-5a95-42a4-9e2f-db59d4f06c7b",
            |    "retailer-order-reference": "3581044c-be0d-4f4d-b86e-69c3e7c05cb2",
            |    "creation-date": "2015-12-13T01:26:35.101Z[UTC]"
            |  },
            |  "gaming-product-orders": {
            |    "http://zoe.mylotto24.co.uk/gaming-products/mylotto24/ejs": {
            |      "bets": [
            |        {
            |          "numbers": [
            |            1,
            |            2,
            |            3,
            |            4,
            |            5
            |          ],
            |          "euronumbers": [
            |            1,
            |            8
            |          ]
            |        },
            |        {
            |          "numbers": [
            |            2,
            |            4,
            |            6,
            |            29,
            |            32
            |          ],
            |          "euronumbers": [
            |            4,
            |            5
            |          ]
            |        }
            |      ],
            |      "participation-pools": {
            |        "first-date": "2015-12-18",
            |        "draw-count": 8
            |      }
            |    }
            |  }
            |}
          """.stripMargin)
        

        val o: Order = parser.parseOrder(data, Paths.get("07ggP6UWr8P_KAXApgtnnoLxu-_LclmsUTq7nvBzWcw/order")).get

        withClue("docPath")(o.docPath shouldEqual Paths.get("07ggP6UWr8P_KAXApgtnnoLxu-_LclmsUTq7nvBzWcw/order"))
        withClue("orderId")(o.orderId shouldEqual "07ggP6UWr8P_KAXApgtnnoLxu-_LclmsUTq7nvBzWcw")
        withClue("rawData")(o.rawData shouldEqual data)
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

        val expectedOrder = EjsGamingProductOrder(expectedBets, expectedPartPools, variant = None, json = play.api.libs.json.JsObject(Seq.empty))

        withClue(s"metaData.gamingProductOrders(${EjsGamingProductOrder.productURI})") {
          o.gamingProductOrders(EjsGamingProductOrder.productURI).withEmptyJson() shouldEqual expectedOrder
        }
      }

      it("parsing order with all products should succeed") {
        val orderFile = new File(workingDir, "src/test/resources/orderdocs/order/orderWithAllProducts.json")
        val o: Order = parser.parseOrder(getFileContent(orderFile), Paths.get("orderId/order")).get

        withClue("docPath")(o.docPath shouldEqual Paths.get("orderId/order"))

        withClue("metaData.creationDate")(o.metaData.creationDate.toString shouldEqual "2017-02-05T13:22:47.123Z")
        withClue("metaData.retailCustomer")(o.metaData.retailCustomer shouldEqual "ff97acdb-d79a-4f8b-b6ae-8ada63d9ea38")
        withClue("metaData.retailerHref")(o.metaData.retailerHref shouldEqual "http://zoe.mylotto24.co.uk/entities/mylotto24")
        withClue("metaData.retailerOrderReference")(o.metaData.retailerOrderReference shouldEqual "8e0c2944-6303-4ed2-8342-2f3bbfe921bd")
        withClue("order.gamingProductOrders") {
          val productIdsInOrder: Set[GamingProductId] = o.gamingProductOrders.keys.map { uri =>
            domain.products.GamingProduct.gamingProductIdFromURI(uri)
          }.toSet
          val allProductIds = ML24GamingProduct.values.map(_.id).toSet
          withClue(s"order.gamingProductOrders difference: ${allProductIds diff productIdsInOrder}") {
            productIdsInOrder shouldEqual allProductIds
          }
        }
        withClue("order.gamingProductOrders.size")(o.gamingProductOrders.size shouldEqual ML24GamingProduct.values.size)

        checkProductOrder[AmlsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(AMLS.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(AmlsBet(numbers = Seq(1, 2, 3, 4, 5, 6)))
          productOrder.participationPools shouldBe AmlsParticipationPools(firstDate = LocalDate.of(2017, 2, 13), drawCount = 1)
        }

        checkProductOrder[AolsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(AOLS.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(AolsBet(numbers = Seq(2, 3, 4, 5, 6, 7, 8)))
          productOrder.participationPools shouldBe AolsParticipationPools(firstDate = LocalDate.of(2017, 2, 7), drawCount = 1)
        }

        checkProductOrder[AplsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(APLS.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(
            AplsBet(numbers = Seq(1, 2, 3, 4, 5, 6), powerball = 2),
            AplsBet(numbers = Seq(2, 3, 4, 5, 6, 7), powerball = 3)
          )
          productOrder.participationPools shouldBe AplsParticipationPools(firstDate = LocalDate.of(2017, 2, 9), drawCount = 1)
        }

        checkProductOrder[AslsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(ASLS.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(
            AslsBet(numbers = Seq(1, 2, 3, 4, 5, 6)),
            AslsBet(numbers = Seq(2, 3, 4, 5, 6, 7))
          )
          productOrder.participationPools shouldBe AslsParticipationPools(firstDate = LocalDate.of(2017, 2, 11), drawCount = 1)
        }

        checkProductOrder[AwlsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(AWLS.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(AwlsBet(numbers = Seq(1, 2, 3, 4, 5, 6)))
          productOrder.participationPools shouldBe AwlsParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawCount = 1)
        }

        checkProductOrder[C4lsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(C4LS.id))) { productOrder =>
          productOrder.variant shouldBe None
          productOrder.bets shouldBe Seq(
            C4lsBet(numbers = Seq(1, 2, 3, 4, 5), cashBall = 1),
            C4lsBet(numbers = Seq(2, 3, 4, 5, 6), cashBall = 2)
          )
          productOrder.participationPools shouldBe C4lsParticipationPools(firstDate = LocalDate.of(2018, 5, 10), drawDays = Set(THURSDAY), drawCount = 1)
        }

        checkProductOrder[EmsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(EMS.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(
            EmsBet(numbers = Seq(1, 2, 3, 4, 5), starnumbers = Seq(1, 11)),
            EmsBet(numbers = Seq(2, 4, 6, 29, 32), starnumbers = Seq(2, 10))
          )
          productOrder.participationPools shouldBe EmsParticipationPools(firstDate = LocalDate.of(2017, 2, 7), drawDays = Set(TUESDAY), drawCount = 1)
        }

        checkProductOrder[EmsPlusGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(EMSPLUS.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(
            EmsPlusBet(numbers = Seq(1, 2, 3, 4, 5)),
            EmsPlusBet(numbers = Seq(2, 4, 6, 29, 32))
          )
          productOrder.participationPools shouldBe EmsPlusParticipationPools(firstDate = LocalDate.of(2017, 2, 7), drawDays = Set(TUESDAY), drawCount = 1)
        }

        checkProductOrder[EjsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(EJS.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(
            EjsBet(numbers = Seq(1, 2, 3, 4, 5), euroNumbers = Seq(1, 8)),
            EjsBet(numbers = Seq(2, 4, 6, 29, 32), euroNumbers = Seq(4, 5))
          )
          productOrder.participationPools shouldBe EjsParticipationPools(firstDate = LocalDate.of(2017, 2, 10), drawCount = 1)
        }

        checkProductOrder[FlsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(FLS.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(
            FlsBet(numbers = Seq(1, 2, 3, 4, 5), chancenumber = 2),
            FlsBet(numbers = Seq(2, 3, 4, 5, 6), chancenumber = 3)
          )
          productOrder.participationPools shouldBe FlsParticipationPools(firstDate = LocalDate.of(2017, 2, 6), drawDays = Set(MONDAY), drawCount = 1)
        }

        checkProductOrder[GlsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(GLS.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(
            GlsBet(numbers = Seq(1, 2, 3, 4, 5, 6), supernumber = 7, system = None),
            GlsBet(numbers = Seq(1, 2, 4, 6, 29, 32, 49), supernumber = 0, system = Some("full"))
          )
          productOrder.participationPools shouldBe GlsParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
        }

        checkProductOrder[GlsSGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(GLSS.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(
            GlsSBet(numbers = Seq(8, 1, 9, 2, 9, 4, 7)),
            GlsSBet(numbers = Seq(2, 5, 0, 7, 0, 8, 6))
          )
          productOrder.participationPools shouldBe GlsSParticipationPools(firstDate = LocalDate.of(2017, 2, 11), drawCount = 1)
        }

        checkProductOrder[IrlsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(IRLS.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(IrlsBet(numbers = Seq(8, 1, 9, 2, 7, 4)))
          productOrder.participationPools shouldBe IrlsParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
        }

        checkProductOrder[IrlsP1GamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(IRLSP1.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(IrlsBet(numbers = Seq(8, 1, 9, 2, 7, 4)))
          productOrder.participationPools shouldBe IrlsP1ParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
        }

        checkProductOrder[IrlsP2GamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(IRLSP2.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(
            IrlsBet(numbers = Seq(8, 1, 9, 2, 7, 4))
          )
          productOrder.participationPools shouldBe IrlsP2ParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
        }

        checkProductOrder[IrishRaffleGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(IRISHRAFFLE.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(
            IrishRaffleBet(numbers = Seq(1, 2, 3, 4)),
            IrishRaffleBet(numbers = Seq(0, 4, 6, 9))
          )
          productOrder.participationPools shouldBe IrishRaffleParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
        }

        checkProductOrder[KenoGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(KENO.id))) { productOrder =>
          productOrder.variant shouldBe None
          productOrder.bets shouldBe Seq(
            KenoBet(numbers = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), stake = 1)
          )
          productOrder.participationPools shouldBe KenoParticipationPools(firstDate = LocalDate.of(2018, 5, 8), drawCount = 1)
        }

        checkProductOrder[MmlsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(MMLS.id))) { productOrder =>
          productOrder.variant shouldBe None
          productOrder.bets shouldBe Seq(
            MmlsBet(numbers = Seq(1, 2, 3, 4, 5), megaBall = 1),
            MmlsBet(numbers = Seq(6, 7, 8, 9, 10), megaBall = 2),
            MmlsBet(numbers = Seq(11, 12, 13, 14, 15), megaBall = 3)
          )
          productOrder.participationPools shouldBe MmlsParticipationPools(firstDate = LocalDate.of(2018, 5, 8), drawDays = Set(TUESDAY), drawCount = 1)
        }

        checkProductOrder[PlsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(PLS.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(PlsBet(numbers = Seq(1, 2, 3, 4, 5, 6)))
          productOrder.participationPools shouldBe PlsParticipationPools(firstDate = LocalDate.of(2017, 2, 7), drawDays = Set(TUESDAY), drawCount = 1)
        }

        checkProductOrder[Plus5GamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(PLUS5.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(
            Plus5Bet(numbers = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9)),
            Plus5Bet(numbers = Seq(2, 3, 4, 5, 6, 7, 8, 9, 10))
          )
          productOrder.participationPools shouldBe Plus5ParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawCount = 1)
        }

        checkProductOrder[S6GamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(S6.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(
            S6Bet(numbers = Seq(0, 8, 1, 8, 3, 6)),
            S6Bet(numbers = Seq(3, 6, 2, 8, 1, 9))
          )
          productOrder.participationPools shouldBe S6ParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
        }

        checkProductOrder[S77GamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(S77.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(
            S77Bet(numbers = Seq(7, 0, 8, 1, 8, 3, 6)),
            S77Bet(numbers = Seq(3, 6, 1, 8, 1, 9, 7))
          )
          productOrder.participationPools shouldBe S77ParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
        }

        checkProductOrder[SlsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(SLS.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(SlsBet(numbers = Seq(1, 2, 3, 4, 5, 6, 7)))
          productOrder.participationPools shouldBe SlsParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
        }

        checkProductOrder[UklsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(UKLS.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(UklsBet(numbers = Seq(8, 1, 9, 2, 7, 4)))
          productOrder.participationPools shouldBe UklsParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
        }

        checkProductOrder[UktblsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(UKTBLS.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(
            UktblsBet(numbers = Seq(1, 2, 3, 4, 5), thunderball = 10),
            UktblsBet(numbers = Seq(6, 7, 8, 9, 10), thunderball = 11)
          )
          productOrder.participationPools shouldBe UktblsParticipationPools(firstDate = LocalDate.of(2017, 2, 8), drawDays = Set(WEDNESDAY), drawCount = 1)
        }

        checkProductOrder[UspblsGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(USPBLS.id))) { productOrder =>
          productOrder.variant shouldBe None
          productOrder.bets shouldBe Seq(UspblsBet(numbers = Seq(1, 2, 3, 4, 5), powerBall = 1), UspblsBet(numbers = Seq(6, 7, 8, 9, 10), powerBall = 2))
          productOrder.participationPools shouldBe UspblsParticipationPools(firstDate = LocalDate.of(2018, 5, 9), drawDays = Set(WEDNESDAY), drawCount = 1)
        }

        checkProductOrder[XmaslGamingProductOrder](o.gamingProductOrders(gamingProductIdToURI(XMASL.id))) { productOrder =>
          productOrder.variant shouldBe Some("variant_1")
          productOrder.bets shouldBe Seq(XmaslBet(numbers = Seq(1, 2, 3, 4, 5)))
          productOrder.participationPools shouldBe XmaslParticipationPools(firstDate = LocalDate.of(2017, 12, 22))
        }
      }
      
      it("parsing of an order should fail with a too short resource path (length < 2)") {
        val orderFile = new File(workingDir, "src/test/resources/orderdocs/order/orderWithAllProducts.json")
        parser.parseOrder(getFileContent(orderFile), Paths.get("order")).failed.get
          .getMessage should include("parseOrder(): provided path must have a length >= 2")        
      }
    }

    it("should accept 'participation-pools' with different 'draw-days' notations") {
      def testWithDrawDay(drawDayStr: String, expectedDrawDay: DayOfWeek): Unit = {
        val orderJsonStr =
          s"""
             |{
             |  "metadata": {
             |    "retailer": {
             |      "href": "http://zoe.mylotto24.co.uk/entities/mylotto24"
             |    },
             |    "origin": "tipp24.com",
             |    "retail-customer": "ff97acdb-d79a-4f8b-b6ae-8ada63d9ea38",
             |    "retailer-order-reference": "8e0c2944-6303-4ed2-8342-2f3bbfe921bd",
             |    "creation-date": "2017-02-05T13:22:47.123Z"
             |  },
             |  "gaming-product-orders": {
             |    "http://zoe.mylotto24.co.uk/gaming-products/mylotto24/gls": {
             |      "bets": [{
             |        "numbers": [1, 2, 3, 4, 5, 6],
             |        "super-number": 7
             |      }],
             |      "participation-pools": {
             |        "first-date": "2017-02-08",
             |        "draw-days": ["$drawDayStr"],
             |        "draw-count": 1
             |      }
             |    }
             |  } 
             |}
          """.stripMargin
        val o: Order = parser.parseOrder(orderJsonStr.getBytes.toIndexedSeq, Paths.get("someorder/order")).get
        o.gamingProductOrders.head._2.participationPools.asInstanceOf[GlsParticipationPools].drawDays shouldBe Set(expectedDrawDay)
      }

      testWithDrawDay("WED", DayOfWeek.WEDNESDAY)
      testWithDrawDay("wed", DayOfWeek.WEDNESDAY)
      testWithDrawDay("WEDNESDAY", DayOfWeek.WEDNESDAY)
      testWithDrawDay("Wednesday", DayOfWeek.WEDNESDAY)
    }    
  }
  
  describe("Parsing of PoolMetadata") {
    
    it("should succeed with valid data") {
      val data = jsonBytes(
        """
          |{
          |  "gaming-product": "http://zoe.mylotto24.co.uk/gaming-products/mylotto24/ejs",
          |  "participation-pool-id": "ejs/2015-12-25",
          |  "participation-pool-digest": {
          |    "base64": "mevsH/v7+OdQwto4gzGD713EfsR3+ZQriDSWHB3mllM=",
          |    "algorithm": "SHA-256"
          |  },
          |  "draw-time": "2015-12-25T18:00:00Z"
          |}
        """.stripMargin)
      inside(parser.parsePoolData(data, Paths.get("filename"))) { case Success(poolMetadata) =>
        poolMetadata.docPath shouldBe Paths.get("filename")
        poolMetadata.gamingProduct shouldBe new URI("http://zoe.mylotto24.co.uk/gaming-products/mylotto24/ejs")
        poolMetadata.drawDate shouldBe ZonedDateTime.of(2015, 12, 25, 18, 0, 0, 0, ZoneId.of("Z"))
        poolMetadata.participationPoolId shouldBe "ejs/2015-12-25"
        poolMetadata.poolDigest shouldBe Some(
          PoolDigest("mevsH/v7+OdQwto4gzGD713EfsR3+ZQriDSWHB3mllM=".getBytes(UTF_8).toIndexedSeq, algorithm = "SHA-256")
        )
        poolMetadata.productId shouldBe "ejs"
      }      
    }
    
    it("should fail for an invalid 'draw-time'") {
      val data = jsonBytes(
        """
          |{
          |  "gaming-product": "http://zoe.mylotto24.co.uk/gaming-products/mylotto24/ejs",
          |  "participation-pool-id": "ejs/2016-01-29",
          |  "participation-pool-digest": {
          |    "base64": "mevsH/v7+OdQwto4gzGD713EfsR3+ZQriDSWHB3mllM=",
          |    "algorithm": "SHA-256"
          |  },
          |  "draw-time": "2016-01-32T18:00:00Z"
          |}
        """.stripMargin)
      inside(parser.parsePoolData(data, Paths.get("filename"))) { case Failure(t) =>
        t.getMessage should include ("Invalid value for DayOfMonth")
      }      
    }    
  }

  describe("Parsing of an order.result") {
    it("parsing of a valid order.result should succeed") {
      val file = new File(workingDir, "src/test/resources/validorders/07ggP6UWr8P_KAXApgtnnoLxu-_LclmsUTq7nvBzWcw/order.result")
      val data = getFileContent(file)
      val o: OrderResult = parser.parseOrderResult(data, Paths.get("order.result")).get
      withClue("docPath")(o.docPath shouldEqual Paths.get("order.result"))
      withClue("rawData")(o.rawData shouldEqual data)
      withClue("creationTime")(o.creationTime.format(OrderResult.dateTimeFormat) shouldEqual "2015-12-13T01:26:35.934Z")
      withClue("orderDigest")(o.orderDigest shouldEqual "sha256=07ggP6UWr8P/KAXApgtnnoLxu+/LclmsUTq7nvBzWcw=".getBytes(UTF_8))
      withClue("orderProcessingResult")(o.orderProcessingResult shouldEqual OrderResult.Accepted)
      withClue("retailerHref")(o.retailerHref shouldEqual "http://www.operator.com/entities/retailer")
      withClue("retailerOrderReference")(o.retailerOrderReference shouldEqual "3581044c-be0d-4f4d-b86e-69c3e7c05cb2")
    }
  }


  describe("Parsing of an order.result.signature") {
    it("parsing of a valid order.result.signature should succeed") {
      val file = new File(workingDir, "src/test/resources/validorders/07ggP6UWr8P_KAXApgtnnoLxu-_LclmsUTq7nvBzWcw/order.result.signature")
      val data = getFileContent(file)
      val o: OrderResultSignature = parser.parseOrderResultSignature(data, Paths.get("order.result.signature")).get
      withClue("algorithm")(o.algorithm shouldEqual "rsa-sha256")
      withClue("keyId")(o.keyId shouldEqual "ZOE_signkey_01")
      withClue("signature") {
        new String(o.signature.toArray, UTF_8) shouldEqual
          "YlT+3bPwdmsTAZgLrr2WlXDAWuwQYzHcR2j5nq/tXuSC0mBDh/Q51+utvLj8Y9QcTOLCWLGUr5hDJySgx55WzZNLyziDu38l+Eqwg4HK6RtTgw0BI47GshQ0acfHcP6iRyPjEUD3GEMP75WAVIiCvj8G3ZcyXk10J4FEecPMZZE="
      }
      withClue("docPath")(o.docPath shouldBe Paths.get("order.result.signature"))
    }
  }

  describe("Parsing of an order.result.signature.timestamp") {
    it("parsing of a order.result.signature.timestamp should succeed") {
      val file = new File(workingDir, "src/test/resources/validorders/07ggP6UWr8P_KAXApgtnnoLxu-_LclmsUTq7nvBzWcw/order.result.signature.timestamp")
      val data = getFileContent(file)
      val o: OrderResultSignatureTimestamp = parser.parseOrderResultSignatureTimestamp(data, Paths.get("order.result.signature.timestamp")).get
      withClue("docPath")(o.docPath shouldEqual Paths.get("order.result.signature.timestamp"))
      withClue("rawData")(o.rawData shouldEqual data)
      withClue("value")(o.value shouldEqual "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")
    }
  }

  describe("Parsing of an order.signature") {
    it("parsing of a order.signature should succeed") {
      val file = new File(workingDir, "src/test/resources/validorders/07ggP6UWr8P_KAXApgtnnoLxu-_LclmsUTq7nvBzWcw/order.signature")
      val data = getFileContent(file)
      val o: OrderSignature = parser.parseOrderSignature(data, Paths.get("order.signature")).get
      withClue("docPath")(o.docPath shouldEqual Paths.get("order.signature"))
      withClue("rawData")(o.rawData shouldEqual data)
      withClue("algorithm")(o.algorithm shouldEqual "rsa-sha256")
      withClue("keyId")(o.keyId shouldEqual "unknown")
      withClue("signature")(o.signature shouldEqual
        "C0E1L2UcS1SfilFTZTr7Ya2Nk8mxC9O2l8XJnXVI4txgdX9cLoitJ0AKenY0nrqoycLt9qmNUPzNk7wGlOZFjnZJRP+UhInba5lbpBnG49/wT+XrWnZd7ImMofxzBKuZRsJW7zX4QW3OIzt6uB65WkLZbW+2Zclfi2AmGYVDqvk="
          .getBytes(StandardCharsets.UTF_8))
    }
  }

  describe("Parsing of an order.metadata") {
    it("parsing of a order.metadata should succeed") {
      val file = new File(workingDir, "src/test/resources/orderdocs/order.metadata/order.metadata")
      val data = getFileContent(file)
      val m: OrderMetadata = parser.parseOrderMetadata(data, Paths.get("order.metadata")).get
      withClue("docPath")(m.docPath shouldEqual Paths.get("order.metadata"))
      val expectedHedgingData = OrderHedgingData(
        Map(
          "ems" -> Seq(DrawHedgingData(poolId = "ems/2016-08-19", LocalDate.of(2016, 8, 19), hedgingChannel = Some("ILS")))
        )
      )
      withClue("hedgingData")(m.hedgingData shouldEqual expectedHedgingData)
    }

    it("parsing of an order.metadata with syntax error should fail") {
      val file = new File(workingDir, "src/test/resources/orderdocs/order.metadata/order.metadata_withSyntaxError")
      val data = getFileContent(file)
      val r = parser.parseOrderMetadata(data, Paths.get("order.metadata_withSyntaxError"))
      r.isFailure shouldEqual true
    }
  }
  
  private def jsonBytes(jsonString: String): Vector[Byte] = jsonString.getBytes(UTF_8).toVector

  private def checkProductOrder[T <: GamingProductOrder : ClassTag](order: GamingProductOrder)(f: T => Unit): Unit = {
    withClue(s"check ${classTag[T].runtimeClass.getSimpleName}:$order") {
      f(order.asInstanceOf[T])
    }
  }

  protected def getFileContent(file: File): IndexedSeq[Byte] = {
    Utils.getInputStream(file.toPath).flatMap(is => Utils.getAsSeq(is, closeStream = true))
      .getOrElse(fail(s"failed to get content of ${file.toPath}"))
  } 
}
