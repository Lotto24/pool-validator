package model

import java.io.File
import java.nio.file.{Path, Paths}
import java.time.{Duration => _, _}

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import domain.Mockups.RunLaterExecutor4Tests.Mode
import domain.Mockups.{ApplicationSettingsManagerMockup, HappyPathPoolValidatorMock, PoolValidatorMockup, RunLaterExecutor4Tests}
import domain.OrderCheck.{OrderAcceptedCheck, PoolParticipationCheck}
import domain.PoolResource.Filenames
import domain.PoolValidator.{CheckFailure, CheckOk, OrderValidationResult}
import domain._
import domain.products.ML24GamingProduct.EJS
import model.ApplicationModel.{ValidationState, _}
import model.ApplicationSettings._
import monix.execution.Scheduler.Implicits.global
import monix.execution.{Scheduler => MonixScheduler}
import org.apache.commons.io.FilenameUtils
import org.scalatest._
import org.scalatest.concurrent.{Eventually, PatienceConfiguration, ScalaFutures}
import org.slf4j.LoggerFactory
import scalafx.Includes._
import util.Utils
import util.Utils.{ErrorMsg, UIValue}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.reflect.{ClassTag, _}


case class PatienceConfigValues(timeout: Duration, interval: Duration)

class ApplicationModelSpec extends FunSpec with Matchers with ScalaFutures with BeforeAndAfterAll with GivenWhenThen with Eventually {

  import ApplicatonModelTestEnv._

  initLogger()
  
  implicit val patienceConfigValues: PatienceConfigValues = PatienceConfigValues(timeout = 3.seconds, interval = 50.millis)
  
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(patienceConfigValues.timeout, patienceConfigValues.interval)
  

  /** Loan fixture method, needed in order to call `ApplicationModel.dispose()` after the test execution so that
    * temporary directories created by the model can be deleted.
    * */
  def withTestSetup(setup: ApplicatonModelTestEnv)(testCode: (ApplicatonModelTestEnv, ApplicationModel) => Any): Unit = {
    testCode(setup, setup.init().model)
    setup.runLaterExecutor.expectNoPendingJobs()
    setup.model.dispose()
  }

  describe("An ApplicationModel") {
    describe("instantiated with valid settings") {
      it("should be in the following initial state") {
        withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
          setup.expectModelStateCleared()
        }
      }

      it("should be in the described state when a valid pool archive is loaded") {
        withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
          setup.loadPoolArchive_blocking(defaultArchiveFile)

          model.validationStateProp.getValue shouldBe ValidationState.NotValidated
          model.validationViewStateProp.getValue shouldBe ValidationViewState.NoItemSelected

          model.poolSourceProp.getValue.isDefined shouldBe true

          eventually {
            withClue("the model's total orders count should match the order-dir count of the loaded pool archive") {
              setup.runLaterExecutor.expectNoPendingJobs()
              val ordersCount = setup.model.resourceProvider.get.getOrdersCount().get
              ordersCount should be > 0
              model.navigatorContentRoot.getChildren.size shouldBe ordersCount
              model.navigatorContentRoot.getChildren.size shouldBe model.getTotalOrdersCount
            }
          }

          model.getArchiveFileSelectorInitialDirectory shouldBe Some(defaultArchiveFile.getParentFile.toPath)
          model.validationStats.invalidOrdersCount shouldBe 0
          model.getSettings shouldBe defaultSettings
          model.orderDocDetailDataProp.getValue shouldBe None
          model.poolSourceProp.getValue shouldBe Some(PoolSourceArchive(defaultArchiveFile.toPath))

          model.validateArchiveEnabledProp.getValue shouldBe true
          model.selectedNavigatorItemProp.getValue shouldBe None
          model.validateSingleOrderEnabledProp.getValue shouldBe false //--> since no order is selected!
          model.backgroundTaskInfoProp.getValue shouldBe None

          model.drawDateInfosProp.value shouldBe Some(
            DrawDateInfos(archiveDrawDate = ZonedDateTime.of(LocalDate.of(2016, 2, 25), LocalTime.of(13, 15, 28, 13000000), ZoneOffset.UTC), customDrawDate = None)
          )

          model.navigatorContentRoot.getValue match {
            case item: ArchiveNavigatorItem =>
              item.poolSource shouldBe PoolSourceArchive(defaultArchiveFile.toPath)
          }

          model.findNavigatorItem {
            case NavigatorTreeItem(item: ArchiveNavigatorItem) => item.poolSource.path == defaultArchiveFile.toPath
          }.map(_.getClass) shouldBe Some(classOf[ArchiveNavigatorItem])
        }
      }

      it("should have single-order-validation enabled a single order is selected") {
        withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
          setup.loadPoolArchive_blocking(defaultArchiveFile)


          val orderId = setup.getNthOrderId(0)
          info(s"orderId:$orderId")
          val orderNavItem = model.findNavigatorItemFor(Paths.get(orderId))
          info(s"orderNavItem:$orderNavItem")
          orderNavItem.map(_.getClass) shouldEqual Some(classOf[OrderDirNavigatorItem])

          model.setSelectedNavigatorItem(orderNavItem)

          model.validationViewStateProp.getValue shouldBe ValidationViewState.ItemNotYetValidated
          model.selectedNavigatorItemProp.getValue shouldBe orderNavItem
          setup.expectDetailDataType(Some(classOf[OrderDirectoryDetailData]))
          model.validateSingleOrderEnabledProp.getValue shouldBe true //--> since a single order is selected!

          model.validationViewItemsProp.getValue.isEmpty shouldBe true
        }
      }

      it("should have single-order-validation disabled when any other or no NavigatorItem is selected") {
        withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
          setup.loadPoolArchive_blocking(defaultArchiveFile)

          val orderId = setup.getNthOrderId(0)

          setup.selectNavigatorItem(Paths.get(orderId, Filenames.Order))
          model.validateSingleOrderEnabledProp.getValue shouldBe false
          model.validationViewStateProp.getValue shouldBe ValidationViewState.ItemNotYetValidated

          setup.selectNavigatorItem(Paths.get(orderId))
          model.validateSingleOrderEnabledProp.getValue shouldBe true

          setup.selectNavigatorItem(Paths.get(orderId, Filenames.OrderResult))
          model.validateSingleOrderEnabledProp.getValue shouldBe false

          setup.selectNavigatorItem(Paths.get(orderId, Filenames.OrderResultSignature))
          model.validateSingleOrderEnabledProp.getValue shouldBe false

          setup.selectNavigatorItem(Paths.get(orderId, Filenames.OrderResultSignatureTimestamp))
          model.validateSingleOrderEnabledProp.getValue shouldBe false

          setup.selectNavigatorItem(Paths.get(orderId, Filenames.OrderResultSignature))
          model.validateSingleOrderEnabledProp.getValue shouldBe false

          setup.selectNavigatorItem(Paths.get(orderId))
          model.validateSingleOrderEnabledProp.getValue shouldBe true
        }
      }

      it("should show have detail-data corresponding to the selected NavigatorItem") {
        withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
          setup.loadPoolArchive_blocking(defaultArchiveFile)
          setup.getNthOrderId(0) should not be setup.getNthOrderId(1)

          for (orderIndex <- 0 to 3) {
            val orderId = setup.getNthOrderId(orderIndex)

            setup.selectNavigatorItem(Paths.get(orderId, Filenames.Order))
            setup.withExpectedDetailData[OrderDocDetailData[_]] { case OrderDocDetailData(doc: Order) =>
              doc.orderId shouldBe orderId
              doc.docPath.toString should endWith(Paths.get(orderId, Filenames.Order).toString)
            }

            setup.selectNavigatorItem(Paths.get(orderId))
            setup.withExpectedDetailData[OrderDirectoryDetailData] { detailData =>
              detailData.orderId shouldBe orderId
            }

            setup.selectNavigatorItem(Paths.get(orderId, Filenames.OrderResult))
            setup.withExpectedDetailData[OrderDocDetailData[_]] { case OrderDocDetailData(doc: OrderResult) =>
              doc.docPath.toString should endWith(Paths.get(orderId, Filenames.OrderResult).toString)
            }

            setup.selectNavigatorItem(Paths.get(orderId, Filenames.OrderResultSignature))
            setup.withExpectedDetailData[OrderDocDetailData[_]] { case OrderDocDetailData(doc: OrderResultSignature) =>
              doc.docPath.toString should endWith(Paths.get(orderId, Filenames.OrderResultSignature).toString)
            }

            setup.selectNavigatorItem(Paths.get(orderId, Filenames.OrderResultSignatureTimestamp))
            setup.withExpectedDetailData[OrderDocDetailData[_]] { case OrderDocDetailData(doc: OrderResultSignatureTimestamp) =>
              doc.docPath.toString should endWith(Paths.get(orderId, Filenames.OrderResultSignatureTimestamp).toString)
            }

            setup.selectNavigatorItem(Paths.get(orderId, Filenames.OrderSignature))
            setup.withExpectedDetailData[OrderDocDetailData[_]] { case OrderDocDetailData(doc: OrderSignature) =>
              doc.docPath.toString should endWith(Paths.get(orderId, Filenames.OrderSignature).toString)
            }

            setup.selectNavigatorItem(Paths.get(orderId))
            setup.expectDetailDataType(Some(classOf[OrderDirectoryDetailData]))
          }
        }
      }
    }

    it("should update the archive-detail-data with betsCount-per-product-infos when the archive is selected") {
      withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
        setup.loadPoolArchive_blocking(defaultArchiveFile)
        setup.selectNavigatorItem(Paths.get(""))
        setup.expectDetailDataType(Some(classOf[ArchiveDetailData]))

        setup.runLaterExecutor.executeAll()

        //all OrderDirNavigatorItems should hold productInfos now:
        val orderDirNavItems = model.navigatorContentRoot.getChildren.map(_.getValue.asInstanceOf[OrderDirNavigatorItem])

        withClue(s"all OrderDirNavigatorItems should hold productInfos now\n${orderDirNavItems.mkString("\n")}") {
          orderDirNavItems.forall(_.productInfos != None) shouldBe true
        }

        setup.withExpectedDetailData[ArchiveDetailData] { archiveDetailData =>
          archiveDetailData.orderStats.totalOrdersCount shouldBe model.getTotalOrdersCount
          archiveDetailData.orderStats.betsCountPerProduct.isDefined shouldBe true
          archiveDetailData.orderStats.betsCountPerProduct.get.get(EJS.id) shouldBe Some(14)
        }
      }
    }

    describe("instantiated with invalid settings (non-existing key-file)") {
      it("should publish an error message") {

        val credSpecs_mod = defaultCredentialsSpecs.copy(certConfigItems = Seq(
          CertificateCfgItem(stableId = "01", name = CredentialsProvider.RootCaCertificate,
            file = UIValue(Some(new File("nonexisting_file"))), description = Some("CA-certificate"))
        ))

        withTestSetup(
          new ApplicatonModelTestEnv().withFailOnErrorMsg(false).withInitModel(false).withMockSettings(
            defaultSettings.copy(
              credentialsSpecs = credSpecs_mod
            )
          )
        ) { (setup, model) =>

          var errorCollector = Seq.empty[ErrorMsg]
          model.addErrorEventHandler { errors =>
            errorCollector = errorCollector ++ errors
          }

          model.init()
          withClue(s"errors: ${errorCollector.mkString("\n")}") {
            errorCollector.size shouldBe 2
            errorCollector.find(_.message == "Error loading loading certificate root-ca-cert").isDefined shouldBe true
          }
        }
      }
    }

    describe("instantiated with invalid settings (.pem file with wrong content)") {
      it("publish an error message") {

        val credSpecs_mod = defaultCredentialsSpecs.copy(certConfigItems = Seq(
          CertificateCfgItem(stableId = "01", name = CredentialsProvider.RootCaCertificate,
            file = UIValue(Some(operatorPubKeyFile)), description = Some("CA-certificate"))
        ))

        withTestSetup(new ApplicatonModelTestEnv().withFailOnErrorMsg(false).withMockSettings(
          defaultSettings.copy(credentialsSpecs = credSpecs_mod
          )).withInitModel(false)) { (setup, model) =>
          var errorsCollector = Seq.empty[ErrorMsg]
          model.addErrorEventHandler { error =>
            errorsCollector = errorsCollector ++ error
          }
          model.init()
          withClue(s"errors: ${errorsCollector.mkString("\n")}") {
            errorsCollector.size shouldBe 2
            errorsCollector.find(_.message == "Error loading loading certificate root-ca-cert").isDefined shouldBe true
          }
        }
      }
    }

    describe("instantiated with invalid settings (non-existing archive-extraction-directory)") {
      it("publish an error message") {

        withTestSetup(
          new ApplicatonModelTestEnv()
            .withFailOnErrorMsg(false).withInitModel(false)
            .withMockSettings(defaultSettings.copy(
              archiveExtractionTarget = UIValue(Some(new File("non-existing-directory")))
            ))
        ) { (setup, model) =>

          var errorsCollector = Seq.empty[ErrorMsg]
          model.addErrorEventHandler { error =>
            errorsCollector = errorsCollector ++ error
          }
          model.init()
          errorsCollector.size shouldBe 1
          errorsCollector.head.message shouldBe "Invalid configuration"
          withClue(s"errors(0).detail: ${errorsCollector.head.detail}") {
            errorsCollector.head.detail.getOrElse("").contains("does not exist") shouldBe true
          }
          model.canOpenArchiveProp.getValue shouldBe false
        }
      }
    }
  }

  describe("An ApplicationModel") {

    describe("when a single valid order is validated") {

      it("the state transition should be as follows") {
        withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
          setup.loadPoolArchive_blocking(defaultArchiveFile)
          model.validationStateProp.getValue shouldBe ValidationState.NotValidated

          val orderId = setup.getNthOrderId(0)

          model.validationViewItemsProp.value.isEmpty shouldBe true
          model.validateSingleOrderEnabledProp.value shouldBe false

          setup.selectNavigatorItem(Paths.get(orderId))

          model.validateSingleOrderEnabledProp.value shouldBe true

          model.findNavigatorItemFor(Paths.get(orderId)) match {
            case Some(navItem: OrderDirNavigatorItem) =>
              navItem.validationResults.isEmpty shouldBe true
              model.validateSingleOrder(navItem)
            case _ => fail("no NavigatorItem found!")
          }

          eventually {
            setup.runLaterExecutor.executeAll()
            model.validationViewStateProp.getValue shouldBe ValidationViewState.ValidationResultsAvailable
          }

          setup.expectNoInvalidOrders()

          model.validationStats.invalidOrdersCount shouldBe 0
          model.validationStats.validatedOrdersCount shouldBe 1
          model.validationStateProp.getValue shouldBe ValidationState.PartiallyValidated

          info("the selected NavigatorItem should be updated with the validation result")
          model.findNavigatorItemFor(Paths.get(orderId)) match {
            case Some(navItem) =>
              withClue(s"${navItem.validationResults.mkString(",")}")(navItem.validationResults.isEmpty shouldBe false)
              navItem.validationResults.forall(_.isOk) shouldBe true
            case _ => fail("no NavigatorItem found!")
          }

          model.validationViewItemsProp.value.isEmpty shouldBe false

          withClue("the archive-navigator-item should not be completely validated") {

            model.findNavigatorItemFor(Paths.get("")) match {
              case Some(archiveItem) =>
                archiveItem.isValid shouldBe None
              case x => fail(s"unexpected: $x")
            }

            // select the archive
            setup.selectNavigatorItem(Paths.get(""))
            model.poolSourceProp
          }
        }
      }

      it("validationState should be CompletelyValidated when all orders are validated after each other") {
        withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
          setup.loadPoolArchive_blocking(defaultArchiveFile)

          var validatedCount = 0
          setup.getAllOrderIds.foreach { orderId =>
            validatedCount += 1
            setup.selectNavigatorItem(Paths.get(orderId))
            model.validateSingleOrderEnabledProp.value shouldBe true

            eventually {
              setup.runLaterExecutor.executeAll()
              model.findNavigatorItemFor(Paths.get(orderId)) match {
                case Some(navItem: OrderDirNavigatorItem) =>
                  model.validateSingleOrder(navItem)
                case _ => fail("no NavigatorItem found!")
              }
              setup.expectNoInvalidOrders()
              model.validationStats.validatedOrdersCount shouldBe validatedCount
              
            }
            setup.runLaterExecutor.executeAll()
          }
          model.validationStateProp.getValue shouldBe ValidationState.CompletelyValidated
        }
      }

      it("..multiple times the getOrder-count methods should deliver consistent values") {
        withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
          setup.loadPoolArchive_blocking(defaultArchiveFile)
          model.getTotalOrdersCount should be > 0
          model.getTotalOrdersCount should be > 0

          setup.expectOrderCounts(totalOrders = 7, validatedOrders = 0, validOrders = 0, invalidOrders = 0)

          val orderId = setup.getNthOrderId(0)
          val selectedItem = setup.selectNavigatorItem(Paths.get(orderId)).get.asInstanceOf[OrderDirNavigatorItem]

          for (i <- 1 to 5) {
            eventually {
              setup.runLaterExecutor.executeAll()
              model.validateSingleOrder(selectedItem)
              setup.expectOrderCounts(totalOrders = 7, validatedOrders = 1, validOrders = 1, invalidOrders = 0)
            }
          }
          setup.runLaterExecutor.executeAll()
        }
      }


      describe("which was timestamped by a fallback tsa") {

        describe("whose certificate is configured in AppSettings.CredentialsSpec.timestamperCertSpecs") {
          it("the timestamp should be valid") {

            val primaryTsaCertSpec = TimestamperCertCfgItem(stableId = "01", priority = 1,
              file = UIValue(Some(timestamperCertFile)), name = "primary TSA cert.", description = None
            )
            val secondaryTsaCertSpec = TimestamperCertCfgItem(stableId = "02", priority = 2,
              file = UIValue(Some(caCertFile)), name = "secondary TSA cert.", description = None
            )

            //INFO we also use a wrong root-ca-cert spec here since certSpecs should no incluence the timestamp-validation
            // (the certificates from CredentialsSpec.timestamperCertSpecs should be used!)

            val credSpecs_mod = defaultCredentialsSpecs.copy(certConfigItems = wrongRootCaCertSpecs,
              timestamperCertConfigItems = Seq(primaryTsaCertSpec, secondaryTsaCertSpec))

            withTestSetup(new ApplicatonModelTestEnv()
              .withMockSettings(defaultSettings.copy(credentialsSpecs = credSpecs_mod))) { (setup, model) =>

              setup.loadPoolArchive_blocking(defaultArchiveFile)
              val orderId = setup.getNthOrderId(0)
              setup.selectNavigatorItem(Paths.get(orderId))
              model.findNavigatorItemFor(Paths.get(orderId)) match {
                case Some(navItem: OrderDirNavigatorItem) =>
                  model.validateSingleOrder(navItem)
                case _ => fail("no NavigatorItem found!")
              }
              eventually {
                setup.runLaterExecutor.executeAll()                
                model.validationViewStateProp.getValue shouldBe ValidationViewState.ValidationResultsAvailable
                setup.expectNoInvalidOrders()
              }
            }
          }
        }
      }
    }
  }

  describe("An ApplicationModel") {
    describe("when a whole valid archive is validated") {
      describe("with no NavigatorItem selected") {
        it("the state transition should be as follows") {
          withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
            setup.loadPoolArchive_blocking(defaultArchiveFile)
            model.validationStats.invalidOrdersCount shouldBe 0
            model.validationStats.validatedOrdersCount shouldBe 0
            model.getTotalOrdersCount shouldBe 7
            model.validationViewItemsProp.value.isEmpty shouldBe true
            model.validationStateProp.getValue shouldBe ValidationState.NotValidated
            setup.validatePool_blocking()
            setup.expectCompletelyValidatedAllValid()
            model.validationViewItemsProp.value.isEmpty shouldBe true
          }
        }
      }

      describe("with the archive selected in the navigator") {
        it("the state transition should be as follows") {
          withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>

            setup.loadPoolArchive_blocking(defaultArchiveFile)

            model.orderDocDetailDataProp.getValue shouldBe None

            model.validateArchiveEnabledProp.value shouldBe true
            model.validateSingleOrderEnabledProp.value shouldBe false

            model.getTotalOrdersCount shouldBe 7
            model.validationStats.invalidOrdersCount shouldBe 0
            model.validationStats.validatedOrdersCount shouldBe 0
            model.validationViewItemsProp.value.isEmpty shouldBe true

            setup.selectNavigatorItem(Paths.get(""))

            setup.expectDetailDataType(Some(classOf[ArchiveDetailData]))

            model.validationStateProp.getValue shouldBe ValidationState.NotValidated

            setup.validatePool_blocking()

            model.validationViewStateProp.getValue shouldBe ValidationViewState.ValidationResultsAvailable

            setup.expectCompletelyValidatedAllValid()

            model.validationViewItemsProp.value.isEmpty shouldBe false
          }
        }

        it("it should behave as follows when an archive is selected, validated and all validation-results are cleared afterwards") {
          withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
            setup.loadPoolArchive_blocking(defaultArchiveFile)

            setup.selectNavigatorItem(Paths.get(""))

            model.selectedNavigatorItemProp.getValue match {
              case Some(item) => item.isValid.isDefined shouldBe false
            }

            setup.validatePool_blocking()

            setup.expectCompletelyValidatedAllValid()
            model.selectedNavigatorItemProp.getValue.isDefined shouldBe true
            model.orderDocDetailDataProp.getValue.get.getClass shouldBe classOf[ArchiveDetailData]
            model.validationViewItemsProp.value.isEmpty shouldBe false

            model.resetValidationResults()

            model.validationViewStateProp.getValue shouldBe ValidationViewState.ItemNotYetValidated

            setup.withExpectedDetailData[ArchiveDetailData] { d =>
              d.validationState shouldBe ValidationState.NotValidated
              d.totalOrdersCount shouldBe 7
              d.validatedOrdersCount shouldBe 0
              d.invalidOrdersCount shouldBe 0
            }

            model.validateArchiveEnabledProp.value shouldBe true

            withClue("all validation-related state-values & flags should be resetted")(setup.expectNoValidationStateInModelData())
          }
        }
      }

      describe("when a user-defined draw-date that is before the order's timestamps is entered after a validation") {
        it("the state transition should be as follows") {
          withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
            setup.loadPoolArchive_blocking(defaultArchiveFile)
            model.validationViewItemsProp.value.isEmpty shouldBe true
            model.validationStateProp.getValue shouldBe ValidationState.NotValidated

            setup.validatePool_blocking()

            setup.expectCompletelyValidatedAllValid()


            model.setCustomDrawDate(Some(
              ZonedDateTime.of(LocalDate.of(2015, 1, 29), LocalTime.of(18, 0, 0), ZoneOffset.UTC)
            ))

            withClue("when an user-defined draw date is entered all validation-state should be reset")(
              setup.expectNoValidationStateInModelData()
            )

            setup.validatePool_blocking()

            model.getTotalOrdersCount shouldBe 7
            model.validationStats.validatedOrdersCount shouldBe 7
            model.validationStats.invalidOrdersCount shouldBe 7
            model.validationStats.validOrdersCount shouldBe 0


            model.setCustomDrawDate(None)

            withClue("when an user-defined draw date is removed all validation-state should be reset")(
              setup.expectNoValidationStateInModelData()
            )

            setup.validatePool_blocking()

            setup.expectCompletelyValidatedAllValid()
          }
        }
      }

      describe("when a user-defined draw-time (!= archive-draw-time) is set ") {

        describe("and thereafter the user-defined draw-time is changed to the same value es the archive-draw-time ") {

          it("appModel.drawInfoProp.customDrawTime should be reset") {
            withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
              setup.loadPoolArchive_blocking(defaultArchiveFile)

              val archiveDrawTime = model.drawDateInfosProp.getValue.get.archiveDrawDate
              val customDrawTime1 = ZonedDateTime.of(LocalDate.of(2015, 1, 29), LocalTime.of(18, 0, 0), ZoneOffset.UTC)

              model.setCustomDrawDate(Some(
                customDrawTime1
              ))

              model.drawDateInfosProp.getValue shouldBe Some(
                DrawDateInfos(archiveDrawTime, customDrawDate = Some(customDrawTime1))
              )

              model.setCustomDrawDate(Some(archiveDrawTime))

              model.drawDateInfosProp.getValue shouldBe Some(DrawDateInfos(archiveDrawTime, customDrawDate = None))
            }
          }
        }
      }
    }
  }

  describe("An ApplicationModel") {
    describe("when the caCertFile does not fit the test-data") {
      describe("and an archive is validated") {

        //INFO ignored since the root-ca-cert is currently not used for validation
        ignore("the model should behave as follows") {

          val credSpecs_mod = defaultCredentialsSpecs.copy(certConfigItems = wrongRootCaCertSpecs)

          withTestSetup(new ApplicatonModelTestEnv()
            .withMockSettings(defaultSettings.copy(credentialsSpecs = credSpecs_mod))) { (setup, model) =>

            setup.loadPoolArchive_blocking(defaultArchiveFile)
            setup.selectNavigatorItem(Paths.get(""))

            setup.validatePool_blocking()

            model.validationViewStateProp.getValue shouldBe ValidationViewState.ValidationResultsAvailable

            setup.withExpectedDetailData[ArchiveDetailData] { d =>
              d.validationState shouldBe ValidationState.CompletelyValidated
              d.validatedOrdersCount shouldBe d.totalOrdersCount
              d.invalidOrdersCount shouldBe d.totalOrdersCount
              d.invalidOrdersCount should be > 0
            }
          }
        }
      }
    }
  }

  describe("An ApplicationModel") {
    describe("when an archive is validated") {
      describe("and a certificate-setting is changed all validation-data should be reset") {
        it("the model should behave as follows") {
          withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>

            setup.loadPoolArchive_blocking(defaultArchiveFile)
            setup.selectNavigatorItem(Paths.get(""))
            setup.validatePool_blocking()

            val credSpecs_mod = defaultCredentialsSpecs.copy(certConfigItems = Seq(
              CertificateCfgItem(stableId = "01", name = CredentialsProvider.RootCaCertificate,
                file = UIValue(Some(timestamperCertFile)), description = Some("CA-certificate"))
            ))


            val settingsWithWrongCaCert = defaultSettings.copy(
              credentialsSpecs = credSpecs_mod
            )

            model.setSettings(settingsWithWrongCaCert)

            setup.expectNoValidationStateInModelData()
          }
        }
      }
    }
  }

  describe("An ApplicationModel") {
    describe("when an archive is loaded") {
      describe("and a navigator-text filter is set and removed again") {
        it("the model should behave as follows") {
          withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
            setup.loadPoolArchive_blocking(defaultArchiveFile)
            setup.selectNavigatorItem(Paths.get(""))

            model.navigatorContentRoot.getChildren.size shouldBe 7

            for (orderIdIndex <- 0 to 3) {
              val orderId = setup.getNthOrderId(orderIdIndex)
              model.setNavigatorFilterText(Some(orderId))
              model.navigatorContentRoot.getChildren.size shouldBe 1
              model.navigatorContentRoot.getChildren.get(0).getValue.displayName shouldBe orderId
              model.setNavigatorFilterText(None)
              model.navigatorContentRoot.getChildren.size shouldBe 7
            }
          }
        }
      }

      describe("when an existing retailer-order-id is entered to navigator's text filter") {
        it("the model should behave as follows") {
          withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
            setup.loadPoolArchive_blocking(defaultArchiveFile)
            setup.selectNavigatorItem(Paths.get(""))
            model.navigatorContentRoot.getChildren.size shouldBe 7

            case class OrderInfo(zoeOrderId: String, retailerOrderId: String)

            val allOrderInfos: IndexedSeq[OrderInfo] = setup.getAllOrderIds.map(setup.model.resourceProvider.get.getOrder(_)).map(o => 
              OrderInfo(zoeOrderId = o.get.orderId, retailerOrderId = o.get.metaData.retailerOrderReference)
            )

            allOrderInfos.size should be > 3

            allOrderInfos.foreach { orderInfo =>
              model.setNavigatorFilterText(Some(orderInfo.retailerOrderId))

              model.navigatorContentRoot.getChildren.size shouldBe 1
              model.navigatorContentRoot.getChildren.get(0).getValue match {
                case dirItem: OrderDirNavigatorItem =>
                  dirItem.retailerOrderReference shouldBe orderInfo.retailerOrderId
                  dirItem.displayName shouldBe orderInfo.zoeOrderId
                  withClue("order.retailerOrderReference in the found order directory") {
                    orderInfo.retailerOrderId shouldEqual setup.model.resourceProvider.get.getOrder(dirItem.displayName).get.metaData.retailerOrderReference
                  }
                case x => fail(s"OrderDirNavigatorItem expected, but obtained $x")
              }
              model.setNavigatorFilterText(None)
              model.navigatorContentRoot.getChildren.size shouldBe 7
            }
          }
        }
      }
    }
  }


  describe("An ApplicationModel") {
    describe("when an archive-file is loaded") {
      describe("and there are is a valid and an invalid order") {
        it("the show-valid & show-invalid filters should behave as follows") {
          val allOrdersPaths = getAllOrderIdsFromPoolArchive(defaultArchiveFile).map(Paths.get(_))
          
          withTestSetup(new ApplicatonModelTestEnv().withValidator(
            new PoolValidatorMockup(
              preparedOrderValidationResults = Vector(
                OrderValidationResult(allOrdersPaths(0), IndexedSeq(CheckOk(OrderAcceptedCheck))),
                OrderValidationResult(allOrdersPaths(1), IndexedSeq(CheckFailure(OrderAcceptedCheck, message = "fake-validation-error"))),
                OrderValidationResult(allOrdersPaths(2), IndexedSeq(CheckOk(OrderAcceptedCheck))),
                OrderValidationResult(allOrdersPaths(3), IndexedSeq(CheckOk(OrderAcceptedCheck))),
                OrderValidationResult(allOrdersPaths(4), IndexedSeq(CheckFailure(OrderAcceptedCheck, message = "fake-validation-error"))),
                OrderValidationResult(allOrdersPaths(5), IndexedSeq(CheckOk(OrderAcceptedCheck))),
                OrderValidationResult(allOrdersPaths(6), IndexedSeq(CheckOk(OrderAcceptedCheck)))
              )
            )
          )) { (setup, model) =>
            setup.loadPoolArchive_blocking(defaultArchiveFile)

            setup.selectNavigatorItem(Paths.get(""))

            model.navigatorContentRoot.getChildren.size shouldBe 7
            model.getVisibleOrdersCount shouldBe 7

            model.setShowInvalidOrders(false)
            model.navigatorContentRoot.getChildren.size shouldBe 7
            model.setShowInvalidOrders(true)
            model.navigatorContentRoot.getChildren.size shouldBe 7

            model.setShowValidOrders(false)
            model.navigatorContentRoot.getChildren.size shouldBe 7
            model.setShowValidOrders(true)
            model.navigatorContentRoot.getChildren.size shouldBe 7


            // simulate a validation

            setup.validatePool_blocking()

            model.navigatorContentRoot.getChildren.size shouldBe 7

            model.validationStats.validOrdersCount shouldBe 5
            model.validationStats.invalidOrdersCount shouldBe 2

            //check filtering

            model.navigatorContentRoot.getChildren.size shouldBe 7

            model.setShowInvalidOrders(false)
            model.navigatorContentRoot.getChildren.size shouldBe 5
            model.getVisibleOrdersCount shouldBe 5
            model.getTotalOrdersCount shouldBe 7
            model.setShowValidOrders(false)
            model.navigatorContentRoot.getChildren.size shouldBe 0

            model.setShowInvalidOrders(true)
            model.navigatorContentRoot.getChildren.size shouldBe 2
            model.getVisibleOrdersCount shouldBe 2
            model.getTotalOrdersCount shouldBe 7


            model.setShowValidOrders(true)
            model.navigatorContentRoot.getChildren.size shouldBe 7


            model.setShowInvalidOrders(false)
            model.setShowValidOrders(false)
            model.navigatorContentRoot.getChildren.size shouldBe 0
            model.getVisibleOrdersCount shouldBe 0
            model.getTotalOrdersCount shouldBe 7


            //reset all validation-results
            model.resetValidationResults()
            model.validationStats.validOrdersCount shouldBe 0
            model.validationStats.invalidOrdersCount shouldBe 0

            model.navigatorContentRoot.getChildren.size shouldBe 7
          }
        }
      }

      describe("and ApplicationModel.unload() is called thereafter the model-state should be completely resetted") {
        it("the show-valid & show-invalid filters should behave as follows") {
          withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
            for (i <- 1 to 4) {
              setup.loadPoolArchive_blocking(defaultArchiveFile)
              model.getTotalOrdersCount should be > 0

              setup.modelUnload_blocking()

              setup.expectModelStateCleared()
            }
          }
        }
      }
    }
  }

  describe("An ApplicationModel with a loaded archive containing some validation errors") {

    it("should apply ValidationStateView-filters depending on the selected NavigatorItem") {
      val allOrdersPaths = getAllOrderIdsFromPoolArchive(defaultArchiveFile).map(Paths.get(_))
      
      withTestSetup(new ApplicatonModelTestEnv().withValidator(
        new PoolValidatorMockup(
          preparedOrderValidationResults = Vector(
            OrderValidationResult(allOrdersPaths(0), IndexedSeq(CheckOk(PoolParticipationCheck))),
            OrderValidationResult(allOrdersPaths(1), IndexedSeq(CheckFailure(PoolParticipationCheck, message = "fake-validation-error"))),
            OrderValidationResult(allOrdersPaths(2), IndexedSeq(CheckOk(PoolParticipationCheck))),
            OrderValidationResult(allOrdersPaths(3), IndexedSeq(CheckOk(PoolParticipationCheck))),
            OrderValidationResult(allOrdersPaths(4), IndexedSeq(CheckOk(PoolParticipationCheck))),
            OrderValidationResult(allOrdersPaths(5), IndexedSeq(CheckFailure(PoolParticipationCheck, message = "fake-validation-error"))),
            OrderValidationResult(allOrdersPaths(6), IndexedSeq(CheckOk(PoolParticipationCheck)))
          )
        )        
      )) { (setup, model) =>
        setup.loadPoolArchive_blocking(defaultArchiveFile)

        Given("A validated archive with some validation-errors")
        And("the ArchiveItem selected in the Pool Navigator")

        setup.selectNavigatorItem(Paths.get("")) shouldBe defined

        setup.validatePool_blocking()

        model.validationViewItemsProp.collect { case i: ValidationViewStateItem => i }.count(_.result.isOk) shouldBe 5
        model.validationViewItemsProp.collect { case i: ValidationViewStateItem => i }.count(!_.result.isOk) shouldBe 2

        When("the ValidationStateView-filter 'hide valid items' is activated")

        model.setValidationViewFilterOptions(ValidationViewFilterOptions(showValidOrders = false, showInvalidOrders = true))

        Then("the journal should not contain any valid items thereafter")

        model.validationViewItemsProp.collect { case i: ValidationViewStateItem => i }.count(_.result.isOk) shouldBe 0
        model.validationViewItemsProp.collect { case i: ValidationViewStateItem => i }.count(!_.result.isOk) shouldBe 2

        When("an OrderDirNavigatorItem is selected thereafter")

        setup.selectNavigatorItem(allOrdersPaths(0))

        setup.expectDetailDataType(Some(classOf[OrderDirectoryDetailData]))

        Then("the ValidationStateView-filter should not be effective anymore")

        model.validationViewItemsProp.collect { case i: ValidationViewStateItem => i }.count(_.result.isOk) shouldBe 1
        model.validationViewItemsProp.collect { case i: ValidationViewStateItem => i }.count(!_.result.isOk) shouldBe 0


        When("the ArchiveNavigatorItem is selected again")

        setup.selectNavigatorItem(Paths.get(""))

        Then("the ValidationStateView-filter should not be effective again")

        model.validationViewItemsProp.collect { case i: ValidationViewStateItem => i }.count(_.result.isOk) shouldBe 0
        model.validationViewItemsProp.collect { case i: ValidationViewStateItem => i }.count(!_.result.isOk) shouldBe 2
        setup.expectDetailDataType(Some(classOf[ArchiveDetailData]))
      }
    }
  }

  describe("An ApplicationModel when loaded a valid archive directory") {
    it("should be in the expected initial state") {
      withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
        setup.loadPoolDirectory_blocking(defaultArchiveDirectory)
        setup.runLaterExecutor.expectNoPendingJobs()
        model.poolSourceProp.getValue shouldBe Some(PoolSourceDirectory(defaultArchiveDirectory.toPath))
        model.navigatorContentRoot.getChildren.size shouldBe 7
        model.validationStateProp.getValue shouldBe ValidationState.NotValidated
      }
    }
    
    describe("with 'validate-on-loading:true'") {
      it("archive should be `CompletelyValidated` at the end") {
        val allOrderIds = getAllOrderIdsFromPoolArchive(defaultArchiveDirectory)
        allOrderIds.size shouldBe 7
        
        val validator = new HappyPathPoolValidatorMock(allOrderIds)
        
        withTestSetup(new ApplicatonModelTestEnv().withValidator(validator).withMockSettings(
          defaultSettings.copy(validatePoolOnLoading = true)          
        )) { (setup, model) =>
          setup.loadPoolDirectory_blocking(defaultArchiveDirectory, expectedValidationState = ValidationState.CompletelyValidated)
          setup.runLaterExecutor.expectNoPendingJobs()
          model.poolSourceProp.getValue shouldBe Some(PoolSourceDirectory(defaultArchiveDirectory.toPath))
          model.navigatorContentRoot.getChildren.size shouldBe 7
          eventually {
            setup.runLaterExecutor.executeAll()
            model.validationStateProp.getValue shouldBe ValidationState.CompletelyValidated
          }
        }
      }
    }

    describe("and ApplicationModel.unload() is called thereafter the model-state should be completely resetted") {
      it("the show-valid & show-invalid filters should behave as follows") {
        withTestSetup(new ApplicatonModelTestEnv()) { (setup, model) =>
          for (i <- 1 to 4) {
            setup.loadPoolDirectory_blocking(defaultArchiveDirectory)
            model.getTotalOrdersCount should be > 0

            setup.modelUnload_blocking()

            setup.expectModelStateCleared()
          }
        }
      }
    }
  }

  protected def initLogger(): Unit = {
    println("initLogger..")
    Option(System.getProperty("logback.configurationFile", null)).foreach { configFile =>
      val context = LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]
      println(s"initLogger..found file: $configFile")
      try {
        val configurator = new JoranConfigurator()
        configurator.setContext(context)
        context.reset()
        configurator.doConfigure(configFile)
      } catch {
        case e: Exception =>
          e.printStackTrace()
      }
    }
  }

  def getAllOrderIdsFromPoolArchive(location: File): Vector[OrderId] = {
    val resourceProvider = location match {
      case dir if location.isDirectory => new PoolResourceProviderDirectoryImpl(dir.toPath, defaultDocsParser)
      case tgzFile if location.isFile && FilenameUtils.getExtension(location.getName) == "tgz" =>
        new PoolResourceProviderTarGzImpl(tgzFile.toPath, defaultDocsParser, 5)
      case _ if location.isFile => fail(s"getAllOrderIdsFromPoolArchive() unsupported file (only .tgz supported): $location")
      case _ => fail(s"getAllOrderIdsFromPoolArchive() - unsupported location: $location")
    }
    whenReady(resourceProvider.getOrderDocsObservable().map(x => x.get.orderId).toListL.runAsync.map(_.toVector))(identity)
  }
  
}


class ApplicatonModelTestEnv(implicit val patienceConfigValues: PatienceConfigValues) extends Matchers with ScalaFutures with Eventually {
  import ApplicatonModelTestEnv.defaultDocsParser
  
  private val logger = LoggerFactory.getLogger(getClass)
  private var _model: ApplicationModel = null
  private var failOnErrorMsg = true
  private var initModel = true
  private implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  private var settingsManager: ApplicationSettingsManager = new ApplicationSettingsManagerMockup(ApplicatonModelTestEnv.defaultSettings)
  private val credentialsManager: CredentialsManager = new CredentialsManagerImpl

  val runLaterExecutor: RunLaterExecutor4Tests = new RunLaterExecutor4Tests
  val resourceProviderFactory: PoolResourceProviderFactoryImpl = new PoolResourceProviderFactoryImpl(
    defaultDocsParser, orderDocsCacheSize = 10)
  var validatorFactory: PoolValidatorFactory = PoolValidatorFactoryImpl

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(patienceConfigValues.timeout, patienceConfigValues.interval)
  
  
  def model: ApplicationModel = _model

  def withValidator(poolValidator: PoolValidator): this.type = {
    validatorFactory = new PoolValidatorFactory(){
      override def getValidator(
        resourceProvider: PoolResourceProvider, credentialsManager: CredentialsManager, scheduler: MonixScheduler
      ): PoolValidator = poolValidator
    }
    this
  }

  def withInitModel(value: Boolean): this.type = {
    initModel = value
    this
  }

  def withFailOnErrorMsg(value: Boolean): this.type = {
    failOnErrorMsg = value
    this
  }

  def withSettingsManager(manager: ApplicationSettingsManager): this.type = {
    settingsManager = manager
    this
  }

  def withMockSettings(settings: ApplicationSettings): this.type = {
    settingsManager = new ApplicationSettingsManagerMockup(settings)
    this
  }

  def init(): this.type = {
    _model = new ApplicationModel(
      resourceProviderFactory,
      validatorFactory,
      settingsManager = settingsManager,
      credentialsManager = credentialsManager,
      configFile = new File("dummyFile"),
      runLaterExecutor = runLaterExecutor,
      implicitly[MonixScheduler]
    )
    if (initModel) {
      model.init()
    }
    if (failOnErrorMsg) {
      model.addErrorEventHandler { errors =>
        fail(
          s"""
             |ApplicationModel sent an error-event:
             |${errors.map(_.message).mkString(" ")}
             |${errors.map(_.detail).mkString("\n")}
           """.stripMargin
        )
      }
    }
    this
  }

  /**
    * Blocking method to load an `archiveFile` via `model.loadPoolArchive(Some(archiveFile))`.
    * Also checks the expected OpStates.
    **/
  def loadPoolArchive_blocking(
    archiveFile: File, 
    expectedValidationState: ValidationState.Value = ValidationState.NotValidated
  ): Unit = {
    loadPoolSource_blocking(PoolSourceArchive(archiveFile.toPath), expectedValidationState)
  }

  /**
    * Blocking method to load an `archiveFile` via `model.loadPoolArchive(Some(archiveFile))`.
    * Also checks the expected OpStates.
    **/
  def loadPoolDirectory_blocking(
    archiveDir: File, expectedValidationState: ValidationState.Value = ValidationState.NotValidated
  ): Unit = {
    loadPoolSource_blocking(PoolSourceDirectory(archiveDir.toPath), expectedValidationState)
  }

  /**
    * Blocking method to load an `archiveFile` via `model.loadPoolArchive(Some(archiveFile))`.
    * Also checks the expected OpStates.
    **/
  def loadPoolSource_blocking(
    poolSource: PoolSource, expectedValidationState: ValidationState.Value = ValidationState.NotValidated
  ): Unit = {

    val modelLoadMethod: (File) => Unit = poolSource match {
      case s: PoolSourceArchive =>
        Utils.isFileUnreadable(poolSource.path.toFile) shouldBe None
        model.loadPoolArchive _
      case s: PoolSourceDirectory =>
        Utils.isFileUnreadable(poolSource.path.toFile, isDirectoryHint = true) shouldBe None
        poolSource.path.toFile.isDirectory() shouldBe true
        model.loadPoolDirectory _
    }

    val appStateChanges = Seq.newBuilder[AppState.Value]

    model.appStateProp.onChange((_, _, newState) => {
      println(s"model.appStateProp..newState = $newState")
      appStateChanges += newState
    })

    val runLaterExecutorMode_old = runLaterExecutor.mode

    runLaterExecutor.setMode(Mode.RunAtOnce) // ==> needed since the promise.success() is called within runLater{}

    modelLoadMethod(poolSource.path.toFile)

    eventually {
      val ordersCount = model.resourceProvider.get.getOrdersCount().get
      withClue("ordersCount:")(ordersCount should be > 0)
    }
    

    eventually {
      runLaterExecutor.executeAll()
      model.appStateProp.getValue shouldBe AppState.Loaded
      model.validationStateProp.getValue shouldBe expectedValidationState
      appStateChanges.result() should contain(AppState.Loading)
      model.poolSourceProp.getValue shouldBe Some(poolSource)
    }
    runLaterExecutor.setMode(runLaterExecutorMode_old)
  }

  /**
    * Blocking method to unload an archiveFile or -directory via `model.unload()`.
    * Also checks the expected OpStates.
    **/
  def modelUnload_blocking(): Unit = {
    whenReady(model.unload(), PatienceConfiguration.Timeout.apply(30 seconds)) { result =>
      logger.debug(s"promise_stateLoaded in whenReady()..result:$result")
      runLaterExecutor.executeAll()
    }
    model.appStateProp.getValue shouldBe AppState.Undefined
  }

  /**
    * Blocking method that calls `model.validateAllOrders()`.
    * Also checks the expected ValidationStates and if validation-progress update values
    * are emitted during the validation.
    **/
  def validatePool_blocking(): Unit = {
    model.validateArchiveEnabledProp.getValue shouldBe true

    val validationProgressList = Seq.newBuilder[Option[TaskInfo]]
    model.backgroundTaskInfoProp.onChange { (_, _, newValue) =>
      validationProgressList += newValue
    }

    logger.info("before validateParticipationPool()")
    model.validateParticipationPool()

    eventually {
      runLaterExecutor.executeAll()
      if (model.getTotalOrdersCount > 0) {
        validationProgressList.result().isEmpty shouldBe false
        validationProgressList.result().exists(_.nonEmpty) // ==> there should be emitted some progress-values during validation
      }
  
      withClue("validatedOrdersCount == totalOrdersCount") {
        model.validationStats.validatedOrdersCount shouldBe model.getTotalOrdersCount
      }
      model.validationStateProp.getValue shouldBe ValidationState.CompletelyValidated
      model.validateArchiveEnabledProp.value shouldBe true
    }
    logger.info("validateParticipationPool() completed")
  }

  def expectOrderCounts(totalOrders: Int = -1, validatedOrders: Int = -1, validOrders: Int = -1, invalidOrders: Int = -1): Unit = {
    if (totalOrders > -1) {
      withClue("#totalOrders")(model.getTotalOrdersCount shouldBe totalOrders)
    }
    if (validatedOrders > -1) {
      withClue("#validatedOrders")(model.validationStats.validatedOrdersCount shouldBe validatedOrders)
    }
    if (validOrders > -1) {
      withClue("#validOrders")(model.validationStats.validOrdersCount shouldBe validOrders)
    }
    if (invalidOrders > -1) {
      withClue("#invalidOrders")(model.validationStats.invalidOrdersCount shouldBe invalidOrders)
    }
  }

  def expectDetailDataType(aType: Option[Class[_ <: DetailData]]): Unit = {
    withClue("model.orderDocDetailDataProp")(model.orderDocDetailDataProp.getValue.map(_.getClass) shouldBe aType)
  }

  def withExpectedDetailData[T <: DetailData : ClassTag](fCallback: T => Unit): Unit = {
    model.orderDocDetailDataProp.getValue.isDefined shouldBe true
    val a = classTag[T].unapply(model.orderDocDetailDataProp.getValue.get) match {
      case Some(detailData) => fCallback(detailData)
      case _ =>
        withClue(s"unexpected model.orderDocDetailDataProp.getValue.get") {
          model.orderDocDetailDataProp.getValue.get.getClass shouldBe classTag[T].runtimeClass
        }
    }
  }

  def expectNoInvalidOrders(): Unit = {

    withClue(
      s"""expectNoInvalidOrders()
         |model.validationStateJournal:
         |${model.validationViewItemsProp.getValue.mkString("\n")}
      """.stripMargin) {

      model.validationStats.invalidOrdersCount shouldBe 0

      withClue("validationStateJournal should not contain any errors") {
        model.validationViewItemsProp.getValue.collect { case i: ValidationViewStateItem => i }.forall(_.result.isOk)
      }

      model.findNavigatorItem(x => x.getValue.isValid == Some(false)) match {
        case Some(item) =>
          fail(
            s"""expectCompletelyValidatedAllValid() found navigator-item with validation-state == Some(false): \n$item
               |item.isValid=${item.isValid}
           """.stripMargin)
        case _ => // do nothing
      }

      model.selectedNavigatorItemProp.getValue match {
        case Some(item) => item.isValid should not be Some(false)
        case _ => // do nothing
      }

      model.orderDocDetailDataProp.getValue match {
        case Some(d: ArchiveDetailData) =>
          d.validatedOrdersCount shouldBe d.totalOrdersCount
          d.invalidOrdersCount shouldBe 0
        case x => // do nothing
      }
    }
  }

  def expectCompletelyValidatedAllValid(): Unit = {

    runLaterExecutor.expectNoPendingJobs()
    
    withClue(
      s"""expectCompletelyValidatedAllValid()
         |model.validationStateJournal:
         |${model.validationViewItemsProp.getValue.mkString("\n")}
      """.stripMargin) {

      model.validationStats.validatedOrdersCount shouldBe model.getTotalOrdersCount
      model.validationStats.invalidOrdersCount shouldBe 0
      model.validationStats.validOrdersCount shouldBe model.getTotalOrdersCount
      model.validationStateProp.getValue shouldBe ValidationState.CompletelyValidated

      model.validateArchiveEnabledProp.value shouldBe true

      withClue("validationStateJournal should not contain any errors") {
        model.validationViewItemsProp.getValue.collect { case i: ValidationViewStateItem => i }.forall(_.result.isOk)
      }

      model.findNavigatorItem(x => x.getValue.isValid != Some(true)) match {
        case Some(item) =>
          fail(
            s"""expectCompletelyValidatedAllValid() found navigator-item with validation-state != Some(true): \n$item
               |item.isValid=${item.isValid}
           """.stripMargin)
        case _ =>

        // do nothing
      }

      model.selectedNavigatorItemProp.getValue match {
        case Some(item) => item.isValid shouldBe Some(true)
        case _ => // do nothing
      }
      
      eventually {
        runLaterExecutor.executeAll()
        model.orderDocDetailDataProp.getValue match {
          case Some(d: ArchiveDetailData) =>
            d.validationState shouldBe ValidationState.CompletelyValidated
            d.validatedOrdersCount shouldBe d.totalOrdersCount
            d.invalidOrdersCount shouldBe 0
            d.validOrdersCount shouldBe d.totalOrdersCount
            d.totalOrdersCount shouldBe model.getTotalOrdersCount
          case x => // do nothing
        }
      }      
    }
  }

  def expectModelStateCleared(): Unit = {

    model.appStateProp.getValue shouldBe AppState.Undefined
    model.validationViewStateProp.getValue shouldBe ValidationViewState.NoArchiveLoaded
    model.canOpenArchiveProp.getValue shouldBe true
    model.validationStats.invalidOrdersCount shouldBe 0
    Option(model.getSettings) should not be None
    model.orderDocDetailDataProp.getValue shouldBe None
    model.poolSourceProp.getValue shouldBe None
    model.selectedNavigatorItemProp.getValue shouldBe None
    model.validateArchiveEnabledProp.getValue shouldBe false
    model.validateSingleOrderEnabledProp.getValue shouldBe false
    model.backgroundTaskInfoProp.getValue shouldBe None

    model.validationStats.validatedOrderIds.size shouldBe 0
    model.getVisibleOrdersCount shouldBe model.getTotalOrdersCount
    model.getTotalOrdersCount shouldBe 0
    model.validationStats.validatedOrdersCount shouldBe 0
    model.validationStats.invalidOrdersCount shouldBe 0
    model.validationStats.validOrdersCount shouldBe 0

    model.canOpenArchiveProp.getValue shouldBe true
    model.drawDateInfosProp.getValue shouldBe None
    model.orderDocDetailDataProp.getValue shouldBe None
    model.poolSourceProp.getValue shouldBe None
    model.selectedNavigatorItemProp.getValue shouldBe None
    model.validateArchiveEnabledProp.getValue shouldBe false
    model.validateSingleOrderEnabledProp.getValue shouldBe false
    model.validationViewStateProp.getValue shouldBe ValidationViewState.NoArchiveLoaded
    model.validationViewItemsProp.getValue.isEmpty shouldBe true
    model.validationStateProp.getValue shouldBe ValidationState.NotValidated
    model.backgroundTaskInfoProp.getValue shouldBe None

    model.navigatorContentRoot.getValue shouldBe null
    model.navigatorContentRoot.getChildren.size shouldBe 0
  }


  def expectNoValidationStateInModelData(): Unit = {
    model.validateArchiveEnabledProp.value shouldBe true
    model.appStateProp.value shouldBe AppState.Loaded

    withClue(s"model.validationStateJournalProp.value should be empty\n${model.validationViewItemsProp.value.mkString("\n")}") {
      model.validationViewItemsProp.value.isEmpty shouldBe true
    }

    model.validationStateProp.value shouldBe ValidationState.NotValidated
    model.validationStats.invalidOrdersCount shouldBe 0
    model.validationStats.validatedOrdersCount shouldBe 0
    model.selectedNavigatorItemProp.getValue.foreach { sel =>
      sel.isValid shouldBe None
      sel.validationResults.isEmpty shouldBe true
    }

    model.orderDocDetailDataProp.getValue match {
      case Some(d: ArchiveDetailData) =>
        d.validationState shouldBe ValidationState.NotValidated
        d.validatedOrdersCount shouldBe 0
        d.invalidOrdersCount shouldBe 0
        d.validOrdersCount shouldBe 0
      case Some(d: OrderDirNavigatorItem) =>
        d.validationState shouldBe ValidationState.NotValidated
        d.hasInvalidOrderDocs shouldBe false
        d.validationResults shouldBe empty
      case _ => //do nothing
    }
  }

  def getNthOrderId(index: Int): String = {
    model.resourceProvider.get.getOrderDocsObservable().toListL.runSyncUnsafe(patienceConfig.timeout).apply(index).map(_.orderId).getOrElse(
      fail(s"no order found for index=$index")
    )
  }

  def getAllOrderIds: IndexedSeq[String] = {
    model.resourceProvider.get.getOrderDocsObservable().toListL.runSyncUnsafe(patienceConfig.timeout).map(_.get.orderId).toVector
  }

  /** Selects the concerned `NavigatorItem` for `file` via `model.setSelectedNavigatorItem(item)` if so and
    * fails if no matching `NavigatorItem` exists.
    * Also checks some other basic `model`-state values associated with the selection-change. */
  def selectNavigatorItem(relativePath: Path): Option[NavigatorItem] = {
    model.findNavigatorItemFor(relativePath) match {
      case item@Some(navItem) =>
        model.setSelectedNavigatorItem(item)
        model.selectedNavigatorItemProp.getValue shouldBe item
        navItem match {
          case ni: ArchiveNavigatorItem =>
            model.orderDocDetailDataProp.getValue.get.getClass shouldBe classOf[ArchiveDetailData]
          case ni: OrderDirNavigatorItem =>
            model.orderDocDetailDataProp.getValue.get.getClass shouldBe classOf[OrderDirectoryDetailData]
          case ni: OrderDocNavigatorItem =>
            model.orderDocDetailDataProp.getValue.get.getClass shouldBe classOf[OrderDocDetailData[_]]
        }
        model.selectedNavigatorItemProp.getValue
      case _ => fail(s"no NavigatorItem found for $relativePath")
    }
  }
}


object ApplicatonModelTestEnv {

  val workingDir = new File(System.getProperty("user.dir"))
  val credentialsDir = new File(workingDir, "src/test/resources/testdata_qa/credentials/")
  val defaultCredentialsManager = new CredentialsManagerImpl

  val caCertFile = new File(credentialsDir, "root-ca-cert.pem")
  val retailerPubKeyFile = new File(credentialsDir, "retailer_publicKey.pem")
  val operatorPubKeyFile = new File(credentialsDir, "operator_publicKey.pem")

  val defaultCredentialsSpecs = CredentialsConfig(
    certConfigItems = Seq(
      CertificateCfgItem(stableId = "01", name = CredentialsProvider.RootCaCertificate,
        file = UIValue(Some(new File(credentialsDir, "root-ca-cert.pem"))), description = Some("CA-certificate"))
    ),
    timestamperCertConfigItems = Seq(
      TimestamperCertCfgItem(stableId = "01", priority = 1, file = UIValue(Some(caCertFile)), name = "primary TSA cert.", description = None)
    ),
    publicKeyConfigItems = Seq(
      PublicKeyCfgItem(stableId = "01", keyId = "ZOE_signkey_01", algorithm = "rsa-sha256",
        file = UIValue(Some(operatorPubKeyFile)), description = Some("operator key")),
      PublicKeyCfgItem(stableId = "02", keyId = "unknown", algorithm = "rsa-sha256",
        file = UIValue(Some(operatorPubKeyFile)), description = Some("unknown key")),
      PublicKeyCfgItem(stableId = "03", keyId = "retailer", algorithm = "rsa-sha256",
        file = UIValue(Some(retailerPubKeyFile)), description = Some("unknown key"))
    )
  )

  //alternative certificates for testing
  val timestamperCertFile = new File(credentialsDir, "mylotto-tsa-cert.pem")
  val wrongRootCaCertFile = new File(credentialsDir, "wrong/retailer-cert.pem")

  /** Seq containing a single `CertificateSpec` which is NOT the correct root-ca-certificate */
  val wrongRootCaCertSpecs = Seq(
    CertificateCfgItem(stableId = "01", name = CredentialsProvider.RootCaCertificate,
      file = UIValue(Some(timestamperCertFile)), description = Some("CA-certificate"))
  )

  val defaultDocsParser: OrderDocumentsParserPlayImpl = new OrderDocumentsParserPlayImpl(
    CompositeProductOrderFactory.allProductsOrderFactory)
  
  val defaultOrderExtractionTarget = UIValue(Some(new File(System.getProperty("java.io.tmpdir"))))

  val defaultSettings: ApplicationSettings = ApplicationSettings(
    defaultCredentialsSpecs,
    archiveExtractionTarget = defaultOrderExtractionTarget,
    validatePoolOnLoading = false,
    showUIDebugControls = false)

  val defaultArchiveFile: File = new File(new File(System.getProperty("user.dir")),
    "src/test/resources/testdata_qa/ejs-2016-02-26_dev_closed.tgz")

  val defaultArchiveDirectory: File = new File(new File(System.getProperty("user.dir")),
    "src/test/resources/testdata_qa/ejs-2016-02-26_dev_closed")
  
}
