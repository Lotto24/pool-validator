package main

import java.io.File
import java.lang.Thread.UncaughtExceptionHandler
import java.nio.file.Path
import java.util.concurrent.Executor

import controller.ApplicationController
import domain._
import javafx.application.Platform
import javafx.scene.image.Image
import model.{ApplicationModel, ApplicationSettingsManagerPropertyImpl, PoolResourceProviderFactoryImpl}
import monix.execution.{Scheduler => MonixScheduler}
import org.slf4j.LoggerFactory
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import util.Utils
import util.Utils.{ErrorMsg, mkFailure}
import view.ApplicationView

import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success, Try}

object Application extends JFXApp {

  //Constants
  val PARAM_configFile = "configFile"
  val configFileName = "settings.properties"
  val defaultConfigFileLocation = Option(System.getProperty("user.dir", null))
  val defaultConfigFile: Option[Path] = defaultConfigFileLocation.map(dir => new File(dir, configFileName).toPath)

  // Model, View, Controller
  var appModel: ApplicationModel = null
  val appView = new ApplicationView
  val appController = new ApplicationController

  implicit val executionContext: ExecutionContextExecutor = scala.concurrent.ExecutionContext.Implicits.global

  implicit val scheduler: MonixScheduler = monix.execution.Scheduler.Implicits.global

  val logger = LoggerFactory.getLogger(getClass)
  val resourceProviderFactory = new PoolResourceProviderFactoryImpl(
    new OrderDocumentsParserPlayImpl(CompositeProductOrderFactory.allProductsOrderFactory),
    orderDocsCacheSize = 10
  )

  val jfxRunLaterExecutor = new Executor() {
    override def execute(command: Runnable): Unit = Platform.runLater(command)
  }

  locally {

    Thread.currentThread().setUncaughtExceptionHandler(new UncaughtExceptionHandler() {
      override def uncaughtException(t: Thread, e: Throwable): Unit = {
        logger.error(e.getMessage, e)
      }
    })

    getConfigFile match {
      case Success(configFile) =>
        launchApplication(configFile)
      case Failure(f) =>
        showErrorMessageAndQuit(ErrorMsg("Config file location is invalid", detail = Some(f.getMessage)))
    }
  }


  private def launchApplication(configFile: File): Unit = {
    logger.info(s"launching application - available processors: ${Runtime.getRuntime().availableProcessors()}")
    
    appModel = new ApplicationModel(
      resourceProviderFactory,
      PoolValidatorFactoryImpl,
      new ApplicationSettingsManagerPropertyImpl,
      new CredentialsManagerImpl,
      configFile = configFile,
      runLaterExecutor = jfxRunLaterExecutor,
      implicitly[MonixScheduler]
    )

    appController.assign(appModel, appView)
    appModel.init()

    stage = new PrimaryStage
    stage.setTitle("Pool validator")
    stage.getIcons.add(new Image("icons/poolvalidator_icon_48px.png"))

    val scene = new Scene(appView)
    scene.getStylesheets.add("application.css")

    stage.scene = scene

    this.stage.onCloseRequest = handle {
      logger.info("Stage is closing")
      appModel.dispose()
    }
  }


  private def getConfigFile: Try[File] = {

    val userConfigFile = parameters.named.get(PARAM_configFile).map(new File(_))

    def tryConfigFile(cfgFile: File, fileType: String): Try[File] = Utils.tryFileReadable(cfgFile) match {
      case suc: Success[_] => suc
      case Failure(t) => mkFailure(s"Error with $fileType '${cfgFile.toString}':\n${t.getMessage}")
    }

    def tryDefaultConfig(): Try[File] = defaultConfigFile.map(_.toFile) match {
      case Some(defaultCfgFile) =>
        tryConfigFile(defaultCfgFile, "Default config file") match {
          case suc: Success[_] =>
            logger.info(s"using default config file $defaultCfgFile")
            suc
          case Failure(t) if userConfigFile.isEmpty =>
            mkFailure(s"Error reading default config file:\n${t.getMessage}.")
          case Failure(t) =>
            mkFailure(s"Default config file is invalid: Expected a file '${configFileName}' in ${defaultConfigFileLocation} or a valid user-" +
              s"defined config file location (programm parameter '--${PARAM_configFile}=<path to file>')."
            )
        }
      case _ => mkFailure(s"Neither a user-defined- nor a default config file ia available! \n" +
        s"Specify a config file location via programm parameter '--${PARAM_configFile}=<path to file>'.")

    }

    userConfigFile match {
      case f@Some(userFile) =>
        tryConfigFile(userFile, "user defined config file") match {
          case suc: Success[_] =>
            logger.info(s"using user defined config file $userFile")
            suc
          case _ => tryDefaultConfig()
        }
      case _ => tryDefaultConfig()
    }
  }

  private def showErrorMessageAndQuit(errorMsg: ErrorMsg*): Unit = {
    appView.showErrorDialog(errorMsg)
    appModel.dispose()
    Platform.exit()
  }
}

