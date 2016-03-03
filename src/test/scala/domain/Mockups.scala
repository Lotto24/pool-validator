package domain

import java.net.URI
import java.nio.file.Path
import java.time.Instant
import java.util.concurrent.Executor

import domain.PoolValidator.{ArchiveValidationResult, IntermediateResultCallback, OrderValidationResult}
import model.ApplicationSettingsManager.{LoadResult, Loaded}
import model.{ApplicationSettings, ApplicationSettingsManagerAI}
import org.scalatest.Matchers
import util.Task.CancellableSupplierAI
import util.{Task, TaskImpl}

import scala.concurrent.ExecutionContext
import scala.util.{Success, Try}



/**
  * Mockup implementations for tests.
  */
object Mockups {

  /**
    * `PoolValidator` mockup implementation for unit tests.
    */
  class PoolValidatorMockup(var preparedOrderValidationResults: IndexedSeq[OrderValidationResult] = IndexedSeq.empty,
                            implicit val ec: ExecutionContext) extends PoolValidator {
    private var credentialsProvider: CredentialsProvider = null

    override def setCredentialsProvider(provider: CredentialsProvider): this.type = {
      this.credentialsProvider = provider
      this
    }

    override def validateOrder(orderDirPath: Path, drawTime: Try[Instant]): OrderValidationResult = {
      val r = preparedOrderValidationResults.find(x => x.orderLocation == orderDirPath)
      if (r.isEmpty) {
        println(
          s"""not found: results for orderDirPath\n${orderDirPath}
              |preparedResults.keys: \n${preparedOrderValidationResults.mkString("\n")}
         """.stripMargin)
      }
      r.get
    }

    override def validatePool(poolLocation: Path, drawTime: Try[Instant],
                              intermediateResultCallback: IntermediateResultCallback): Task[ArchiveValidationResult] = {

      new TaskImpl[ArchiveValidationResult](info="validatePool", new CancellableSupplierAI[ArchiveValidationResult] {
        override def supply(): ArchiveValidationResult = {
          preparedOrderValidationResults.zipWithIndex.foreach {
            case (orderValidationR, i) => intermediateResultCallback.onOrderValidated(orderValidationR, i + 1)
          }
          val r = ArchiveValidationResult(poolLocation, poolValidationResults = IndexedSeq.empty, preparedOrderValidationResults)
          r
        }
      })
    }
  }


  class ApplicationSettingsManagerMockup(var settings: ApplicationSettings) extends ApplicationSettingsManagerAI {
    override def loadSettingsImpl(configSource: URI): LoadResult = {
      Loaded(settings)
    }

    def saveSettings(settings: ApplicationSettings, target: URI): Try[Boolean] = {
      this.settings = settings
      Success(true)
    }
  }


  /**
    * In the "real" JavaFX-Application all executions that are triggered from non-UI Threads
    * have to be executed via `Platform.runLater(Runnable)`.
    * In order to do unit-tests (without a running JavaFX-Thread) the `ApplicationModel` uses an abstraction
    * about the runLater invocation: `runLaterExecutor: Executor`.
    * `RunLaterExecutor4Tests` is the implementation used for unit-tests.
    * */
  class RunLaterExecutor4Tests extends Executor with Matchers {

    import RunLaterExecutor4Tests.Mode

    private var _mode: Mode.Value = Mode.PutToQueue

    def setMode(value: Mode.Value): Unit = {
      this.synchronized {
        _mode = value
      }
    }

    def mode: Mode.Value = _mode

    def expectNoPendingJobs(): Unit = {
      withClue("PlattformRunLaterMockup.exexutions.size")(exexutions.size shouldBe 0)
    }

    var exexutions = List.empty[Runnable]

    def executeAll(): Unit = {
      while (exexutions.nonEmpty) {
        val toRun = exexutions.head
        exexutions = exexutions.tail
        toRun.run() // ==> this may lead to new items in exexutions (therefore 'while(exexutions.nonEmpty)')
      }
    }

    override def execute(command: Runnable): Unit = {
      this.synchronized {
        //==> needed due to use of parallel collections in the ApplicationModel
        if (mode == Mode.PutToQueue) {
          exexutions = exexutions :+ command
        } else {
          command.run()
        }
      }
    }
  }


  object RunLaterExecutor4Tests {
    object Mode extends Enumeration {
      val RunAtOnce, PutToQueue = Value
    }
  }


}
