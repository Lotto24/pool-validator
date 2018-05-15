package domain

import java.net.URI
import java.nio.file.Paths
import java.time.Instant
import java.util.concurrent.Executor

import domain.PoolValidator._
import model.ApplicationSettingsManager.{LoadResult, Loaded}
import model.{ApplicationSettings, ApplicationSettingsManagerAI}
import monix.eval.Task
import monix.execution.{Scheduler => MonixScheduler}
import org.scalatest.{Assertions, Matchers}

import scala.util.{Success, Try}


/** Mockup implementations for tests. */
object Mockups {

  /** `PoolValidator` mockup implementation for unit tests. */
  class PoolValidatorMockup(var preparedOrderValidationResults: IndexedSeq[OrderValidationResult] = IndexedSeq.empty
  )(implicit val monixScheduler: MonixScheduler) extends PoolValidator with Assertions {

    override def validateOrder(orderId: OrderId, drawTime: Try[Instant]): Task[OrderValidationResult] = Task {
      preparedOrderValidationResults.find(x => x.orderLocation == Paths.get(orderId)).getOrElse(
        fail(s"no prepared results found for orderDirPath\n${Paths.get(orderId)}\n" + s"preparedResults.keys: \n${preparedOrderValidationResults.mkString("\n")}")
      )
    }

    override def validatePool(drawTime: Try[Instant],
      intermediateResultCallback: IntermediateResultCallback): monix.eval.Task[ArchiveValidationResult] = {
      Task {
        preparedOrderValidationResults.zipWithIndex.foreach {
          case (orderValidationR, i) => intermediateResultCallback.onOrderValidated(orderValidationR, i + 1)
        }
        ArchiveValidationResult(Paths.get(""), poolValidationResults = IndexedSeq.empty, preparedOrderValidationResults)
      }
    }

    override def validateOrderDocs(orderDocs: OrderDocs, drawtime: Try[Instant], poolMetadata: Try[PoolMetadata]): OrderValidationResult = {
      preparedOrderValidationResults.find(_.orderId == orderDocs.orderId)
        .getOrElse(fail(s"preparedOrderValidationResults is undefined for ${orderDocs.orderId}"))
    }

    override def validatePoolSeal(
      orderIds: IndexedSeq[String], poolMetadata: Try[PoolMetadata], drawTime: Try[Instant], cbProgress: Option[ProgressIndicator] = None
    ): IndexedSeq[PoolValidator.CheckResult] = ???
  }


  /** Dynamic "happy-path" validator that delivers `CheckOk` for known `PoolArchiveCheck`s.*/
  class HappyPathPoolValidatorMock(allOrderIds: IndexedSeq[String]) extends PoolValidator() {
    override def validateOrder(orderId: OrderId, drawTime: Try[Instant]): Task[OrderValidationResult] = Task.pure(
      OrderValidationResult(Paths.get(orderId), OrderCheck.values.map(check => CheckOk(check)).toVector)
    )

    override def validateOrderDocs(orderDocs: OrderDocs, drawtime: Try[Instant], poolMetadata: Try[PoolMetadata]): OrderValidationResult = {
      OrderValidationResult(Paths.get(orderDocs.orderId), OrderCheck.values.map(check => CheckOk(check)).toVector)
    }

    override def validatePoolSeal(
      orderIds: IndexedSeq[String], poolMetadata: Try[PoolMetadata], drawTime: Try[Instant], cbProgress: Option[ProgressIndicator]
    ): IndexedSeq[PoolValidator.CheckResult] = {
      PoolSealCheck.values.map(check => CheckOk(check))
    }

    override def validatePool(
      drawTime: Try[Instant], intermediateResultCallback: PoolValidator.IntermediateResultCallback
    ): Task[PoolValidator.ArchiveValidationResult] = {
      val orderValidationResults = allOrderIds.zipWithIndex.map { case (orderId, index) =>
        val ovr = OrderValidationResult(Paths.get(orderId), OrderCheck.values.map(check => CheckOk(check)).toVector)
        intermediateResultCallback.onOrderValidated(ovr, index + 1)
        ovr
      }
      Task.pure(
        PoolValidator.ArchiveValidationResult(Paths.get(""),
          poolValidationResults = PoolSealCheck.values.map(check => CheckOk(check)),
          orderValidationResults = orderValidationResults
        )
      )
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
    **/
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
