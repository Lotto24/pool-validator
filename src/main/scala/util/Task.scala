package util

import java.util.concurrent.CancellationException

import util.Task.{CancellableMappingAI, CancellableSupplier}

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

trait Task[T] extends Awaitable[T] {
  def run(): this.type

  def cancel(): Unit

  def isCancelled: Boolean

  def value: Option[Try[T]]

  def onComplete[U](f: Try[T] => U)(implicit executor: ExecutionContext): Unit

  def map[S](mapping: CancellableMappingAI[T, S])(implicit executor: ExecutionContext): Task[S]
}

object Task {

  trait CancellableSupplier[T] {
    def supply(): T

    def cancel(): Unit

    def isCancelled: Boolean
  }

  abstract class CancellableSupplierAI[T] extends CancellableSupplier[T] {
    private var cancelled = false

    override def isCancelled: Boolean = cancelled

    override def cancel(): Unit = cancelled = true
  }

  class CancellableMappingAI[S, T](f: S => T) {
    var fIsCancelled: () => Boolean = _

    def isCancelled: Boolean = fIsCancelled()

    def map(s: S): T = f.apply(s)
  }

}


class TaskImpl[T](val info: String, callable: CancellableSupplier[T])(implicit ec: ExecutionContext) extends Task[T] {
  private val promise = Promise[T]

  override def run(): this.type = {
    Future {
      complete(Try(callable.supply()))
    }
    this
  }


  private def complete(result: Try[T]): this.type = {
    promise.complete(result)
    this
  }

  override def value: Option[Try[T]] = promise.future.value

  override def isCancelled: Boolean = {
    callable.isCancelled
  }

  override def cancel(): Unit = callable.cancel()

  def onComplete[U](f: Try[T] => U)(implicit executor: ExecutionContext): Unit = {
    promise.future.onComplete(f)(executor)
  }


  /** Creates a new future by applying a function to the successful result of
    * this future. If this future is completed with an exception then the new
    * future will also contain this exception.
    *
    */
  override def map[S](mapping: CancellableMappingAI[T, S])(implicit executor: ExecutionContext): Task[S] = {
    mapping.fIsCancelled = () => this.isCancelled

    val outerTask = this

    val p = new TaskImpl[S](s"${outerTask.info}.mapped", null)(executor) {
      override def run(): this.type = {
        outerTask.run();
        this
      }
    }

    this.onComplete { v =>
      if (isCancelled) {
        p.complete(Failure[S](new CancellationException(s"cancelled: $info")))
      } else {
        v match {
          case Success(t) =>
            p.complete(Try(mapping.map(t)))
          case Failure(t) => p.complete(Failure[S](t))
        }
      }
    }(executor)
    p
  }

  @throws[InterruptedException](classOf[InterruptedException])
  @throws[TimeoutException](classOf[TimeoutException])
  override def ready(atMost: Duration)(implicit permit: CanAwait): TaskImpl.this.type = {
    promise.future.ready(atMost)(permit)
    this
  }

  @throws[Exception](classOf[Exception])
  override def result(atMost: Duration)(implicit permit: CanAwait): T = {
    promise.future.result(atMost)(permit)
  }
}
