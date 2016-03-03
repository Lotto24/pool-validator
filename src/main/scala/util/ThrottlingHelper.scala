package util

import scala.concurrent.duration.Duration

/**
  * Little helper class for throttling in single-threaded use cases (e.g. update rates in the UI).
  * */
class ThrottlingHelper[T](private var updateRate: Duration) {
  private var tsLastUpdate = Option.empty[Long]
  private var _lastOmittedUpdate = Option.empty[T]
  private var countSuppressedUpdate = 0
  private var delegateFunction: (T) => Unit = _
  private var throttlingEnabled = true

  def setThrottlingEnabled(enabled: Boolean, deliverOnDisabling: Boolean): Unit = {
    val changed = throttlingEnabled != enabled
    if (changed) {
      throttlingEnabled = enabled
      if (!enabled) {
        if (deliverOnDisabling)
          deliverOmittedUpdateIfSo()
        reset()
      }
    }
  }

  def deliverOmittedUpdateIfSo(): Option[T] = {
    val retVal = _lastOmittedUpdate
    _lastOmittedUpdate.foreach(delegateFunction.apply)
    reset()
    retVal
  }

  def setDelegate(function: (T) => Unit): this.type = {
    delegateFunction = function;
    this
  }

  def lastOmittedUpdate: Option[T] = _lastOmittedUpdate

  def reset(): Unit = {
    tsLastUpdate = None
    _lastOmittedUpdate = None
    countSuppressedUpdate = 0
  }

  def update(value: T): Unit = {
    if (throttlingEnabled) {
      val now = System.currentTimeMillis()
      val doUpdate = if (tsLastUpdate.isEmpty) true else ((now - tsLastUpdate.getOrElse(0L)) > updateRate.toMillis)
      if (doUpdate) {
        delegateFunction(value)
        tsLastUpdate = Some(now)
      } else {
        countSuppressedUpdate += 1
        _lastOmittedUpdate = Some(value)
      }
    } else {
      delegateFunction(value)
    }
  }
}
