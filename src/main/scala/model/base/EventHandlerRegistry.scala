package model.base

import java.util.UUID

import scala.collection.mutable

/**
  * Simple EventHandlerRegistry (e.g. for distributing error messages).
  */
class EventHandlerRegistry[T] {

  def countListeners: Int = handlers.size

  private val handlers = mutable.Map.empty[UUID, (T) => Unit]

  def addEventHandler(handler: T => Unit): UUID = {
    val id = UUID.randomUUID()
    handlers.update(id, handler)
    id
  }

  def removeEventHandler(id: UUID): Option[(T) => Unit] = handlers.remove(id)

  def publishEvent(event: T): Unit = handlers.values.foreach(_.apply(event))
}
