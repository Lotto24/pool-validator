package view

import javafx.application.Platform
import javafx.scene.{Node, Scene}

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}

/**
  */
object JfxUtils {

  /**
   * Convenience method for invoking `javafx.application.Platform.runLater`.
   */
  def runLater(aBlock: => Unit) {
    Platform.runLater(new Runnable() {
      def run() {
        aBlock
      }
    })
  }

  def reloadCss(aScene: Scene) {
    require(aScene != null, "param. aScene is null!")
    val stylesheets = aScene.getStylesheets.toList
    aScene.getStylesheets.setAll()
    aScene.getStylesheets.setAll(stylesheets: _*)
  }

  def setCssClass[T <: Node](node: T, cssClass: String, present: Boolean): Unit = {
    if (!present) {
      var removed = false
      do {
        removed = node.getStyleClass.removeAll(cssClass) //remove ALL occurrences
      } while (removed)
    }
    else if (!node.getStyleClass.contains(cssClass))
      node.getStyleClass.add(cssClass)
  }

  /**
   * Starts a `ScenicView`-instance for the given `Stage`.
   **/
  def startScenicView(aScene: Scene): Try[Unit] = {
    try {
      val scenicViewClass = getClass.getClassLoader.loadClass("org.scenicview.ScenicView")
      val showMethod = scenicViewClass.getDeclaredMethod("show", classOf[javafx.scene.Scene])
      showMethod.invoke(null, aScene)
      Success()
    } catch {
      case e: Exception =>
        Failure(e)
    }
  }

}
