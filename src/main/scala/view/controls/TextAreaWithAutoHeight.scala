package view.controls


import java.util.concurrent.Callable
import javafx.beans.binding.Bindings
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.scene.control.{Skin, TextArea}
import javafx.scene.text.Text

import view.JfxUtils

/**
  * `TextAreaWithAutoHeight` extends `javafx.scene.control.TextArea` with an "auto-height" feature.
  */
class TextAreaWithAutoHeight extends TextArea{

  locally{
    skinProperty().addListener(new ChangeListener[Skin[_]] {
      override def changed(observable: ObservableValue[_ <: Skin[_]], oldValue: Skin[_], newValue: Skin[_]): Unit = {
        Option(newValue).foreach{ newSkin =>
          JfxUtils.runLater {
            installHeightProperryBindings()
          }
        }
      }
    })
  }

  private def installHeightProperryBindings(): Unit = {
    val text = this.lookup(".text").asInstanceOf[Text]
    if(text != null) {

      // Bind the preferred height of the text area to the actual height of the text
      // This will make the text area the height of the text, plus some padding
      // of 20 pixels, as long as that height is between the text area's minHeight
      // and maxHeight. The minHeight we set to 24 pixels, the max height will be
      // the height of its parent (usually).

      val heightBinding = Bindings.createDoubleBinding(new Callable[java.lang.Double]() {
        override def call(): java.lang.Double = {
          text.getBoundsInLocal().getHeight()
        }
      }, text.boundsInLocalProperty()).add(10)

      this.minHeightProperty().bind(heightBinding)
      this.maxHeightProperty().bind(heightBinding)
      this.prefHeightProperty().bind(heightBinding)
    }
  }
}
