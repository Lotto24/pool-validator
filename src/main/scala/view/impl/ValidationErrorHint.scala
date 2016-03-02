package view.impl

import javafx.scene.image.ImageView
import javafx.scene.layout.{HBox, Region}

import view.controls.TextAreaWithAutoHeight

/**
  * Mini-widget for displaying validation errors
  */
class ValidationErrorHint extends HBox {
  private val warningIcon = new ImageView()
  private val message = new TextAreaWithAutoHeight

  getChildren.addAll(warningIcon, message)
  getStyleClass.add("validation-hint")
  setError(None)

  def setError(text: Option[String]): Unit = {
    message.setText(text.getOrElse(""))
    setVisible(text.isDefined)
    setManaged(text.isDefined)
    setVisible(text.isDefined)
    setMaxHeight(if (text.isEmpty) 0 else Region.USE_COMPUTED_SIZE)
  }

  def getError: Option[String] = if (message.getText == null || message.getText.isEmpty) None else Some(message.getText)
}
