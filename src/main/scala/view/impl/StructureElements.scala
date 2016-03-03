package view.impl

import javafx.geometry.Orientation
import javafx.scene.control.Label
import javafx.scene.control.{Label, Separator}
import javafx.scene.layout.{StackPane, HBox, Pane, Priority}

import view.CssClass

import scala.collection.JavaConversions._


/**
  * Collection of some shared helper widgets used to provide a common layout and structure across different
  * view classes.
  */
object StructureElements {

  class HSpacer(val cssClass : String = null) extends Pane{
    getStyleClass.addAll(Seq("h-spacer") ++ Option(cssClass))
  }


  class VariableHSpacer extends HBox{
    def setIndentation(level: Int): Unit = {
      if(level > getChildren.size){
        getChildren.addAll(Seq.fill(level - getChildren.size)(new HSpacer()):_*)
      }else if (level < getChildren.size){
        (1 to (getChildren.size - level)).foreach{_ => getChildren.remove(0) }
      }
    }
  }


  class VSpacer(val cssClass : String = null) extends Pane{
    getStyleClass.addAll(Seq("v-spacer") ++ Option(cssClass))
  }


  /**
    * Header area of a view.
    * */
  class ViewHeader(initialText : String = "") extends HBox{
    private val headerLabel = new Label("")

    locally{
      getStyleClass += "view-header"
      getChildren.addAll(headerLabel)
      setText(initialText)
    }

    def setText(text: String): Unit = {
      headerLabel.setText(text.toUpperCase)
    }
  }


  /**
    * Layout element that combines a `Label` with a horizontal `Separator`.
    * */
  class SectionSeparator(name: String, level: Int = 1, cssClass: String=null) extends HBox {
    val lblName = new Label(name)
    val sep = new Separator(Orientation.HORIZONTAL)

    locally {
      HBox.setHgrow(sep, Priority.ALWAYS)
      getStyleClass.addAll(Option("section-separator") ++ Option(cssClass))
      if(level > 1)
        getStyleClass += CssClass.indendation(level)
      getChildren.setAll(lblName, sep)
    }
  }


  /**
    * `EmptyPanelHint` is displayed in views when there is no other content to display.
    * It may also contain a text to guide the user what to in order to see "real" content in this view.
    * */
  class EmptyPanelHint(text : String="", uppercase: Boolean=true) extends StackPane{
    private val label = new Label()
    locally{
      getStyleClass.add("empty-panel-hint")
      getChildren.add(label)
      setText(text)
    }
    def setText(value: String): Unit = label.setText(if(uppercase)value.toUpperCase else value)
  }

}
