package view

/**
  * Created by khoff on 04.02.16.
  */
object CssClass {

  def indendation(level: Int): String = s"indent$level"

  def svgIcon : String = "svg-icon"

  object IconSize{
    val smaller : String = "icon_smaller"
    val small : String = "icon_small"
    val large : String = "icon_large"
    val larger : String = "icon_larger"
  }

  object Color{
    val barcolors : String = "barcolors"
    val viewcolors : String = "viewcolors"
  }

}
