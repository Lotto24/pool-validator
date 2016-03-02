package view.impl

import java.io.File
import javafx.stage.FileChooser.ExtensionFilter
import javafx.stage.{DirectoryChooser, FileChooser, Window}

import scalafx.Includes._



/**
  * Helper-methods for File- and DirectoryChoose
  * */
object ChooseViaDialog {

  def chooseFileViaDialog(dialogParent: Window, initialFile: Option[File], title: String,
                          extensionFilters: Seq[ExtensionFilter]): Option[File] = {
    val fileChooser = new FileChooser()
    initialFile.foreach { f =>
      if (f.isDirectory)
        fileChooser.setInitialDirectory(f)
      else
        fileChooser.setInitialDirectory(f.getParentFile)
    }

    fileChooser.setTitle(title)
    fileChooser.extensionFilters.addAll(extensionFilters: _*)
    val selectedFile = fileChooser.showOpenDialog(dialogParent)
    Option(selectedFile)
  }


  def chooseDirectoryViaDialog(dialogParent: Window, initialDir: Option[File], title: String): Option[File] = {
    val dirChooser = new DirectoryChooser()
    initialDir.foreach { f =>
      require(f.isDirectory, s"no directory: $initialDir")
    }
    initialDir.foreach { f =>
      dirChooser.setInitialDirectory(f)
    }
    dirChooser.setTitle(title)
    val selectedFile = dirChooser.showDialog(dialogParent)
    Option(selectedFile)
  }
}
