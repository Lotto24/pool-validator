package view

import javafx.beans.property.ReadOnlyObjectWrapper
import javafx.beans.value.ObservableValue
import javafx.scene.control.TableColumn.CellDataFeatures
import javafx.scene.control._
import javafx.util.Callback

object JfxImplicits {

  implicit def toRichTableColumn[R,C](delegate : TableColumn[R,C]): RichTableColumn[R,C] = {
    new RichTableColumn[R,C](delegate)
  }

  implicit def toRichListView[T](delegate : ListView[T]): RichListView[T] = {
    new RichListView(delegate)
  }

  class RichTableColumn[R,C](delegate : TableColumn[R,C]) extends TableColumn[R,C]{

    def installCellFactory(fCellFactory: TableColumn[R, C] => TableCell[R, C]): Unit = {
      delegate.setCellFactory(new Callback[TableColumn[R, C], TableCell[R, C]]() {
        override def call(column: TableColumn[R, C]): TableCell[R, C] = fCellFactory(column)
      })
    }

    def addCellValuePojoSource(extractor: CellDataFeatures[R, C] => C): Unit = {
      delegate.setCellValueFactory(
        new Callback[CellDataFeatures[R, C], ObservableValue[C]]() {
          override def call(aParam: CellDataFeatures[R, C]): ObservableValue[C] = new ReadOnlyObjectWrapper(extractor(aParam))
        }
      )
    }
  }

  class RichListView[T](val delegate : ListView[T]){

    def setCellFactory2(fCellFactory: ListView[T] => ListCell[T]): Unit = {
      delegate.setCellFactory(new Callback[ListView[T], ListCell[T]]() {
        override def call(listView: ListView[T]): ListCell[T] = fCellFactory(listView)
      })
    }
  }

}
