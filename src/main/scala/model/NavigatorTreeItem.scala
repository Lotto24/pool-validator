package model

import java.util.function.Predicate
import javafx.collections.ObservableList
import javafx.collections.transformation.FilteredList
import javafx.scene.control.TreeItem

import model.NavigatorTreeItem._

/**
  */
object NavigatorTreeItem {

  def genericPredicateOf[T](filterFunction: TreeItem[T] => Boolean): Predicate[TreeItem[T]] = {
    new Predicate[TreeItem[T]]() {
      override def test(item: TreeItem[T]): Boolean = filterFunction(item)
    }
  }

  /** Provides pattern-matching by `NavigatorItem`-type (e.g. `case NavigatorTreeItem(value : ArchiveNavigatorItem)` */
  def unapply[T <: NavigatorItem](treeItem: TreeItem[T])(implicit ct: reflect.ClassTag[T]): Option[T] = {
    if (treeItem == null) None
    else if (ct.unapply(treeItem.getValue).isDefined) {
      Some(treeItem.getValue)
    } else None
  }
}


/**
  * `TreeItemExt[T]` extends `TreeItem[T]` with filter functionality for `getChildren`.
  * */
class TreeItemExt[T](value: T) extends TreeItem[T](value) {
  def this() {
    this(null.asInstanceOf[T])
  }

  private[model] val filteredChildren: FilteredList[TreeItem[T]] = super.getChildren.filtered(genericPredicateOf[T](_ => true))

  private[model] def filteredChildren_source: ObservableList[TreeItemExt[T]] = filteredChildren.getSource.asInstanceOf[ObservableList[TreeItemExt[T]]]

  def setPredicate(predicate: Predicate[_ >: TreeItem[T]]) {
    filteredChildren.setPredicate(predicate)

    //Workaround: the TreeView is not updated after changed Predicate without toggling the 'isExpanded'-state twice..
    setExpanded(!isExpanded)
    setExpanded(!isExpanded)
  }

  override def getChildren: ObservableList[TreeItem[T]] = filteredChildren.asInstanceOf[ObservableList[TreeItem[T]]]
}

