package view

import javafx.event.EventHandler
import javafx.geometry.Side
import javafx.stage.WindowEvent

import com.typesafe.scalalogging.Logger
import controller.ApplicationController
import model.ApplicationModel.{PoolSourceArchive, PoolSourceDirectory}
import model._
import org.apache.commons.lang.StringUtils.isEmpty
import org.slf4j.LoggerFactory
import view.CssClass.Color
import view.impl.StructureElements.{HSpacer, ViewHeader}

import scalafx.Includes._
import scalafx.event.subscriptions.Subscription
import scalafx.scene.control._
import scalafx.scene.image.ImageView
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{HBox, Pane, Priority, VBox}

class NavigatorView extends VBox {
  private val viewHeader = new ViewHeader("Pool navigator")

  private var controller : ApplicationController = null
  private var validateSingleOrderEnabled = false

  private var currentRootNodeChildrenListener = Option.empty[Subscription]

  private val logger = Logger(LoggerFactory.getLogger(this.getClass))

  def init(controller: ApplicationController) : Unit = {
    this.controller = controller

    filterBar.btnTextFilterReset.onMouseClicked = handle {
      filterBar.tefTextFilter.setText("")
    }

    filterBar.tefTextFilter.text.onChange((_, _, newValue) => {
      filterBar.btnTextFilterReset.setVisible(! isEmpty(newValue))
      controller.setNavigatorFilterText(
        if( isEmpty(newValue)) None else Some(newValue)
      )
    })

    treeView.selectionModel.value.selectedItemProperty().onChange((_, anOld, aNew) => {
      val sel = Option(aNew).flatMap(x => Option(x.getValue))
      controller.setSelectedNavigatorItem(sel)
    })

    filterBar.mi_showValidOrders.selected.onChange( (_,_,newVal) => {
      controller.setShowValidOrders(newVal)
    })

    filterBar.mi_showInvalidOrders.selected.onChange( (_,_,newVal) => {
      controller.setShowInvalidOrders(newVal)
    })

    filterBar.mi_showUnvalidatedOrders.selected.onChange( (_,_,newVal) => {
      controller.setShowUnvalidatedOrders(newVal)
    })

    filterBar.cbShowValidOrders.selected.onChange((_, _, newVal) => {
      controller.setShowValidOrders(newVal)
    })

    filterBar.cbShowInvalidOrders.selected.onChange((_, _, newVal) => {
      controller.setShowInvalidOrders(newVal)
    })

    filterBar.cbShowUnvalidatedOrders.selected.onChange{ (_,_,newVal) =>
      controller.setShowUnvalidatedOrders(newVal)
    }
  }


  def setRootItem(root: Option[TreeItemExt[NavigatorItem]]): Unit = {
    logger.info(s"setRootItem($root)")
    treeView.setRoot(root.orNull)
    root match {
      case Some(contentRoot) =>
        setFilterBarVisible(contentRoot.children != null && ! contentRoot.children.isEmpty)
        val l = treeView.getRoot.getChildren.onChange { (listProp, _) =>
          setFilterBarVisible(listProp.nonEmpty || ((treeView.getRoot != null) && (treeView.getRoot.getValue != null)))
        }
        currentRootNodeChildrenListener = Option(l)
      case _ =>
        currentRootNodeChildrenListener.foreach(_.cancel())
        currentRootNodeChildrenListener = None
        setFilterBarVisible(false)
    }
  }

  def setFilterOptions(filterProps: NavigatorFilterOptions): Unit = {
    filterBar.cbShowInvalidOrders.setSelected(filterProps.showInvalidOrders)
    filterBar.mi_showInvalidOrders.setSelected(filterProps.showInvalidOrders)

    filterBar.cbShowValidOrders.setSelected(filterProps.showValidOrders)
    filterBar.mi_showValidOrders.setSelected(filterProps.showValidOrders)

    filterBar.cbShowUnvalidatedOrders.setSelected(filterProps.showUnvalidatedOrders)
    filterBar.mi_showUnvalidatedOrders.setSelected(filterProps.showUnvalidatedOrders)
  }

  private def setFilterBarVisible(visible: Boolean): Unit = {
    filterBar.setVisible(visible)
    filterBar.setManaged(visible)
  }

  def setValidateSingleOrderEnabled(value: Boolean): Unit = {
    validateSingleOrderEnabled = value
  }

  private val filterBar = new HBox() {
    val cbShowValidOrders = new CheckBox("valid")
    val cbShowInvalidOrders = new CheckBox("invalid")
    val cbShowUnvalidatedOrders = new CheckBox("unvalidated")
    val icon_navigatorSettings = new Pane()
    val tefTextFilter = new TextField
    val btnTextFilterReset = new Pane()

    val controlGrpLeft = new HBox()

    val controlGrpRight = new HBox()

    HBox.setHgrow(controlGrpLeft, Priority.Always)
    HBox.setHgrow(controlGrpRight, Priority.Never)


    val contextMenu = new ContextMenu()
    val mi_showValidOrders = new CheckMenuItem("Show valid orders")
    val mi_showInvalidOrders = new CheckMenuItem("Show invalid orders")
    val mi_showUnvalidatedOrders = new CheckMenuItem("Show unvalidated orders")

    locally {

      cbShowValidOrders.setContentDisplay(ContentDisplay.GraphicOnly)
      cbShowValidOrders.setGraphic(svgIcon("svg_valid", CssClass.IconSize.smaller))
      cbShowValidOrders.delegate.setTooltip(Tooltip("Show valid orders"))

      cbShowInvalidOrders.setGraphic(svgIcon("svg_invalid", CssClass.IconSize.smaller))
      cbShowInvalidOrders.setContentDisplay(ContentDisplay.GraphicOnly)
      cbShowInvalidOrders.delegate.setTooltip(Tooltip("Show invalid orders"))

      cbShowUnvalidatedOrders.setGraphic(svgIcon("svg_unvalidated", CssClass.IconSize.smaller))
      cbShowUnvalidatedOrders.setContentDisplay(ContentDisplay.GraphicOnly)
      cbShowUnvalidatedOrders.delegate.setTooltip(Tooltip("Show unvalidated orders"))

      contextMenu.items += mi_showValidOrders
      contextMenu.items += mi_showInvalidOrders
      contextMenu.items += mi_showUnvalidatedOrders

      icon_navigatorSettings.onMouseClicked = (event: MouseEvent) => {
        contextMenu.show(icon_navigatorSettings.delegate, Side.RIGHT, 0 , 15)
      }


      styleClass += "filter-bar"
      tefTextFilter.styleClass += "tef-filter"
      btnTextFilterReset.getStyleClass.addAll("svg_reset","resettextfilter", CssClass.IconSize.smaller)
      btnTextFilterReset.setVisible(false)
      icon_navigatorSettings.styleClass += "navigatorsettings"

      controlGrpLeft.getStyleClass.add("controlsleft")
      controlGrpLeft.getChildren.addAll(cbShowValidOrders, new HSpacer(),
        cbShowInvalidOrders, new HSpacer(),
        cbShowUnvalidatedOrders, new HSpacer(), new HSpacer(),
        tefTextFilter, btnTextFilterReset)

      controlGrpRight.getChildren.addAll(icon_navigatorSettings)
      controlGrpRight.getStyleClass.add("controlsright")

      content.addAll(controlGrpLeft, controlGrpRight)
    }
  }

  private def svgIcon(cssClass: String*): Pane = {
    val p = new Pane
    p.getStyleClass.addAll(cssClass:_*)
    p
  }

  private val treeView = new TreeView[NavigatorItem]()

  locally {
    content.addAll(viewHeader, filterBar, treeView)
    this.getStyleClass.addAll(Color.viewcolors, "navigator-view")
    treeView.showRoot = true
    treeView.prefHeight bind this.height
    filterBar.getStyleClass.addAll(Color.barcolors, "filter-bar")
    treeView.cellFactory = (x) => new NavigatorTreeCell
  }

  class NavigatorTreeCell extends scalafx.scene.control.TreeCell[NavigatorItem]{
    private var _orderDirContextMenu: OrderDirContextMenu = null
    private val iconArchive = new ImageView()
    private val iconPoolDirectory = new ImageView()
    private val iconOrderDir = new ImageView()
    private val iconOrderDoc = new ImageView()

    def treeCellSelf : NavigatorTreeCell = this

    iconArchive.styleClass += "archive"
    iconPoolDirectory.styleClass += "pooldirectory"
    iconOrderDir.styleClass += "orderdir"
    iconOrderDoc.styleClass += "orderdoc"

    item.onChange((_, anOld, aNew) => {
      Option(aNew) match {
        case Some(value) =>
          text = value.displayName
          JfxUtils.setCssClass(this.delegate, cssClass = "invalid", present = (value.isValid == Some(false)))
          JfxUtils.setCssClass(this.delegate, cssClass = "valid", present = (value.isValid == Some(true)))
          value match {
            case item: ArchiveNavigatorItem =>
              graphic = item.poolSource match {
                case src : PoolSourceArchive => iconArchive
                case src : PoolSourceDirectory => iconPoolDirectory
                case _ => null
              }
              contextMenu = null
            case item: OrderDirNavigatorItem =>
              graphic = iconOrderDir
              contextMenu = orderDirContextMenu()
            case item: OrderDocNavigatorItem =>
              graphic = iconOrderDoc
              contextMenu = null
          }
        case _ =>
          text = ""
          graphic = null
          JfxUtils.setCssClass(this.delegate, cssClass = "invalid", present = false)
          JfxUtils.setCssClass(this.delegate, cssClass = "valid", present = false)
      }
    })

    def orderDirContextMenu(): OrderDirContextMenu = {
      if (_orderDirContextMenu != null) _orderDirContextMenu
      else {
        _orderDirContextMenu = new OrderDirContextMenu()
        _orderDirContextMenu
      }
    }

    class OrderDirContextMenu extends ContextMenu{
      val item_validate = new MenuItem("Validate")

      locally {
        items += item_validate

        delegate.setOnShowing(new EventHandler[WindowEvent]() {
          override def handle(event: WindowEvent): Unit = {
            item_validate.setDisable(!validateSingleOrderEnabled)
          }
        })

        item_validate.onAction = handle {
          val x = treeCellSelf.delegate.getTreeItem.getValue
          controller.validateSingleOrder(treeCellSelf.delegate.getTreeItem.getValue)
        }
      }
    }

  }
}
