package view

import javafx.collections.ObservableList
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.layout.{HBox, Priority, VBox}

import com.typesafe.scalalogging.Logger
import controller.ApplicationController
import domain.PoolResource
import domain.PoolValidator.{CheckFailure, CheckOk, OrderCheck, PoolCheck}
import model.ApplicationModel._
import model._
import org.slf4j.LoggerFactory
import view.JfxImplicits._
import view.controls.TextAreaWithAutoHeight
import view.impl.StructureElements.{EmptyPanelHint, HSpacer, VariableHSpacer, ViewHeader}

import scalafx.Includes._


class ValidationView extends VBox with UIUpdateHandler {
  private val viewHeader = new ViewHeader("Validation results")
  private val emptyPanelHint = new EmptyPanelHint
  private val filterBar = new FilterBar
  private val listView = new ListView[ValidationViewItem]
  private val logger = Logger(LoggerFactory.getLogger(this.getClass))

  val progressBarGroup = new HBox() {
    val lblInProgress = new Label("Validation in progress:")
    val lblPercent = new Label("")
    val progressBar = new ProgressBar()
    val btnCancel = new Button("Cancel")

    private var currentTaskId = Option.empty[String]

    locally {
      getStyleClass.addAll("progress-bar-group")
      lblInProgress.styleClass += "inprogress"
      lblPercent.styleClass += "percent"
      btnCancel.styleClass += "cancel"
      getChildren.addAll(lblInProgress, new HSpacer, progressBar, new HSpacer, lblPercent, new HSpacer, btnCancel)
    }

    def init(controller : ApplicationController): Unit = {
      btnCancel.onAction = handle{
        currentTaskId.foreach(id => controller.cancelTask(id))
      }
    }

    def setLabelText(text: String): Unit = {
      lblInProgress.setText(text)
    }

    def updateProgressBar(taskInfo: Option[TaskInfo]): Unit = {
      val progress = taskInfo.map(_.progress)
      lblPercent.text = progress.map(x => s"${(x * 100).toInt.toString}%").getOrElse("")
      progressBar.progress = progress.getOrElse(0)
      btnCancel.setVisible(taskInfo.map(_.cancellable).getOrElse(false))
      btnCancel.setManaged(taskInfo.map(_.cancellable).getOrElse(false))
      currentTaskId = taskInfo.map(_.id)
    }
  }

  val content = new VBox{
    getStyleClass.add("content")
    getChildren.addAll(progressBarGroup, filterBar, listView, emptyPanelHint)
  }

  private var selectedNavItem = Option.empty[NavigatorItem]
  private var opState = AppState.Undefined
  private var validationViewState : ValidationViewState.Value = null
  private var _filterOptions : ValidationViewFilterOptions = null
  private var lastTaskInfo = Option.empty[TaskInfo]

  locally {
    updateBackgroundTaskInfo(None)
    getStyleClass.addAll(CssClass.Color.viewcolors, "validation-view")
    prepareListView()
    getChildren.addAll(viewHeader, content)

    VBox.setVgrow(content, Priority.ALWAYS)
    VBox.setVgrow(listView, Priority.ALWAYS)
    VBox.setVgrow(emptyPanelHint, Priority.ALWAYS)
  }


  def init(controller: ApplicationController) = {
    filterBar.cbShowInvalidOrders.selected.onChange( (_,_,newValue) => {
      whenModifiedByUserInput{
        controller.setValidationStateViewFilterOptions(_filterOptions.copy(showInvalidOrders=newValue ))
      }
    })
    filterBar.cbShowValidOrders.selected.onChange( (_,_,newValue) => {
      whenModifiedByUserInput {
        controller.setValidationStateViewFilterOptions(_filterOptions.copy(showValidOrders = newValue))
      }
    })
    progressBarGroup.init(controller)
  }

  private def prepareListView(): Unit = {
    listView.setCellFactory2 { listView => new ValidationStateListCell }
  }

  def updateBackgroundTaskInfo(taskInfo: Option[TaskInfo]): Unit = updateUI {
    if(lastTaskInfo.map(_.id) != taskInfo.map(_.id)){
      progressBarGroup.setLabelText(taskInfo.map(_.id).getOrElse(""))
    }
    progressBarGroup.updateProgressBar(taskInfo)
    progressBarGroup.setManaged(taskInfo.isDefined)
    progressBarGroup.setVisible(taskInfo.isDefined)
    lastTaskInfo = taskInfo
  }

  def setFilterOptions(filterOptions: ValidationViewFilterOptions): Unit = updateUI {
    _filterOptions = filterOptions
    filterBar.cbShowInvalidOrders.setSelected(filterOptions.showInvalidOrders)
    filterBar.cbShowValidOrders.setSelected(filterOptions.showValidOrders)
    updateFilterBarVisible()
  }

  def setValidationViewState(state: ValidationViewState.Value): Unit = updateUI {
    validationViewState = state

    import ValidationViewState._

    val emptyPanelMsg = state match {
      case NoArchiveLoaded => "No\nPool Archive\nloaded"
      case LoadingArchive => "Archive is currently\ngetting loaded"
      case ItemNotYetValidated => "Item has\nnot yet \nbeen validated"
      case ValidationInProgress => "Archive is currently \nbeing validated"
      case PreparingData => "Validation data\nis being prepared.."
      case NoItemSelected => "Select an item in the \n Pool Navigator to show \nits validation state"
      case NoResultsDueToFiltering => "No validation-results visible\ndue to active filters\nin the Pool Navigator"
      case _ => ""
    }

    emptyPanelHint.setText(emptyPanelMsg)

    val isListViewVisible = (state == ValidationResultsAvailable) // && listView.getItems.nonEmpty
    listView.setVisible(isListViewVisible)
    listView.setManaged(isListViewVisible)
    emptyPanelHint.setVisible(! isListViewVisible)
    emptyPanelHint.setManaged(! isListViewVisible)
    updateFilterBarVisible()
  }

  def setItems(items: ObservableList[ValidationViewItem]): Unit = updateUI {
    listView.setItems(items)
  }

  def setSelectedNaviagorItem(item: Option[NavigatorItem]): Unit = updateUI {
    selectedNavItem = item
    updateFilterBarVisible()
  }

  private def updateFilterBarVisible() : Unit = {
    val archiveItemSelected = selectedNavItem match {
      case Some(i : ArchiveNavigatorItem) => true
      case _ => false
    }

    val journalItemAvailable = validationViewState match {
      case ValidationViewState.ValidationResultsAvailable => true
      case ValidationViewState.NoResultsDueToFiltering => true
      case _ => false
    }

    val barVisible = archiveItemSelected && journalItemAvailable

    filterBar.setVisible(barVisible)
    filterBar.setManaged(barVisible)

    logger.debug(
      s"""updateFilterBarVisible()..archiveItemSelected:$archiveItemSelected
         |barVisible:$barVisible
         |validationJournalState:$validationViewState
         |journalItemAvailable:$journalItemAvailable
       """.stripMargin)
  }

  def setAppState(value: AppState.Value) = updateUI {
    opState = value
  }

  class FilterBar extends HBox {

    import scalafx.scene.control.CheckBox

    val cbShowValidOrders = new CheckBox("valid")
    val cbShowInvalidOrders = new CheckBox("invalid")

    locally{
      getStyleClass.addAll(CssClass.Color.barcolors, "filter-bar")
      getChildren.addAll(cbShowValidOrders, cbShowInvalidOrders)
    }
  }
}


class ValidationStateListCell extends ListCell[ValidationViewItem] {

  val cellContent_structuralItem = new HBox {
    private val icon = new ImageView()
    private val lblCheckDescription = new Label
    private val indentation = new VariableHSpacer
    private var resourceCssClass = Option.empty[String]

    locally {
      getStyleClass.addAll("content", "structural")
      getChildren.addAll(indentation, icon, new HSpacer("rightbesideicon"), lblCheckDescription)
    }

    def setItem(item: ValidationViewStructuralItem): Unit = {
      indentation.setIndentation(item.level - 1)
      val resourceName = item.resource match {
        case PoolResource.PRType.OrderDirectory => "Order"
        case r => r.toString
      }
      resourceCssClass.foreach(c => JfxUtils.setCssClass(this, c, false))
      resourceCssClass = Some(item.resource.toString.toLowerCase)
      JfxUtils.setCssClass(this, resourceCssClass.get, true)

      lblCheckDescription.setText(s"${resourceName}: ${item.name}")
    }
  }

  val cellContent_stateItem = new HBox {

    private val icon = new ImageView()
    private val lblCheckDescription = new Label
    private val indentation = new VariableHSpacer

    private val tefErrorMessage = new TextAreaWithAutoHeight()

    val messageArea = new VBox(){
      getStyleClass += "message-area"
      getChildren.addAll(lblCheckDescription, tefErrorMessage)
    }

    locally {
      tefErrorMessage.setEditable(false)
      tefErrorMessage.setWrapText(true)
      getStyleClass.addAll("content", "checkresult")
      getChildren.addAll(indentation, icon, new HSpacer("rightbesideicon"), messageArea)
    }

    def setItem(item: ValidationViewStateItem): Unit = {
      lblCheckDescription.setText(item.result.check.description)

      val (isOrderCheckItem, isPoolCheckItem) =  item.result.check match {
        case c : OrderCheck => (true, false)
        case c : PoolCheck => (false, true)
        case _ => (false, false)
      }
      JfxUtils.setCssClass(this, "order", isOrderCheckItem)
      JfxUtils.setCssClass(this, "pool", isPoolCheckItem)
      JfxUtils.setCssClass(this, "valid", present = item.result.isOk)
      JfxUtils.setCssClass(this, "invalid", present = ! item.result.isOk)

      indentation.setIndentation(item.level - 1)
      item.result match {
        case v : CheckFailure =>
          tefErrorMessage.setText(v.message)
        case v : CheckOk =>
          tefErrorMessage.setText("")
      }
      tefErrorMessage.setVisible(! item.result.isOk)
      tefErrorMessage.setManaged(! item.result.isOk)
    }
  }


  locally {
    getStyleClass.addAll("validation-state-cell")
  }

  override protected def updateItem(item: ValidationViewItem, empty: Boolean): Unit = {
    super.updateItem(item, empty)

    if(empty || item == null) setGraphic(null) else {
      setGraphic(
        item match {
          case stateItem : ValidationViewStateItem =>
            cellContent_stateItem.setItem(stateItem)
            cellContent_stateItem
          case i : ValidationViewStructuralItem =>
            cellContent_structuralItem.setItem(i)
            cellContent_structuralItem
          case x => new Label(s"unexpected item: $x")
        }
      )
    }
  }
}
