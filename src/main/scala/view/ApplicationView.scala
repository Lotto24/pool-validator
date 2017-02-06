package view

import java.nio.file.Path
import java.time.format.{DateTimeFormatter, FormatStyle, TextStyle}
import java.time.{ZoneId, ZoneOffset, ZonedDateTime}
import java.util.Locale
import javafx.geometry.Orientation
import javafx.scene.control.Alert.AlertType
import javafx.scene.control._
import javafx.scene.{Cursor, Node}
import javafx.util.{Callback, StringConverter}

import controller.ApplicationController
import jfxtras.scene.control.LocalDateTimeTextField
import model.ApplicationModel._
import model.{ApplicationSettings, DetailData, NavigatorItem}
import org.slf4j.LoggerFactory
import util.Utils
import util.Utils.ErrorMsg
import view.CssClass.Color
import view.impl.ChooseViaDialog
import view.impl.StructureElements.HSpacer

import scala.collection.JavaConversions._
import scalafx.Includes._
import scalafx.scene.layout.{HBox, Pane, VBox}
import scalafx.stage.FileChooser.ExtensionFilter



class ApplicationView extends VBox with UIUpdateHandler{
  private var controller: ApplicationController = _
  private val logger = LoggerFactory.getLogger(getClass)
  private val menuBar = new ApplicationMenuBar()
  private val toolbar = new ApplicationToolbar
  private val contentPane = new SplitPane
  private val mainAreaContent = new SplitPane
  private val detailView = new DetailView
  val navigator = new NavigatorView()

  val validationStateView = new ValidationView

  locally {

    this.getStyleClass.addAll(Color.viewcolors, "application-view")

    mainAreaContent.setOrientation(Orientation.VERTICAL)
    mainAreaContent.styleClass += "main-area-content"

    mainAreaContent.getItems.addAll(detailView, validationStateView)
    detailView.styleClass += "order-doc-detail-view-holder"

    contentPane.setOrientation(Orientation.HORIZONTAL)
    contentPane.styleClass += "content"
    contentPane.setDividerPosition(0, 0.38)
    contentPane.prefHeightProperty() bind height
    contentPane.getItems.addAll(navigator, mainAreaContent)

    //navigator
    navigator.prefHeight bind contentPane.height

    children.addAll(menuBar, toolbar, contentPane)
  }

  private def initActionHandlers(): Unit = {}

  def init(controller: ApplicationController): Unit = {
    this.controller = controller
    navigator.init(controller)
    toolbar.init(controller)
    menuBar.init(controller)
    validationStateView.init(controller)
    initActionHandlers()
  }

  def setShowUIDebugControls(enabled: Boolean): Unit = {
    toolbar.setShowUIDebugControls(enabled)
  }

  def setArchiveValidationEnabled(newVal: Boolean): Unit = {
    toolbar.setValidateArchiveEnabled(newVal)
    menuBar.menuValidation.item_validateArchive.setDisable(! newVal)
  }

  def setSingleOrderValidationEnabled(newVal: Boolean): Unit = {
    menuBar.menuValidation.item_validateSingleOrder.setDisable(! newVal)
    navigator.setValidateSingleOrderEnabled(newVal)
  }

  def setAppState(state: AppState.Value): Unit = {
    val isWaiting = Seq(AppState.Loading).contains(state)
    navigator.setDisable(isWaiting)
    validationStateView.setAppState(state)
    logger.info(s"setAppState($state)..isWaiting=$isWaiting")
    delegate.setCursor(if (isWaiting) Cursor.WAIT else Cursor.DEFAULT)

    menuBar.setAppState(state)
    toolbar.setAppState(state)

    val canOpen = Seq(AppState.Undefined, AppState.Loaded).contains(state)
    menuBar.menuFile.item_loadArchive.setDisable(! canOpen)
    menuBar.menuFile.item_loadDirectory.setDisable(! canOpen)
  }

  def setValidateArchiveEnabled(value: Boolean): Unit = {
    toolbar.setValidateArchiveEnabled(value)
  }

  def setValidateSingleOrderEnabled(value: Boolean): Unit = {
    toolbar.setValidateSingleOrderEnabled(value)
    navigator.setValidateSingleOrderEnabled(value)
  }

  def setPoolSource(archive: Option[PoolSource]): Unit = {
    detailView.setArchiveLoaded(archive.isDefined)
    menuBar.setPoolArchive(archive)
  }

  def setSelectedNaviagorItem(item: Option[NavigatorItem]): Unit = {
    validationStateView.setSelectedNaviagorItem(item)
  }

  def setDrawDateInfos(value: Option[DrawDateInfos]): Unit = {
    toolbar.setDrawDateInfos(value)
  }

  def setDetailData(data: Option[DetailData]): Unit = {
    detailView.setData(data)
  }

  def showArchiveFileSelector(initialFile: Option[Path]): Unit = {
    ChooseViaDialog.chooseFileViaDialog(this.getScene.getWindow, initialFile = initialFile.map(_.toFile),
      title = "Open Resource File",
      extensionFilters = Seq(new ExtensionFilter("tgz files", "*.tgz"), new ExtensionFilter("tar.gz files", "*.tar.gz"))
    ).foreach(controller.loadPoolArchive)
  }

  def showArchiveDirectorySelector(initialDir: Option[Path]): Unit = {
    ChooseViaDialog.chooseDirectoryViaDialog(this.getScene.getWindow,
      initialDir = initialDir.map(_.toFile), title ="Open participation pool directory"
    ).foreach(controller.loadPoolDirectory)
  }

  def showErrorDialog(error: Seq[ErrorMsg]): Unit = {
    val alert = new Alert(AlertType.ERROR)
    alert.setTitle("Error Dialog")
    alert.setResizable(true)

    val taContentText = new TextArea

    alert.getDialogPane.setContent(taContentText)

    alert.setHeaderText(error.map(x => x.message).mkString(","))
    val detailTxt = error.map(e => e.detail.getOrElse(e.message)).mkString("\n")
    taContentText.setText(detailTxt)
    alert.showAndWait()
  }

  def editSettings(settings: ApplicationSettings, showUIDebugControls: Boolean): Unit = {
    val dialog = new SettingsDialogView(initialSettings = settings)
    dialog.setShowUIDebugControls(showUIDebugControls)
    dialog.initOwner(delegate.getScene.getWindow) // => necessary for SettingsDialogView to use application.css
    val result = dialog.showAndWait()
    if (result.isPresent)
      controller.updateSettings(result.get)
  }


  class ApplicationMenuBar extends MenuBar() {
    val menuFile = new Menu("File") {
      val item_loadArchive = new MenuItem("Load pool archive")
      val item_loadDirectory = new MenuItem("Load pool directory")
      val item_closeArchive = new MenuItem("Close pool archive")
      val item_settings = new MenuItem("Settings")
      val item_exit = new MenuItem("Exit")

      getItems.addAll(item_loadArchive, item_loadDirectory, item_closeArchive, item_settings, new SeparatorMenuItem, item_exit)
    }

    val menuValidation = new Menu("Validation") {
      val item_validateArchive = new MenuItem("Validate archive")
      val item_validateSingleOrder = new MenuItem("Validate single order")
      val item_resetValidationResults = new MenuItem("Reset Validation results")

      getItems.addAll(item_validateArchive, item_validateSingleOrder, new SeparatorMenuItem, item_resetValidationResults)
    }

    locally {
      getMenus.addAll(menuFile, menuValidation)
    }

    def init(controller: ApplicationController): Unit = {
      menuFile.item_loadArchive.onAction = handle {
        controller.requestOpenArchiveSelector()
      }
      menuFile.item_loadDirectory.onAction = handle {
        controller.requestOpenPoolDirectorySelector()
      }
      menuFile.item_closeArchive.onAction = handle {
        controller.closeArchive()
      }
      menuFile.item_settings.onAction = handle {
        controller.requestOpenSettingsView()
      }
      menuFile.item_exit.onAction = handle {
        controller.exit()
      }
      menuValidation.item_validateSingleOrder.onAction = handle {
        controller.validateSelectedOrder()
      }
      menuValidation.item_validateArchive.onAction = handle {
        controller.validateAllOrders()
      }
      menuValidation.item_resetValidationResults.onAction = handle {
        controller.resetValidationResults()
      }
    }

    def setPoolArchive(archive: Option[PoolSource]): Unit = {
      menuFile.item_closeArchive.setDisable(archive.isEmpty)
    }

    def setAppState(value: AppState.Value): Unit = {
      val isDisabled = Seq(AppState.Loading, AppState.Validating).contains(value)
      menuFile.item_loadArchive.setDisable(isDisabled)
      menuFile.item_loadDirectory.setDisable(isDisabled)
    }
  }


  class ApplicationToolbar extends Pane {

    private val btn_OpenArchive = new Pane
    private val lblDrawTime = new Label("Draw-time:")
    private val tefDrawDateTime = new LocalDateTimeTextField()
    private val btnResetCustomDrawDate = new Pane()
    private val dpdZoneId = new ComboBox[ZoneId]()

    private val btn_validate = new Button("Validate Archive")
    private val btn_cancelValidation = new Button("Cancel validation")


    private val zoneIdConverter = new StringConverter[ZoneId](){
      override def fromString(string: String): ZoneId = ZoneId.of(string)
      override def toString(value: ZoneId): String = value.getDisplayName(TextStyle.NARROW, Locale.getDefault)
    }

    private val controlGrpRight = new HBox()
    private val controlGrpLeft = new HBox()

    //debug-controls
    private val btn_reloadCss = new Button("Reload CSS")
    private val btn_startScenicView = new Button("ScenicView")


    private var drawDateInfos = Option.empty[DrawDateInfos]

    locally {

      btn_OpenArchive.getStyleClass.addAll("openarchive", "svg_open-folder", CssClass.svgIcon, CssClass.IconSize.large)

      this.getStyleClass.addAll(Color.barcolors, "application-toolbar")

      lblDrawTime.getStyleClass.add("drawtime")

      dpdZoneId.getStyleClass.add("zoneid")
      dpdZoneId.setEditable(false)
      dpdZoneId.setConverter(zoneIdConverter)

      val zoneIdsSorted = ZoneId.getAvailableZoneIds.toSeq.map(ZoneId.of).sortWith( (a,b) => {
        zoneIdConverter.toString(a) < zoneIdConverter.toString(b)
      })

      dpdZoneId.getItems.setAll(zoneIdsSorted :_*)

      tefDrawDateTime.setDateTimeFormatter(DateTimeFormatter.ofLocalizedDateTime(FormatStyle.LONG, FormatStyle.MEDIUM))
      tefDrawDateTime.setParseErrorCallback(new Callback[Throwable, Void]() {
        override def call(t: Throwable): Void = {
          logger.info(s"invalid date-time: ${t.getMessage}")
          null
        }
      })

      btnResetCustomDrawDate.getStyleClass.addAll("svg_reset", "resetcustomdrawdate", CssClass.IconSize.smaller)
      btnResetCustomDrawDate.setVisible(false)
      btnResetCustomDrawDate.onMouseClicked = handle {
        drawDateInfos.foreach{ddi =>
          controller.setCustomDrawDate(None)
        }
      }

      controlGrpLeft.getStyleClass += "control-grp-left"

      controlGrpLeft.prefHeight.bind(this.heightProperty())
      controlGrpRight.prefHeight.bind(this.heightProperty())

      controlGrpRight.layoutXProperty().bind(this.widthProperty().subtract(controlGrpRight.widthProperty()))
      controlGrpRight.getStyleClass += "control-grp-right"

      //initial values
      btn_validate.disable = true
      btn_validate.getStyleClass.add("validatearchive")

      btn_cancelValidation.getStyleClass.add("cancelvalidation")

      btn_reloadCss.onAction = handle {
        JfxUtils.reloadCss(ApplicationView.this.getScene)
      }

      btn_startScenicView.onAction = handle {
        JfxUtils.startScenicView(ApplicationView.this.getScene).failed.foreach { ex =>
          ex.printStackTrace()
        }
      }

      controlGrpLeft.content.addAll(btn_OpenArchive, new HSpacer(), lblDrawTime, new HSpacer(), tefDrawDateTime, btnResetCustomDrawDate, dpdZoneId)
      controlGrpRight.content.addAll(btn_reloadCss, btn_startScenicView, new HSpacer(), btn_validate, btn_cancelValidation)

      content.addAll(controlGrpLeft, controlGrpRight)
    }

    def init(controller: ApplicationController): Unit = {

      btn_OpenArchive.onMouseClicked = handle {
        controller.requestOpenArchiveSelector()
      }

      btn_validate.onAction = handle {
        controller.validateAllOrders()
      }

      btn_cancelValidation.onAction = handle {
        controller.cancelValidation()
      }

      tefDrawDateTime.localDateTimeProperty().onChange((_, _, newValue) => {
        onCustomDrawDateTimeOrZoneChanged()
      })

      toolbar.dpdZoneId.valueProperty().onChange((_, _, newValue) => {
        onCustomDrawDateTimeOrZoneChanged()
      })
    }

    def setShowUIDebugControls(enabled: Boolean): Unit = {
      btn_reloadCss.setVisible(enabled)
      btn_startScenicView.setVisible(enabled)
    }

    private def setCustomDrawtimeEditable(editable: Boolean): Unit = {
      logger.info(s"setCustomDrawtimeEditable($editable)")

      dpdZoneId.setDisable(! editable)
      tefDrawDateTime.setDisable(! editable)
    }

    private def onCustomDrawDateTimeOrZoneChanged(): Unit = {
      val customDrawDate: Option[ZonedDateTime] = for (
        dateTime <- Option(tefDrawDateTime.getLocalDateTime);
        zoneId <- Option(dpdZoneId.getValue)
      ) yield {
        ZonedDateTime.of(dateTime, zoneId)
      }

      whenModifiedByUserInput {
        controller.setCustomDrawDate(customDrawDate)
      }
    }

    def setDrawDateInfos(value: Option[DrawDateInfos]): Unit = updateUI {
      drawDateInfos = value
      value match {
        case Some(drawDateInfo) =>
          val dateTime = drawDateInfo.customDrawDate.getOrElse(drawDateInfo.archiveDrawDate)
          tefDrawDateTime.setLocalDateTime(dateTime.toLocalDateTime)

          val zoneId = dateTime.getZone match {
            case ZoneOffset.UTC => ZoneId.of("UTC")
            case _ => dateTime.getZone
          }

          dpdZoneId.setValue(zoneId)

          lblDrawTime.setText(
            if(drawDateInfo.customDrawDate.isDefined)
              "Draw-time\n(custom)"
            else
              "Draw-time\n(archive)"
          )

          btnResetCustomDrawDate.setVisible(drawDateInfo.customDrawDate.isDefined)

          JfxUtils.setCssClass(lblDrawTime, "custom", present=drawDateInfo.customDrawDate.isDefined)

        case _ =>
          lblDrawTime.setText("Draw-time")
          tefDrawDateTime.setLocalDateTime(null)
          dpdZoneId.setValue(null)
      }
    }

    def setAppState(state: AppState.Value): Unit = {
      setCustomDrawtimeEditable(
        state match {
          case AppState.Loaded => true
          case _ => false
        }
      )

      val isValidating = AppState.Validating == state
      val isLoaded = Seq(AppState.Loaded, AppState.Validating).contains(state)


      val canOpen = Seq(AppState.Undefined, AppState.Loaded).contains(state)
      btn_OpenArchive.setDisable(! canOpen)

      val loadedAndNotValidating = isLoaded && ! isValidating

      setNodeVisible(btn_validate, loadedAndNotValidating)
      setNodeVisible(btn_cancelValidation, isValidating)
      setNodeVisible(lblDrawTime, isLoaded)
      setNodeVisible(tefDrawDateTime, isLoaded)
      setNodeVisible(dpdZoneId, isLoaded)
    }

    private def setNodeVisible(node: Node, visible: Boolean): Unit = {
      node.setVisible(visible)
      node.setManaged(visible)
    }

    def setValidateArchiveEnabled(value: Boolean): Unit = {
      btn_validate.disable = !value
    }

    def setValidateSingleOrderEnabled(value: Boolean): Unit = {
      navigator.setValidateSingleOrderEnabled(value)
    }
  }
}


