package view

import java.io.File
import java.util.Comparator
import javafx.collections.FXCollections
import javafx.collections.transformation.SortedList
import javafx.scene.control._
import javafx.scene.layout._
import javafx.stage.FileChooser.ExtensionFilter
import javafx.util.Callback

import util.Utils
import Utils.UIValue
import model.ApplicationSettings
import model.ApplicationSettings._
import org.slf4j.LoggerFactory
import util.Utils
import view.CssClass.Color
import view.JfxImplicits._
import view.PublicKeyEditor.PublicKeyListItem
import view.impl.StructureElements.{SectionSeparator, VSpacer}
import view.impl.{ChooseViaDialog, ValidationErrorHint}
import Utils.stringToOption

import scalafx.Includes._


/**
  */
class SettingsDialogView(private var initialSettings: ApplicationSettings) extends Dialog[ApplicationSettings] with UIUpdateHandler {

  private val lblOperatorCert = new Label("Operator Certificate")

  private val tefCaCertFile = new TextField()
  private val validationHint_caCertFile = new ValidationErrorHint

  private val tefArchiveExtractionDir = new TextField()
  private val validationHint_tefArchiveExtractionDir = new ValidationErrorHint

  private val cbStartValidationAfterLoading = new CheckBox()
  
  private val btnChooseFile_caCert = new Button("File")
  private val btnChooseFile_archiveExtractionDir = new Button("File")

  private val btn_startScenicView = new Button("ScenicView")
  private val btn_reloadCss = new Button("Reload CSS")


  private val publicKeyEditor = new PublicKeyEditor
  private val timestamperCertificateEditor = new TimestamperCertificateEditor

  locally {

    Seq(tefCaCertFile,
      tefArchiveExtractionDir
    ).foreach { textInput =>
      textInput.getStyleClass.add("value")
    }

    Seq(btnChooseFile_caCert,
      btnChooseFile_archiveExtractionDir
    ).foreach(_.getStyleClass.add("file"))


    setTitle("Settings")
    setHeaderText("Settings")

    // Set the button types.
    getDialogPane().getButtonTypes().addAll(ButtonType.OK, ButtonType.CANCEL);
    getDialogPane.getStyleClass.addAll(Color.viewcolors, "settings-dialog-view")


    val gridCerts = new GridPane()
    gridCerts.getStyleClass.add("certificates")

    val gridGlobal = new GridPane()
    gridGlobal.getStyleClass.add("global")

    tefCaCertFile.styleClass += "cacertfile"

    tefCaCertFile.text.onChange((_, _, newValue) => {
      tefCaCertFile.tooltip = new Tooltip(newValue)
    })

    tefArchiveExtractionDir.styleClass += "orderextractiondir"

    // populate gridGlobal
    {
      var rowInd = 0

      gridGlobal.add(new Label("Archive extraction target:"), 0, rowInd)
      gridGlobal.add(tefArchiveExtractionDir, 1, rowInd)
      gridGlobal.add(btnChooseFile_archiveExtractionDir, 2, rowInd)

      rowInd += 1

      gridGlobal.add(validationHint_tefArchiveExtractionDir, 1, rowInd)

      rowInd += 1

      gridGlobal.add(new Label("Start validation after loading:"), 0, rowInd)
      gridGlobal.add(cbStartValidationAfterLoading, 1, rowInd)
    }

    // populate gridCerts
    {
      var rowInd = 0

      gridCerts.add(new Label("CA-Certificate:"), 0, rowInd)
      gridCerts.add(tefCaCertFile, 1, rowInd)
      gridCerts.add(btnChooseFile_caCert, 2, rowInd)

      rowInd += 1

      gridCerts.add(validationHint_caCertFile, 1, rowInd)
    }

    btn_startScenicView.onAction = handle {
      JfxUtils.startScenicView(this.getDialogPane.getScene)
    }

    btn_reloadCss.onAction = handle {
      JfxUtils.reloadCss(this.getDialogPane.getScene)
    }

    publicKeyEditor.setOnChanged { (newSpecs) =>
      val updateCredSpecs = initialSettings.credentialsSpecs.copy(publicKeyConfigItems=newSpecs)
      this.initialSettings = initialSettings.copy(credentialsSpecs = updateCredSpecs)
      updateOkEnabled()
    }

    timestamperCertificateEditor.setOnChanged{(newSpecs) =>
      val updatedCredSpecs = initialSettings.credentialsSpecs.copy(timestamperCertConfigItems = newSpecs)
      this.initialSettings = initialSettings.copy(credentialsSpecs = updatedCredSpecs)
      updateOkEnabled()
    }

    val dialogContent = new VBox
    dialogContent.content.setAll(btn_startScenicView, btn_reloadCss,
      new SectionSeparator("Global"),
      gridGlobal,
      new VSpacer(),
      new SectionSeparator("Root CA certificate"),
      gridCerts,
      new VSpacer(),
      new SectionSeparator("Timestamper certificates"),
      timestamperCertificateEditor,
      new VSpacer(),
      new SectionSeparator("Public keys"),
      publicKeyEditor
    )

    getDialogPane().setContent(dialogContent)
    initValues(initialSettings)
    initEventHandlers()


    setResultConverter(new Callback[ButtonType, ApplicationSettings]() {
      override def call(buttonType: ButtonType): ApplicationSettings = {
        if (buttonType != ButtonType.OK) null
        else {
          val caCertFile = UIValue(stringToOption(tefCaCertFile.getText).map(new File(_)))

          val credentialSpecs_new = initialSettings.credentialsSpecs.copy(certConfigItems = Seq(
            CertificateCfgItem(stableId = "01", name = "root-ca-cert", file = caCertFile, description = Some("root ca certificate"))
          ))

          val archiveExtractionTarget = UIValue(Option(tefArchiveExtractionDir.getText).map(new File(_)))
          val r = initialSettings.copy(
            credentialsSpecs = credentialSpecs_new,
            archiveExtractionTarget = archiveExtractionTarget
          )
          r
        }
      }
    })
  }

  private def initValues(settings: ApplicationSettings): Unit = {
    require(settings != null)

    tefArchiveExtractionDir.setText(settings.archiveExtractionTarget.value.map(_.toString).orNull)
    validationHint_tefArchiveExtractionDir.setError(settings.archiveExtractionTarget.error.map(_.message))

    cbStartValidationAfterLoading.setSelected(settings.validatePoolOnLoading)
    
    tefCaCertFile.setText(settings.credentialsSpecs.certConfigItems.headOption.flatMap(_.file.value.map(_.toString)).getOrElse(""))
    validationHint_caCertFile.setError(settings.credentialsSpecs.certConfigItems.headOption.flatMap(_.file.error.map(_.message)))

    publicKeyEditor.setItems(settings.credentialsSpecs.publicKeyConfigItems)

    timestamperCertificateEditor.setItems(settings.credentialsSpecs.timestamperCertConfigItems)

    updateOkEnabled()
  }

  def setShowUIDebugControls(enabled: Boolean) = {
    btn_startScenicView.setVisible(enabled)
    btn_startScenicView.setManaged(enabled)

    btn_reloadCss.setVisible(enabled)
    btn_reloadCss.setManaged(enabled)
  }

  def initEventHandlers(): Unit = {

    // ca-cert-file

    btnChooseFile_caCert.onAction = handle {
      val currentFile = Option(tefCaCertFile.text.value).map(x => new File(x))
      validationHint_caCertFile.setError(None)
      val file = ChooseViaDialog.chooseFileViaDialog(
        dialogParent = getDialogPane.getScene.getWindow,
        initialFile = currentFile, title = "Select CA Certificate file",
        extensionFilters = Seq(new ExtensionFilter("pem files", "*.pem"))
      )
      tefCaCertFile.setText(file.map(_.toString).getOrElse(""))
      onCaCertFileChanged(file)
    }

    tefCaCertFile.onAction = handle {
      onCaCertFileChanged(Utils.stringToOption(tefCaCertFile.getText).map(new File(_)))
    }

    // archiveExtractionDir

    btnChooseFile_archiveExtractionDir.onAction = handle {
      val currentFile = Option(tefArchiveExtractionDir.text.value).map(x => new File(x))
      val file = ChooseViaDialog.chooseDirectoryViaDialog(
        dialogParent = getDialogPane.getScene.getWindow,
        initialDir = currentFile, title = "Select archive extraction dir"
      )
      tefArchiveExtractionDir.setText(file.map(_.toString).getOrElse(""))
      onArchiveExtractionDirChanged(file)
    }

    tefArchiveExtractionDir.onAction = handle {
      onArchiveExtractionDirChanged(Utils.stringToOption(tefArchiveExtractionDir.getText).map(new File(_)))
    }

    cbStartValidationAfterLoading.onAction = handle {
      initialSettings = initialSettings.copy(validatePoolOnLoading = cbStartValidationAfterLoading.isSelected)
    }
  }

  private def onCaCertFileChanged(file: Option[File]): Unit = {
    val errorMsg = file.flatMap(Utils.isInvalidX509CertPemFile).map(_.message)
    validationHint_caCertFile.setError(errorMsg)
    updateOkEnabled()
  }


  private def onArchiveExtractionDirChanged(file: Option[File]): Unit = {
    val errorMsg = file.flatMap(f => Utils.isFileUnwriteable(f, isDirectoryHint = true))
    validationHint_tefArchiveExtractionDir.setError(errorMsg)
    updateOkEnabled()
  }

  private def updateOkEnabled(): Unit = {
    getDialogPane().getButtonTypes().get(0)
    Option(getDialogPane.lookupButton(ButtonType.OK)) match {
      case Some(button) =>
        val keyAndCertFilesValid = Seq(validationHint_caCertFile,
          validationHint_tefArchiveExtractionDir).forall(_.getError.isEmpty)
        button.setDisable(!keyAndCertFilesValid)
      case _ =>
    }
  }
}


class PublicKeyEditor extends HBox with UIUpdateHandler {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val listView = new ListView[PublicKeyListItem]
  private val btnAdd = new Button("Add")
  private val btnRemove = new Button("Remove")

  private val valueEditPane = new PublicKeyValueEditPane

  private var onChangedHandler: (Seq[PublicKeyCfgItem]) => Unit = null

  private var updateFromValueEditPaneInProgressItem = Option.empty[PublicKeyCfgItem]
  private var selectionToBeRestored = Option.empty[PublicKeyCfgItem]

  private val listViewItems_unsorted = FXCollections.observableArrayList[PublicKeyListItem]()
  private var listViewItems_sorted = new SortedList[PublicKeyListItem](listViewItems_unsorted)


  locally {
    btnAdd.getStyleClass += "add"
    btnRemove.getStyleClass += "remove"
    btnRemove.setDisable(true)

    valueEditPane.setOnChanged(this.onPublicKeySpecEdited _)
    valueEditPane.setValue(None)

    val areaLeft = new VBox()
    areaLeft.getStyleClass += "area-left"

    val buttonRow = new HBox()
    buttonRow.getStyleClass += "button-row"
    buttonRow.getChildren.addAll(btnAdd, btnRemove)

    areaLeft.getChildren.addAll(listView, buttonRow)

    getChildren.addAll(areaLeft, valueEditPane)


    listViewItems_sorted.setComparator(new Comparator[PublicKeyListItem] {
      override def compare(o1: PublicKeyListItem, o2: PublicKeyListItem): Int = o1.keySpec.keyId.toLowerCase.compare(o2.keySpec.keyId.toLowerCase)
    })

    listView.setItems(listViewItems_sorted)

    listView.setCellFactory2(listView => new ListCell[PublicKeyListItem]() {
      override def updateItem(item: PublicKeyListItem, empty: Boolean): Unit = {
        super.updateItem(item, empty)
        setText(if (empty || item == null) "" else item.keySpec.keyId)
      }
    })

    // action handlers

    listView.getSelectionModel.selectedItemProperty().onChange { (_, oldSel, newSel) =>
      val newSelectionOpt = Option(newSel)
      if(updateFromValueEditPaneInProgressItem.isEmpty){

        if(newSelectionOpt.nonEmpty && selectionToBeRestored.map(_.stableId) == newSelectionOpt.map(_.keySpec.stableId)){
          selectionToBeRestored = None
        }else{
          valueEditPane.setValue(newSelectionOpt.map(_.keySpec))
        }
      }else {
        if(newSelectionOpt.nonEmpty){
          selectionToBeRestored = updateFromValueEditPaneInProgressItem
          JfxUtils.runLater{
            selectionToBeRestored.foreach{selToBeRest =>
              val index = listView.getItems.indexWhere(_.keySpec.stableId == selToBeRest.stableId)
              if(index >= 0)
                listView.getSelectionModel.select(index)
            }
          }
        }
      }
      btnRemove.setDisable(newSel == null)
    }

    btnAdd.onAction = handle {
      val newStableId = CredentialsConfig.getNewUniqueStableId(currentPublicKeySpecs)
      val newPubKeySpec = PublicKeyCfgItem(stableId = newStableId, keyId = "<new PublicKey>", algorithm = "", file = UIValue(None), description = None)
      val newItem = new PublicKeyListItem(newPubKeySpec)
      listViewItems_unsorted.add(newItem)
      listView.getSelectionModel.select(newItem)
      listView.requestFocus()
      onChangedHandler(currentPublicKeySpecs)
    }

    btnRemove.onAction = handle {
      Option(listView.getSelectionModel.getSelectedItem).foreach { sel =>
        listViewItems_unsorted.remove(sel)
        onChangedHandler(currentPublicKeySpecs)
        valueEditPane.setValue(None)
      }
    }
  }


  def setOnChanged(handler: (Seq[PublicKeyCfgItem]) => Unit): Unit = {
    onChangedHandler = handler
  }

  def setItems(items: Seq[PublicKeyCfgItem]): Unit = updateUI {
    listViewItems_unsorted.setAll(items.map(new PublicKeyListItem(_)): _*)
  }

  private def currentPublicKeySpecs : Seq[PublicKeyCfgItem] = listView.getItems.map(_.keySpec).sortBy(_.stableId)

  private def onPublicKeySpecEdited(spec: PublicKeyCfgItem): Unit = {

    //workaround: the ListView clears the selection when it contains only one element and this element is udpate..
    try{
      updateFromValueEditPaneInProgressItem = Option(spec)
      val oldKeySpecs = currentPublicKeySpecs

      // update ListView
      val affectedItemIndex = listViewItems_unsorted.indexWhere(_.keySpec.stableId == spec.stableId)
      if (affectedItemIndex >= 0) {
        listViewItems_unsorted.set(affectedItemIndex, new PublicKeyListItem(spec))
      }

      // controller-call
      if (currentPublicKeySpecs != oldKeySpecs) {
        whenModifiedByUserInput {
          onChangedHandler(currentPublicKeySpecs)
        }
      }
    }finally{
      updateFromValueEditPaneInProgressItem = None
    }
  }


  class PublicKeyValueEditPane extends VBox {
    private val logger = LoggerFactory.getLogger(this.getClass)
    private val grid = new GridPane()

    private val tefKeyId = new TextField()
    private val cbAlgorithm = new ComboBox[String]()
    private val tefFile = new TextField()
    private val validationHint_file = new ValidationErrorHint
    private val btnChooseFile = new Button("File")
    private val taDescription = new TextArea()

    private var currentKeySpec = Option.empty[PublicKeyCfgItem]

    private var onChangedHandler: (PublicKeyCfgItem) => Unit = _

    def setOnChanged(handler: (PublicKeyCfgItem) => Unit): Unit = {
      whenModifiedByUserInput {
        onChangedHandler = handler
      }
    }

    locally {
      getStyleClass += "public-key-value-edit-pane"
      grid.getStyleClass += "publickeys"

      tefKeyId.getStyleClass.addAll("value", "keyId")

      cbAlgorithm.getStyleClass.addAll("algorithm")
      cbAlgorithm.getItems.addAll("value", "rsa-sha256")

      tefFile.getStyleClass.addAll("value", "file")
      taDescription.getStyleClass.addAll("value", "description")

      getChildren.addAll(grid)

      populateGrid()
      initActionHandlers()
    }

    private def populateGrid(): Unit = {
      var rowInd = 0

      grid.add(new Label("Key Id"), 0, rowInd)
      grid.add(tefKeyId, 1, rowInd)

      rowInd += 1

      grid.add(new Label("Algorithm"), 0, rowInd)
      grid.add(cbAlgorithm, 1, rowInd)

      rowInd += 1

      grid.add(new Label("File"), 0, rowInd)
      grid.add(tefFile, 1, rowInd)
      grid.add(btnChooseFile, 2, rowInd)

      rowInd += 1

      grid.add(validationHint_file, 1, rowInd)

      rowInd += 1

      grid.add(new Label("Description"), 0, rowInd)
      grid.add(taDescription, 1, rowInd)
    }

    private def initActionHandlers(): Unit = {

      tefKeyId.textProperty().onChange { (_, _, newValue) =>
        onInputChanged()
      }
      cbAlgorithm.valueProperty().onChange { (_, _, newValue) =>
        onInputChanged()
      }
      tefFile.textProperty().onChange { (_, _, newValue) =>
        onInputChanged()
      }

      btnChooseFile.onAction = handle {
        val file = ChooseViaDialog.chooseFileViaDialog(
          dialogParent = getScene.getWindow,
          initialFile = stringToOption(tefFile.getText).map(new File(_)), title = "Select public key file",
          extensionFilters = Seq(new ExtensionFilter("pem files", "*.pem"))
        )
        tefFile.setText(file.map(_.toString).getOrElse(""))
        onFileChanged(file)
      }

      tefFile.onAction = handle{
        onFileChanged(Utils.stringToOption(tefFile.getText).map(new File(_)))
      }


      taDescription.textProperty().onChange { (_, _, newValue) =>
        onInputChanged()
      }
    }

    private def onFileChanged(file: Option[File]): Unit = {
      val errorMsg = file.flatMap(Utils.isInvalidPublicKeyPemFile).map(_.message)
      validationHint_file.setError(errorMsg)
    }

    private def onInputChanged(): Unit = {
      val old = currentKeySpec
      currentKeySpec = currentKeySpec.map { current =>
        val file_str = stringToOption(tefFile.getText)
        val fileNew = UIValue(file_str.map(x => new File(x)))
        current.copy(
          keyId = Option(tefKeyId.getText).getOrElse(""),
          algorithm = Option(cbAlgorithm.getValue).getOrElse(""),
          file = fileNew,
          description = Option(taDescription.getText)
        )
      }
      if (currentKeySpec != old) whenModifiedByUserInput {
        currentKeySpec.foreach { x =>
          onChangedHandler(x)
        }
      }
    }

    def setValue(keySpec: Option[PublicKeyCfgItem]): Unit = updateUI {
      this.currentKeySpec = keySpec
      tefKeyId.setText(keySpec.map(_.keyId).orNull)
      cbAlgorithm.setValue(keySpec.map(_.algorithm).orNull)
      tefFile.setText(keySpec.flatMap(_.file.value.map(_.toString)).getOrElse(""))
      validationHint_file.setError(keySpec.flatMap(_.file.error.map(_.message)))
      taDescription.setText(keySpec.flatMap(_.description) getOrElse (""))

      Seq(tefKeyId, cbAlgorithm, tefFile, taDescription).foreach{control =>
        control.setDisable(keySpec.isEmpty)
      }
    }
  }
}


object PublicKeyEditor {
  class PublicKeyListItem(val keySpec: PublicKeyCfgItem)
}



class TimestamperCertificateEditor extends HBox with UIUpdateHandler{

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val listView = new ListView[TimestamperCertCfgItem]

  private val btnAdd = new Button("Add")
  private val btnRemove = new Button("Remove")

  private val btnMoveItemUp = new Button("Up")
  private val btnMoveItemDown = new Button("Down")

  private val valueEditPane = new TimestamperCertValueEditPane

  private var onChangedHandler : (Seq[TimestamperCertCfgItem]) => Unit = _
  private var updateFromValueEditPaneInProgressItem = Option.empty[TimestamperCertCfgItem]
  private var selectionToBeRestored = Option.empty[TimestamperCertCfgItem]

  locally{

    val listViewArea = new VBox()
    listViewArea.getStyleClass += "listview-area"

    val buttonRow = new HBox()
    buttonRow.getStyleClass += "button-row"
    buttonRow.getChildren.addAll(btnAdd, btnRemove)

    listViewArea.getChildren.addAll(listView, buttonRow)

    listView.setCellFactory2(listView => new ListCell[TimestamperCertCfgItem]() {
      override def updateItem(item: TimestamperCertCfgItem, empty: Boolean): Unit = {
        super.updateItem(item, empty)
        setText(if (empty || item == null) "" else item.name)
      }
    })

    //Move up/down Buttons
    btnMoveItemUp.getStyleClass.add("moveup")
    val ico_btnMoveItemUp = new Pane()
    ico_btnMoveItemUp.getStyleClass.addAll(CssClass.svgIcon, CssClass.IconSize.small, "svg_arrow-up")
    btnMoveItemUp.setGraphic(ico_btnMoveItemUp)
    btnMoveItemUp.setDisable(true)

    btnMoveItemDown.getStyleClass.add("movedown")
    val ico_btnMoveItemDown = new Pane()
    ico_btnMoveItemDown.getStyleClass.addAll(CssClass.svgIcon, CssClass.IconSize.small, "svg_arrow-down")
    btnMoveItemDown.setGraphic(ico_btnMoveItemDown)
    btnMoveItemDown.setDisable(true)

    valueEditPane.setOnChanged(this.onCertSpecEdited _)
    valueEditPane.setValue(None)

    val rightBesideListView = new VBox()
    rightBesideListView.getStyleClass += "right-beside-list"
    rightBesideListView.maxHeightProperty().bind(listView.heightProperty())
    val springRegion = new Region
    VBox.setVgrow(springRegion, Priority.ALWAYS)
    rightBesideListView.getChildren.addAll(btnMoveItemUp, springRegion, btnMoveItemDown)

    getChildren.addAll(listViewArea, rightBesideListView, valueEditPane)

    getChildren.addAll()
  }

  def setOnChanged(handler: (Seq[TimestamperCertCfgItem]) => Unit): Unit = {
    onChangedHandler = handler

    listView.getSelectionModel.selectedItemProperty().onChange { (_,oldSel,newSel) =>
      btnRemove.setDisable(newSel == null)
      btnMoveItemUp.setDisable(newSel == null)
      btnMoveItemDown.setDisable(newSel == null)

      val newSelectionOpt = Option(newSel)

      if(updateFromValueEditPaneInProgressItem.isEmpty){
        if(newSelectionOpt.nonEmpty && selectionToBeRestored.map(_.stableId) == newSelectionOpt.map(_.stableId)){
          selectionToBeRestored = None
        }else{
          valueEditPane.setValue(newSelectionOpt)
        }
      }else {
        if(newSelectionOpt.isEmpty){
          selectionToBeRestored = updateFromValueEditPaneInProgressItem
          JfxUtils.runLater{
            selectionToBeRestored.foreach{selToBeRest =>
              val index = listView.getItems.indexWhere(_.stableId == selToBeRest.stableId)
              if(index >= 0)
                listView.getSelectionModel.select(index)
            }
          }
        }
      }
    }

    btnAdd.onAction= handle{
      val newItem = TimestamperCertCfgItem(stableId=CredentialsConfig.getNewUniqueStableId(listView.getItems.toSeq),
        priority=listView.getItems.size + 1, name="<new Certificate>", file=UIValue(None) , description=None)
      listView.getItems.add(newItem)
      rewriteCertSpecPositions()
      listView.getSelectionModel.select(newItem)
      listView.requestFocus()
      onChangedHandler(listView.getItems.toSeq)
    }

    btnRemove.onAction= handle{
      Option(listView.getSelectionModel.getSelectedItem).foreach { item =>
        listView.getItems.remove(item)
        rewriteCertSpecPositions()
        onChangedHandler(listView.getItems.toSeq)
      }
    }

    btnMoveItemUp.onAction = handle{
      Option(listView.getSelectionModel.getSelectedItem).foreach{item =>
        val index = listView.getItems.indexOf(item)
        index match {
          case i if(index <= 0) => // do nothing
          case i =>
            val selection_stableId = Option(listView.getSelectionModel.getSelectedItem).map(_.stableId)
            listView.getItems.remove(index)
            listView.getItems.add(index - 1, item)
            restoreSelectionViaStableId(selection_stableId)
        }
      }
      rewriteCertSpecPositions()
      onChangedHandler(listView.getItems.toSeq)
    }

    btnMoveItemDown.onAction = handle {
      Option(listView.getSelectionModel.getSelectedItem).foreach{item =>
        val index = listView.getItems.indexOf(item)
        index match {
          case i if(index < listView.getItems.size - 1) =>
            val selection_stableId = Option(listView.getSelectionModel.getSelectedItem).map(_.stableId)
            listView.getItems.remove(index)
            listView.getItems.add(index + 1, item)
            restoreSelectionViaStableId(selection_stableId)
          case i => // do nothing
        }
      }
      rewriteCertSpecPositions()
      onChangedHandler(listView.getItems.toSeq)
    }
  }

  private def restoreSelectionViaStableId(stableId: Option[String]): Unit = {
    stableId.foreach{ oldSel_stableId =>
      val indexToSelect = listView.getItems.indexWhere(_.stableId == oldSel_stableId)
      if(indexToSelect >= 0){
        listView.getSelectionModel.select(indexToSelect)
      }
    }
  }

  def setItems(items : Seq[TimestamperCertCfgItem]): Unit = updateUI {
    listView.getItems.setAll(items.sortBy(_.priority):_*)
  }


  /**
    * When an item is added or removed from/to the `ListView`, the all `TimestamperCertSpec.position` values must be adjusted.
    * */
  private def rewriteCertSpecPositions(): Unit = {
    val selection_stableId = Option(listView.getSelectionModel.getSelectedItem).map(_.stableId)
    val updatedItems = listView.getItems.toSeq.zipWithIndex.map{ case (item, index) => item.copy(priority = index + 1) }
    listView.getItems.setAll(updatedItems:_*)
    restoreSelectionViaStableId(selection_stableId)
  }

  private def onCertSpecEdited(spec: TimestamperCertCfgItem): Unit = {
    //workaround: the ListView clears the selection when it contains only one element and this element is udpate..
    try{
      updateFromValueEditPaneInProgressItem = Option(spec)

      // update ListView
      val lvIndex = listView.getItems.indexWhere(_.stableId == spec.stableId)
      require(lvIndex >= 0, s"unexpected lvIndex: $lvIndex")
      listView.getItems.set(lvIndex, spec)

      // controller-call
      whenModifiedByUserInput {
        onChangedHandler(listView.getItems.toSeq)
      }
    }finally{
      updateFromValueEditPaneInProgressItem = None
    }
  }


  class TimestamperCertValueEditPane extends VBox {
    private val logger = LoggerFactory.getLogger(this.getClass)
    private val grid = new GridPane()

    private val tefName = new TextField()
    private val tefFile = new TextField()
    private val validationHint_file = new ValidationErrorHint
    private val btnChooseFile = new Button("File")
    private val taDescription = new TextArea()

    private var currentCertSpec = Option.empty[TimestamperCertCfgItem]

    private var onChangedHandler: (TimestamperCertCfgItem) => Unit = _

    def setOnChanged(handler: (TimestamperCertCfgItem) => Unit): Unit = {
      whenModifiedByUserInput {
        onChangedHandler = handler
      }
    }

    locally {
      getStyleClass += "timestamper-cert-value-edit-pane"
      grid.getStyleClass += "timestampercerts"

      tefName.getStyleClass.addAll("value", "name")
      tefFile.getStyleClass.addAll("value", "file")
      taDescription.getStyleClass.addAll("value", "description")

      getChildren.addAll(grid)

      populateGrid()
      initActionHandlers()
    }

    private def populateGrid(): Unit = {
      var rowInd = 0

      grid.add(new Label("Name"), 0, rowInd)
      grid.add(tefName, 1, rowInd)

      rowInd += 1

      grid.add(new Label("File"), 0, rowInd)
      grid.add(tefFile, 1, rowInd)
      grid.add(btnChooseFile, 2, rowInd)

      rowInd += 1

      grid.add(validationHint_file, 1, rowInd)

      rowInd += 1

      grid.add(new Label("Description"), 0, rowInd)
      grid.add(taDescription, 1, rowInd)
    }

    private def initActionHandlers(): Unit = {

      tefName.textProperty().onChange { (_, _, newValue) =>
        onInputChanged()
      }

      tefFile.textProperty().onChange { (_, _, newValue) =>
        onInputChanged()
      }

      btnChooseFile.onAction = handle {
        val file = ChooseViaDialog.chooseFileViaDialog(
          dialogParent = getScene.getWindow,
          initialFile = stringToOption(tefFile.getText).map(new File(_)), title = "Select certificate file",
          extensionFilters = Seq(new ExtensionFilter("pem files", "*.pem"))
        )
        tefFile.setText(file.map(_.toString).getOrElse(""))
        onFileChanged(file)
      }

      tefFile.onAction = handle{
        onFileChanged(Utils.stringToOption(tefFile.getText).map(new File(_)))
      }

      taDescription.textProperty().onChange { (_, _, newValue) =>
        onInputChanged()
      }
    }

    private def onFileChanged(file: Option[File]): Unit = {
      val errorMsg = file.flatMap(Utils.isInvalidX509CertPemFile).map(_.message)
      validationHint_file.setError(errorMsg)
    }

    private def onInputChanged(): Unit = {
      val old = currentCertSpec
      currentCertSpec = currentCertSpec.map { current =>
        val file_str = stringToOption(tefFile.getText)
        val fileNew = UIValue(file_str.map(x => new File(x)))
        current.copy(
          name = Option(tefName.getText).getOrElse(""),
          file = fileNew,
          description = Option(taDescription.getText)
        )
      }
      if (currentCertSpec != old) whenModifiedByUserInput {
        currentCertSpec.foreach { x =>
          onChangedHandler(x)
        }
      }
    }

    def setValue(certSpec: Option[TimestamperCertCfgItem]): Unit = updateUI {
      this.currentCertSpec = certSpec
      tefName.setText(certSpec.map(_.name).orNull)
      tefFile.setText(certSpec.flatMap(_.file.value.map(_.toString)).getOrElse(""))
      validationHint_file.setError(certSpec.flatMap(_.file.error.map(_.message)))
      taDescription.setText(certSpec.flatMap(_.description) getOrElse (""))
      Seq(tefName, tefFile, taDescription).foreach{control =>
        control.setDisable(certSpec.isEmpty)
      }
    }
  }
}

