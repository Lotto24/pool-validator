package view

import java.nio.charset.StandardCharsets
import java.text.NumberFormat
import java.time.format.{DateTimeFormatter, FormatStyle}
import java.util.Locale

import com.sun.javafx.scene.control.skin.TableViewSkin
import domain._
import domain.products.Bet
import domain.products.ejs.EjsBet
import domain.products.ems.EmsBet
import domain.products.gls.GlsBet
import domain.products.glss.GlsSBet
import domain.products.s6.S6Bet
import domain.products.s77.S77Bet
import javafx.beans.binding.Bindings
import javafx.beans.property.SimpleDoubleProperty
import javafx.scene.Node
import javafx.scene.control._
import javafx.scene.layout._
import model.ArchiveDetailData.BetBreakdownRowItem
import model._
import scalafx.Includes._
import scalafx.scene.control.{Label, TextField}
import view.CssClass.Color
import view.DetailView._
import view.JfxImplicits._
import view.controls.TextAreaWithAutoHeight
import view.impl.StructureElements.{EmptyPanelHint, SectionSeparator, VSpacer, ViewHeader}

import scala.collection.JavaConversions._
import scala.util.Try


class DetailView extends VBox {

  val scrollPane = new ScrollPane
  private val detailViewHeader = new ViewHeader("Detail View")
  private val orderDocDetailViewHolder = new StackPane()

  //DetailPanes
  private val emptyPanelHint = new EmptyPanelHint
  private val detailDataLoadingError = new DetailDataErrorPane
  private val orderDetailPane = new OrderDetailPane
  private val orderResultDetailPane = new OrderResultDetailPane
  private val orderResultSignatureDetailPane = new OrderResultSignatureDetailPane
  private val orderResultSignatureTimestampDetailPane = new OrderResultSignatureTimestampDetailPane
  private val orderSignatureDetailPane = new OrderSignatureDetailPane
  private val hedgingDetailPane = new HedgingDetailPane
  private val archiveDetailPane = new ArchiveDetailPane
  private val orderDirDetailPane = new OrderDirDetailPane

  locally {

    orderDocDetailViewHolder.getChildren.setAll(
      detailDataLoadingError,
      orderDetailPane,
      orderResultDetailPane,
      orderResultSignatureDetailPane,
      orderResultSignatureTimestampDetailPane,
      orderSignatureDetailPane,
      hedgingDetailPane,
      archiveDetailPane,
      orderDirDetailPane,
      emptyPanelHint
    )

    orderDocDetailViewHolder.getStyleClass.addAll("view-holder")

    scrollPane.setFitToWidth(true)
    scrollPane.setFitToHeight(true)

    VBox.setVgrow(emptyPanelHint, Priority.ALWAYS)

    scrollPane.setContent(orderDocDetailViewHolder.asInstanceOf[Pane])
    scrollPane.prefHeightProperty().bind(this.heightProperty())
    scrollPane.getStyleClass.add(Color.viewcolors)

    getChildren.addAll(detailViewHeader, scrollPane)
    getStyleClass.addAll(Color.viewcolors, "detail-view")
  }

  def setArchiveLoaded(loaded: Boolean): Unit = {
    emptyPanelHint.setText(
      loaded match{
        case true => "Select an item \n to show details for"
        case _ => "No\nPool Archive\nloaded"
      }
    )
  }

  private def setExclusivelyVisible(detailPane : Node): Unit = {
    orderDocDetailViewHolder.getChildren.foreach{child =>
      child.setVisible(child eq detailPane)
      child.setManaged(child eq detailPane)
    }
  }

  private var currentDetailPane : Option[Pane] = null   //null is needed here to handle the initial update case

  def setData(aData: Option[DetailData]): Unit = {

    val newDetailPaneOpt: Option[DetailPane[DetailData]] = {
      aData.flatMap { data =>
        val detailContent: DetailPane[_ <: DetailData] = {
          data match {
            case OrderDocDetailData(doc: Order) => orderDetailPane
            case OrderDocDetailData(doc: OrderResult) => orderResultDetailPane
            case OrderDocDetailData(doc: OrderResultSignature) => orderResultSignatureDetailPane
            case OrderDocDetailData(doc: OrderResultSignatureTimestamp) => orderResultSignatureTimestampDetailPane
            case OrderDocDetailData(doc: OrderSignature) => orderSignatureDetailPane
            case data: OrderHedgingDetailData => hedgingDetailPane
            case data: ArchiveDetailData => archiveDetailPane
            case data : OrderDirectoryDetailData => orderDirDetailPane
            case err : DetailDataLoadingError => detailDataLoadingError  
            case d => sys.error(s"unexpected DetailData: $d")
          }
        }
        Option(detailContent.asInstanceOf[DetailPane[DetailData]])
      }
    }

    val detailPaneChanged = newDetailPaneOpt != currentDetailPane

    currentDetailPane = newDetailPaneOpt match {
      case c @ Some(content) =>
        if(detailPaneChanged)
          setExclusivelyVisible(content)
        aData.foreach(content.setData)
        detailViewHeader.setText(content.title)
        c
      case _ =>
        if(detailPaneChanged)
          setExclusivelyVisible(emptyPanelHint)
        detailViewHeader.setText("Detail View")
        Some(emptyPanelHint)
    }
  }
}


object DetailView {

  protected val dateFormat = DateTimeFormatter.ofLocalizedDate(FormatStyle.MEDIUM)
  
  trait DetailPane[-T <: DetailData] extends Pane{
    def title: String
    def setData(data: T): Unit
  }

  /**
    * Little helper class for displaying key-value pairs in different styles.
    * */
  class KeyValuePair(keyLabelText: String,
                     isMultiLineContent : Boolean=false,
                     level: Int = 1,
                     simpleLabel: Boolean = false,
                     cssClass: String=null) extends HBox {
    private val keyLabel = new Label(keyLabelText)
    private var valueLabel = Option.empty[Label]
    private var valueTextControl = Option.empty[TextInputControl]

    locally {

      if(simpleLabel){
        valueLabel = Some(new Label)
        valueTextControl = None
      }else {
        valueTextControl = if (isMultiLineContent) {
          val ta = new TextAreaWithAutoHeight()
          ta.setWrapText(true)
          Some(ta)
        } else Some(new TextField())
      }

      valueTextControl.foreach(_.setEditable(false))

      getStyleClass.addAll( Option("key-value-pair") ++ Option(cssClass))

      if(level > 1)
        getStyleClass += CssClass.indendation(level)

      keyLabel.getStyleClass += "key"
      valueLabel.foreach(_.getStyleClass += "value")
      valueTextControl.foreach(_.getStyleClass += "value")
      getChildren.add(keyLabel)
      valueLabel.foreach(getChildren.add(_))
      valueTextControl.foreach(getChildren.add(_))
    }

    def setValue(value: String): Unit = {
      valueLabel.foreach(_.text = value)
      valueTextControl.foreach(_.setText(value))
    }
  }

  class DetailDataErrorPane extends VBox with DetailPane[DetailDataLoadingError] {

    private val kvl_errorMessage = new KeyValuePair("Error", cssClass = "metadata")
    private val kvl_errorDetail = new KeyValuePair("Detail", cssClass = "metadata", isMultiLineContent = true)

    override val title: String = "Detail data loading error"

    locally {
      getStyleClass.addAll("detail-pane", "detail-data-error-pane")

      val dataGroup = new VBox
      dataGroup.getStyleClass += "data-group"
      
      VBox.setVgrow(dataGroup, Priority.ALWAYS)
      
      dataGroup.getChildren.addAll(
        new SectionSeparator("Detail data loading error"),
        kvl_errorMessage, 
        kvl_errorDetail
      )      
      
      getChildren.addAll(dataGroup)
    }
    
    override def setData(data: DetailDataLoadingError): Unit = {
      kvl_errorMessage.setValue(data.error.message)
      kvl_errorDetail.setValue(data.error.detail.getOrElse(""))
    }
  }
  
  class OrderDetailPane extends VBox with DetailPane[OrderDocDetailData[Order]] {
    private val kvl_meta_creationDate = new KeyValuePair("Creation date", cssClass = "metadata")
    private val kvl_meta_retailCustomerId = new KeyValuePair("Retail customer", cssClass = "metadata")
    private val kvl_meta_retailer = new KeyValuePair("Retailer", cssClass = "metadata")
    private val kvl_meta_retailerHref = new KeyValuePair("Retailer HREF", cssClass = "metadata")
    private val kvl_meta_retailerOrderReference = new KeyValuePair("Retailer order reference", cssClass = "metadata")

    private val dateFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
    private val dateFormatter2 = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM, FormatStyle.FULL).withLocale(Locale.UK)

    val ordersGroup = new VBox

    override val title : String = "Order Details"

    locally {

      getStyleClass.addAll("detail-pane", "order-detail-pane")

      val metaDataGroup = new VBox

      ordersGroup.getStyleClass += "orders-group"

      metaDataGroup.getChildren.addAll(
        kvl_meta_creationDate,
        kvl_meta_retailCustomerId,
        kvl_meta_retailer,
        kvl_meta_retailerHref,
        kvl_meta_retailerOrderReference)

      getChildren.addAll(
        new SectionSeparator("Metadata"),
        metaDataGroup, new VSpacer(), new VSpacer(),
        new SectionSeparator("Gaming product orders"), new VSpacer(),
        ordersGroup)
    }

    override def setData(data: OrderDocDetailData[Order]): Unit = {
      kvl_meta_creationDate.setValue {
        Try(dateFormatter2.format(data.doc.metaData.creationDate)).getOrElse(data.doc.metaData.creationDate.toString)
      }
      kvl_meta_retailCustomerId setValue data.doc.metaData.retailCustomerId
      kvl_meta_retailer setValue data.doc.metaData.retailer.name
      kvl_meta_retailerHref setValue data.doc.metaData.retailerHref
      kvl_meta_retailerOrderReference setValue data.doc.metaData.retailerOrderReference

      ordersGroup.getChildren.clear()

      data.doc.gamingProductOrders.foreach { case (productId, productOrder) =>
        val productLabel = new Label(s"\u2022 ${productId.toString}")
        productLabel.getStyleClass += "producturl"
        ordersGroup.getChildren.add(productLabel)
        val orderPane = view.OrderPaneFactory.createOrderPane(productOrder).setOrder(productOrder)
        ordersGroup.getChildren.add(orderPane)
      }
    }
  }

  
  class OrderResultDetailPane extends VBox with DetailPane[OrderDocDetailData[OrderResult]] {
    private val dateFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME

    private val kvl_creationTime = new KeyValuePair("Creation time")
    private val kvl_orderDigest = new KeyValuePair("Order digest")
    private val kvl_orderProcessingResult = new KeyValuePair("Order Processing result")
    private val kvl_retailerHref = new KeyValuePair("Retailer HREF")
    private val kvl_retailerOrderRef = new KeyValuePair("Retailer order reference")

    override val title : String = "Order Result Details"

    locally {
      getStyleClass += "detail-pane"

      val dataGroup = new VBox

      dataGroup.getChildren.addAll(
        new SectionSeparator("Order Result Details"),
        kvl_creationTime,
        kvl_orderDigest,
        kvl_orderProcessingResult,
        kvl_retailerHref,
        kvl_retailerOrderRef
      )

      getChildren.addAll(dataGroup)
    }

    override def setData(data: OrderDocDetailData[OrderResult]): Unit = {
      kvl_creationTime.setValue {
        Try(dateFormatter.format(data.doc.creationTime)).getOrElse(data.doc.creationTime.toString)
      }
      kvl_orderDigest.setValue(new String(data.doc.orderDigest.toArray, StandardCharsets.UTF_8))
      kvl_orderProcessingResult setValue data.doc.orderProcessingResult
      kvl_retailerHref setValue data.doc.retailerHref
      kvl_retailerOrderRef setValue data.doc.retailerOrderReference
    }
  }

  class OrderResultSignatureDetailPane extends VBox with DetailPane[OrderDocDetailData[OrderResultSignature]] {

    private val kvl_algorithm = new KeyValuePair("Algorithm")
    private val kvl_keyId = new KeyValuePair("Operator key Id")
    private val kvl_signature = new KeyValuePair("Signature", isMultiLineContent = true)

    override val title : String = "Order Result Signature Details"

    locally {
      getStyleClass += "detail-pane"

      val dataGroup = new VBox

      dataGroup.getChildren.addAll(
        new SectionSeparator("Order Result Signature Details"),
        kvl_algorithm,
        kvl_keyId,
        kvl_signature
      )

      getChildren.addAll(dataGroup)
    }

    override def setData(data: OrderDocDetailData[OrderResultSignature]): Unit = {
      kvl_algorithm setValue data.doc.algorithm
      kvl_keyId setValue data.doc.keyId
      kvl_signature.setValue(new String(data.doc.signature.toArray, StandardCharsets.UTF_8))
    }
  }

  class OrderResultSignatureTimestampDetailPane extends VBox with DetailPane[OrderDocDetailData[OrderResultSignatureTimestamp]] {
    override val title : String = "Order Result Signature Timestamp Details"

    private val kvl_status = new KeyValuePair("Status")
    private val kvl_acceptanceDate = new KeyValuePair("Acceptance date")
    private val kvl_tsa = new KeyValuePair("TSA")


    locally {
      getStyleClass += "detail-pane"
      val dataGroup = new VBox
      dataGroup.getChildren.addAll(
        new SectionSeparator("Order Result Signature Timestamp Details"),
        kvl_status,
        kvl_acceptanceDate,
        kvl_tsa
      )
      getChildren.addAll(dataGroup)
    }

    override def setData(data: OrderDocDetailData[OrderResultSignatureTimestamp]): Unit = {

      val errorTxt = "<not available>"

      val tsResp = data.doc.timeStampResponse
      val time = tsResp.map(_.getTimeStampToken.getTimeStampInfo.getGenTime.toInstant)
      val timeStr = time.map(t => DateTimeFormatter.ISO_INSTANT.format(t)).getOrElse(errorTxt)

      val tsToken = tsResp.map(_.getTimeStampToken)
      val statusStr = tsResp.map(_.getStatusString).getOrElse(errorTxt)
      val tsaStr = tsToken.map(_.getTimeStampInfo.getTsa.toString).getOrElse(errorTxt)

      kvl_status setValue statusStr
      kvl_acceptanceDate setValue timeStr
      kvl_tsa setValue tsaStr
    }
  }

  class OrderSignatureDetailPane extends VBox with DetailPane[OrderDocDetailData[OrderSignature]] {

    override val title : String = "Order Signature Details"

    private val kvl_keyId = new KeyValuePair("Retailer key Id")
    private val kvl_algorithm = new KeyValuePair("Algorithm")
    private val kvl_docPath = new KeyValuePair("Document path")
    private val kvl_signature = new KeyValuePair("Signature", isMultiLineContent = true)

    locally{

      getStyleClass += "detail-pane"

      getChildren.setAll(
        new SectionSeparator("Order Signature Details"),
        kvl_keyId, kvl_algorithm,
        kvl_signature,
        kvl_docPath
      )
    }

    override def setData(data: OrderDocDetailData[OrderSignature]): Unit = {
      kvl_keyId.setValue(data.doc.keyId)
      kvl_algorithm.setValue(data.doc.algorithm)
      kvl_docPath.setValue(data.doc.docPath.toString)
      kvl_signature.setValue(new String(data.doc.signature.toArray, StandardCharsets.UTF_8))
    }
  }

  
  class HedgingDetailPane extends VBox with DetailPane[OrderHedgingDetailData] {

    override val title : String = "Order Metadata Details"

    val tableView = new TableView[OrderHedgingDetailData.RowData]
  
    locally {
      getStyleClass.addAll("detail-pane", "hedging-detail-pane")

      tableView.getStyleClass += "hedgingdata"
     
      val heightBinding = tableView.fixedCellSizeProperty().multiply(Bindings.size(tableView.getItems()).add(1.16))
      tableView.prefHeightProperty().bind(heightBinding)
      tableView.minHeightProperty().bind(heightBinding)
      tableView.maxHeightProperty().bind(heightBinding)
      tableView.setColumnResizePolicy(TableView.UNCONSTRAINED_RESIZE_POLICY)

      tableView.getColumns += new TableColumn[OrderHedgingDetailData.RowData, String] {
        getStyleClass += "productid"
        setText("ProductId")
        setPrefWidth(100)  //INFO the width currently cannot be set via CSS when the columns shall be resizable..
        this.addCellValuePojoSource(_.getValue.productId.name)
      }

      tableView.getColumns += new TableColumn[OrderHedgingDetailData.RowData, String] {
        getStyleClass += "variant"
        setText("Variant")
        setPrefWidth(110)
        this.addCellValuePojoSource(_.getValue.variant.getOrElse(""))
      }

      tableView.getColumns += new TableColumn[OrderHedgingDetailData.RowData, String] {
        getStyleClass += "drawdate"
        setText("Draw date")
        setPrefWidth(110)
        this.addCellValuePojoSource(_.getValue.drawDate.format(dateFormat))
      }
      
      tableView.getColumns += new TableColumn[OrderHedgingDetailData.RowData, Bet] {
        getStyleClass += "bets"
        setText("Bets")
        setPrefWidth(550)
        this.addCellValuePojoSource(_.getValue.bet)
        
        this.cellFactory = (x) => new TableCell[OrderHedgingDetailData.RowData, Bet](){
          
          getStyleClass.add("bet")
          
          override protected def updateItem (bet: Bet, empty: Boolean) : Unit = {
            super.updateItem(bet, empty)
            if(empty || (bet == null)){
              setText("")
            }else{
              setText(
                bet match {
                  case bet : EjsBet => s"Numbers: ${bet.numbers.mkString(",")}".padTo(48, ' ') + s"Euronumbers: ${bet.euroNumbers.mkString(",")}"
                  case bet : EmsBet => s"Numbers: ${bet.numbers.mkString(",")}".padTo(48, ' ') + s"Starnumbers: ${bet.starnumbers.mkString(",")}"
                  case bet : GlsBet => s"Numbers: ${bet.numbers.mkString(",")}".padTo(48, ' ') + s"Supernumber: ${bet.supernumber}"
                  case bet : GlsSBet => s"Numbers: ${bet.numbers.mkString(",")}"
                  case bet : S6Bet => s"Numbers: ${bet.numbers.mkString(",")}"
                  case bet : S77Bet => s"Numbers: ${bet.numbers.mkString(",")}"
                  case bet => bet.toString
                }
              )
            }
          }
        } 
      }

      tableView.getColumns += new TableColumn[OrderHedgingDetailData.RowData, String] {
        getStyleClass += "hedgingchannel"
        setText("Hedging channel")
        setPrefWidth(150)
        this.addCellValuePojoSource(_.getValue.hedgingChannel)
      }

      tableView.getColumns.foreach{ col =>
        col.impl_setReorderable(false)
        col.setSortable(false)
      }
      
      getChildren.addAll(
        new SectionSeparator("Hedging data", level = 3),
        tableView
      )
    }

    override def setData(data: OrderHedgingDetailData): Unit = {
      val data_flat = data.expandedBetListPerProduct.flatMap(_._2)
      tableView.getItems.setAll(data.expandedBetListPerProduct.flatMap(_._2))
    }
  }

  class ArchiveDetailPane extends VBox with DetailPane[ArchiveDetailData] {

    private val grpBetsCountPerProduct = new VBox(){
      getStyleClass.add("betscountperproduct")
    }

    private val numberFormat = NumberFormat.getInstance(Locale.UK)
    
    private val kvl_ordersCount = new KeyValuePair("Total orders count", simpleLabel = true)
    private val table_betCountBreakdown = new TableView[BetBreakdownRowItem] {
      val headerHeightProperty: SimpleDoubleProperty = new SimpleDoubleProperty(0)
      skinProperty().onChange((_,_,newSkin) => {
        headerHeightProperty.bind(newSkin.asInstanceOf[TableViewSkin[BetBreakdownRowItem]].getTableHeaderRow.heightProperty())
      })
      override def createDefaultSkin(): Skin[_ <: Skinnable] = new TableViewSkin[BetBreakdownRowItem](this){
        override def computePrefWidth(height: Double, topInset: Double, rightInset: Double, bottomInset: Double, leftInset: Double): Double = {
          super.computePrefWidth(height, topInset, rightInset, bottomInset, leftInset) + 17 //workaround to avoid horizontal scrollbars in some edge cases
        }
      }
    }

    private val kvl_archiveFile = new KeyValuePair("Archive file")
    private val kvl_extractedToDir = new KeyValuePair("Archive directory")

    private val kvl_poolMetadata_productId = new KeyValuePair("Product Id")
    private val kvl_poolMetadata_poolId = new KeyValuePair("Pool Id")
    private val kvl_poolMetadata_drawDate = new KeyValuePair("Draw date")

    private val kvl_poolMetadata_poolDigestBase64 = new KeyValuePair("Digest-data")
    private val kvl_poolMetadata_poolDigestAlgorithm = new KeyValuePair("Algorithm")
    private val kvl_poolDigestTimestamp = new KeyValuePair("Pool digest timestamp", isMultiLineContent=true)

    private val kvl_validationState = new KeyValuePair("Validation state", simpleLabel = true)
    private val kvl_totalOrdersCount = new KeyValuePair("Total orders count", simpleLabel = true)
    private val kvl_validOrdersCount = new KeyValuePair("Valid orders count", simpleLabel = true)
    private val kvl_invalidCount = new KeyValuePair("Invalid orders count", simpleLabel = true)

    override val title : String = "Pool archive details"

    locally {
      getStyleClass.addAll("detail-pane", "archive-detail-pane")
      getChildren.addAll(
        new SectionSeparator("Orders and bets statistics"),
        kvl_ordersCount,
        grpBetsCountPerProduct,
        table_betCountBreakdown,
        new SectionSeparator("Archive metadata"),
        kvl_poolMetadata_productId,
        kvl_poolMetadata_poolId,
        kvl_poolMetadata_drawDate,
        new SectionSeparator("Participation pool digest"),
        kvl_poolMetadata_poolDigestBase64,
        kvl_poolMetadata_poolDigestAlgorithm,
        kvl_poolDigestTimestamp,
        new SectionSeparator("Validation stats"),
        kvl_validationState, kvl_totalOrdersCount, kvl_validOrdersCount, kvl_invalidCount,
        new SectionSeparator("Archive locations"),
        kvl_archiveFile, kvl_extractedToDir
      )
      
      this.setFillWidth(false)
      VBox.setVgrow(table_betCountBreakdown, Priority.ALWAYS)
    }

    override def setData(data: ArchiveDetailData): Unit = {
      initBetCountBreakdownTableView(data)
      kvl_ordersCount.setValue(numberFormat.format(data.orderStats.totalOrdersCount))
      grpBetsCountPerProduct.getChildren.clear()
      data.orderStats.betsCountPerProduct.foreach { case (productId, betsCount) =>
        val kvl_betsCountForProduct = new KeyValuePair(s"# ${productId.name}-bets (total)", simpleLabel = true)
        kvl_betsCountForProduct.getStyleClass.add("betscountperproduct")
        kvl_betsCountForProduct.setValue(numberFormat.format(betsCount))
        grpBetsCountPerProduct.getChildren.add(kvl_betsCountForProduct)
      }

      kvl_archiveFile.setValue(data.poolSource.path.toString)
      kvl_extractedToDir.setValue(data.extractedToDir.toString)

      kvl_poolMetadata_productId.setValue(data.poolMetadata.productId.name)
      kvl_poolMetadata_poolId.setValue(data.poolMetadata.participationPoolId)
      kvl_poolMetadata_drawDate.setValue(data.poolMetadata.drawDate.toString)

      kvl_poolMetadata_poolDigestBase64.setValue(
        data.poolMetadata.poolDigest.map(x => new String(x.base64.toArray, StandardCharsets.UTF_8)).getOrElse("<undefined>")
      )
      kvl_poolMetadata_poolDigestAlgorithm.setValue(data.poolMetadata.poolDigest.map(_.algorithm).getOrElse("<undefined>"))
      kvl_poolDigestTimestamp.setValue(
        data.poolDigestTimestamp.map(ts => new String(ts.toArray, StandardCharsets.UTF_8)).getOrElse("<undefined>")
      )

      kvl_validationState.setValue(data.validationState.toString)
      kvl_totalOrdersCount.setValue(numberFormat.format(data.totalOrdersCount))
      kvl_validOrdersCount.setValue(numberFormat.format(data.validOrdersCount))
      kvl_invalidCount.setValue(numberFormat.format(data.invalidOrdersCount))
    }

    private def initBetCountBreakdownTableView(data: ArchiveDetailData): Unit = {
      val columnsBuilder = Vector.newBuilder[TableColumn[BetBreakdownRowItem, _]]
      
      table_betCountBreakdown.getStyleClass += "betcount-breakdown"

      val heightBinding = table_betCountBreakdown.fixedCellSizeProperty().multiply(Bindings.size(table_betCountBreakdown.getItems()))
        .add(table_betCountBreakdown.headerHeightProperty).add(2)
      table_betCountBreakdown.prefHeightProperty().bind(heightBinding)
      table_betCountBreakdown.minHeightProperty().bind(heightBinding)
      table_betCountBreakdown.maxHeightProperty().bind(heightBinding)

      columnsBuilder += new TableColumn[BetBreakdownRowItem, String] {
        getStyleClass += "retailer"
        setText("Retailer")
        setPrefWidth(200) //INFO setting this via css (`-fx-pref-width`) causes layout problems since the value is NOT applied to `TableColumn.prefWidthProp`!
        this.addCellValuePojoSource(_.getValue.retailer.name)
      }
      columnsBuilder += new TableColumn[BetBreakdownRowItem, String] {
        getStyleClass += "origin"
        setText("Origin")
        setPrefWidth(200)
        this.addCellValuePojoSource(_.getValue.origin.map(_.name).getOrElse("<none>"))
      }
      columnsBuilder += new TableColumn[BetBreakdownRowItem, String] {
        getStyleClass += "orders-count"
        setText("#orders")
        setPrefWidth(200)
        this.addCellValuePojoSource(x => numberFormat.format(x.getValue.ordersCount))
      }
      columnsBuilder ++= data.productIds.toVector.sortBy(_.name).map { productId =>
        new TableColumn[BetBreakdownRowItem, String] {
          getStyleClass += "betcount"
          setText(s"#${productId.name}-bets")
          setPrefWidth(200)
          this.addCellValuePojoSource(x => numberFormat.format(x.getValue.betCountPerProduct.getOrElse(productId, 0)))
        }        
      }
      
      table_betCountBreakdown.getColumns.setAll(columnsBuilder.result().map { column =>
        column.impl_setReorderable(false)
        column.setSortable(false)
        column
      })
      
      table_betCountBreakdown.getItems.setAll(data.betBreakdownRowData)
    }
  }
}

class OrderDirDetailPane extends VBox with DetailPane[OrderDirectoryDetailData]{

  private val kvl_name = new KeyValuePair("Name")
  private val kvl_orderId = new KeyValuePair("Order Id")
  private val kvl_location = new KeyValuePair("Physical location")
  private val kvl_validationState = new KeyValuePair("Validation state", simpleLabel = true)

  override val title : String = "Order directory details"

  locally {
    getStyleClass += "detail-pane"
    getChildren.addAll(
      new SectionSeparator("Order directory details"),
      kvl_name, kvl_orderId, kvl_location, kvl_validationState
    )
  }

  override def setData(data: OrderDirectoryDetailData): Unit = {
    kvl_name.setValue(data.path.toString)
    kvl_orderId.setValue(data.orderId)
    kvl_location.setValue(data.path.toString)
    kvl_validationState.setValue(data.validationState.toString)
  }
}

