package view

import java.nio.charset.StandardCharsets
import java.time.format.{DateTimeFormatter, FormatStyle}
import java.util.Locale
import javafx.beans.binding.Bindings
import javafx.scene.Node
import javafx.scene.control._
import javafx.scene.layout._

import domain._
import domain.products.GamingProduct.GamingProductId
import domain.products.ejs.{EjsBet, EjsGamingProductOrder}
import domain.products.ems.{EmsBet, EmsGamingProductOrder}
import domain.products.gls.{GlsBet, GlsGamingProductOrder}
import domain.products.glss.{GlsSBet, GlsSGamingProductOrder}
import domain.products.s6.{S6Bet, S6GamingProductOrder}
import domain.products.s77.{S77Bet, S77GamingProductOrder}
import domain.products.{Bet, GamingProductOrder}
import model._
import play.api.libs.json.Json
import view.CssClass.Color
import view.DetailView._
import view.JfxImplicits._
import view.controls.TextAreaWithAutoHeight
import view.impl.StructureElements.{EmptyPanelHint, SectionSeparator, VSpacer, ViewHeader}

import scala.collection.JavaConversions._
import scala.util.Try
import scalafx.Includes._
import scalafx.scene.control.{Label, TextField}


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
    private val kvl_meta_retailCustomer = new KeyValuePair("Retail customer", cssClass = "metadata")
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
        kvl_meta_retailCustomer,
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
      kvl_meta_retailCustomer setValue data.doc.metaData.retailCustomer
      kvl_meta_retailerHref setValue data.doc.metaData.retailerHref
      kvl_meta_retailerOrderReference setValue data.doc.metaData.retailerOrderReference

      ordersGroup.getChildren.clear()

      data.doc.gamingProductOrders.foreach {
        case (productId, productOrder) =>
          val productLabel = new Label(s"\u2022 ${productId.toString}")
          productLabel.getStyleClass += "producturl"
          ordersGroup.getChildren.add(productLabel)
          val orderPane : OrderPane[_ <: GamingProductOrder] = productOrder match {
            case order : EjsGamingProductOrder => 
              new EjsOrderPane().init().setOrder(order)
            case order : EmsGamingProductOrder =>
              new EmsOrderPane().init().setOrder(order)
            case order : GlsGamingProductOrder =>
              new GlsOrderPane().init().setOrder(order)
            case order : GlsSGamingProductOrder =>
              new GlsSOrderPane().init().setOrder(order)
            case order : S6GamingProductOrder =>
              new S6OrderPane().init().setOrder(order)
            case order : S77GamingProductOrder =>
              new S77OrderPane().init().setOrder(order)
              
            case _ =>
              new DefaultOrderPane().init().setOrder(productOrder)
          }
          ordersGroup.getChildren.add(orderPane)
      }
    }
  }

  
  trait OrderPane[O <: GamingProductOrder] extends Pane {
    def setOrder(order: O): this.type
    def init(): this.type = this
  }

  
  trait TableViewOrderPaneBase[O <: GamingProductOrder, T <: Bet] extends Pane with OrderPane[O] {
    protected val tableViewBets = new TableView[T]

    locally {
      getStyleClass += "table-view-order-pane"

      tableViewBets.getStyleClass += "bets"

      val heightBinding = tableViewBets.fixedCellSizeProperty().multiply(Bindings.size(tableViewBets.getItems()).add(1.16))
      tableViewBets.prefHeightProperty().bind(heightBinding)
      tableViewBets.minHeightProperty().bind(heightBinding)
      tableViewBets.maxHeightProperty().bind(heightBinding)

      tableViewBets.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY)

      createTableColumns()

      tableViewBets.getColumns.foreach{ col =>
        col.setResizable(false)
        col.impl_setReorderable(false)
        col.setSortable(false)
      }

      getChildren.addAll(
        new SectionSeparator("Bets", level = 3),
        tableViewBets
      )
    }

    override def init(): this.type = {      
      // add participation-pool widgets delivered by concrete implementations if so:
      createParticipationPoolWidgets().foreach{ poolWidgetsParent =>
        getChildren.addAll(
          new VSpacer(),
          new SectionSeparator("Participation pools", level = 3),
          poolWidgetsParent,
          new VSpacer()
        )        
      }
      this
    }

    /** Factory-method to create implementation-specific TableColumns.*/
    protected def createTableColumns(): Unit
    
    /** Factory-method to create implementation-specific widgets.*/
    protected def createParticipationPoolWidgets(): Option[Node]
  }
  

  class PoolSpecsPaneOneDay extends VBox {
    val kvl_firstDate = new KeyValuePair("First date", level=3, cssClass = "partpools")
    val kvl_drawCount = new KeyValuePair("Draw count", level=3, cssClass = "partpools")
    getChildren.addAll(kvl_firstDate, kvl_drawCount)
  }

  
  class PoolSpecsPaneMultiplDays extends VBox {
    val kvl_firstDate = new KeyValuePair("First date", level=3, cssClass = "partpools")
    val kvl_drawCount = new KeyValuePair("Draw count", level=3, cssClass = "partpools")
    val kvl_drawDays = new KeyValuePair("Draw days", level=3, cssClass = "partpools")
    getChildren.addAll(kvl_firstDate, kvl_drawCount, kvl_drawDays)
  }

  
  class EjsOrderPane extends VBox with TableViewOrderPaneBase[EjsGamingProductOrder, EjsBet]{
    private val poolSpecsPane = new PoolSpecsPaneOneDay

    getStyleClass += "ejs"
    
    override protected def createTableColumns(): Unit = {
      tableViewBets.getColumns += new TableColumn[EjsBet, String] {
        getStyleClass += "numbers"
        setText("Numbers")
        this.addCellValuePojoSource(cdf => cdf.getValue.numbers.toSeq.sorted.mkString(", "))
      }
      tableViewBets.getColumns += new TableColumn[EjsBet, String] {
        getStyleClass += "extranumbers"
        setText("Euronumbers")
        this.addCellValuePojoSource(cdf => cdf.getValue.euroNumbers.toSeq.sorted.mkString(", "))
      }
    }

    protected def createParticipationPoolWidgets(): Option[Node] = Some(poolSpecsPane)

    def setOrder(order: EjsGamingProductOrder): this.type = {
      tableViewBets.getItems.setAll(order.bets: _*)
      poolSpecsPane.kvl_firstDate.setValue(dateFormat.format(order.participationPools.firstDate))
      poolSpecsPane.kvl_drawCount.setValue(order.participationPools.drawCount.toString)
      this
    }
  }
 
  
  class EmsOrderPane extends VBox with TableViewOrderPaneBase[EmsGamingProductOrder, EmsBet]{
    val poolSpecsPane = new PoolSpecsPaneMultiplDays

    getStyleClass += "ems"
    
    override protected def createTableColumns(): Unit = {
      tableViewBets.getColumns.add(new TableColumn[EmsBet, String] {
        getStyleClass += "numbers"
        setText("Numbers")
        this.addCellValuePojoSource(cdf => cdf.getValue.numbers.toSeq.sorted.mkString(", "))
      })
      tableViewBets.getColumns += new TableColumn[EmsBet, String] {
        getStyleClass += "extranumbers"
        setText("Starnumbers")
        this.addCellValuePojoSource(cdf => cdf.getValue.starnumbers.toSeq.sorted.mkString(", "))
      }
    }

    override protected def createParticipationPoolWidgets(): Option[Node] = Some(poolSpecsPane)
    
    override def setOrder(order: EmsGamingProductOrder): this.type = {
      tableViewBets.getItems.setAll(order.bets: _*)
      poolSpecsPane.kvl_firstDate.setValue(dateFormat.format(order.participationPools.firstDate))
      poolSpecsPane.kvl_drawCount.setValue(order.participationPools.drawCount.toString)
      poolSpecsPane.kvl_drawDays.setValue(order.participationPools.drawDays.mkString(", ").toString)
      this
    }
  }
  

  class GlsOrderPane extends VBox with TableViewOrderPaneBase[GlsGamingProductOrder, GlsBet]{
    val poolSpecsPane = new PoolSpecsPaneMultiplDays

    getStyleClass += "gls"
    
    override protected def createTableColumns(): Unit = {
      tableViewBets.getColumns.add(new TableColumn[GlsBet, String] {
        getStyleClass += "numbers"
        setText("Numbers")
        this.addCellValuePojoSource(cdf => cdf.getValue.numbers.toSeq.sorted.mkString(", "))
      })
      tableViewBets.getColumns += new TableColumn[GlsBet, String] {
        getStyleClass += "extranumbers"
        setText("Supernumbers")
        this.addCellValuePojoSource(cdf => cdf.getValue.supernumber.toString)
      }
    }

    override protected def createParticipationPoolWidgets(): Option[Node] = Some(poolSpecsPane)
    
    override def setOrder(order: GlsGamingProductOrder): this.type = {
      tableViewBets.getItems.setAll(order.bets: _*)
      poolSpecsPane.kvl_firstDate.setValue(dateFormat.format(order.participationPools.firstDate))
      poolSpecsPane.kvl_drawCount.setValue(order.participationPools.weeks.toString)
      poolSpecsPane.kvl_drawDays.setValue(order.participationPools.drawDays.mkString(", ").toString)
      this
    }
  }
  

  class GlsSOrderPane extends VBox with TableViewOrderPaneBase[GlsSGamingProductOrder, GlsSBet]{
    val poolSpecsPane = new PoolSpecsPaneOneDay

    getStyleClass += "glss"
    
    override protected def createTableColumns(): Unit = {
      tableViewBets.getColumns.add(new TableColumn[GlsSBet, String] {
        getStyleClass += "numbers"
        setText("Numbers")
        this.addCellValuePojoSource(cdf => cdf.getValue.numbers.toSeq.sorted.mkString(", "))
      })
    }

    override protected def createParticipationPoolWidgets(): Option[Node] = Some(poolSpecsPane)
    
    override def setOrder(order: GlsSGamingProductOrder): this.type = {
      tableViewBets.getItems.setAll(order.bets: _*)
      poolSpecsPane.kvl_firstDate.setValue(dateFormat.format(order.participationPools.firstDate))
      poolSpecsPane.kvl_drawCount.setValue(order.participationPools.drawCount.toString)
      this
    }
  }

  
  class S6OrderPane extends VBox with TableViewOrderPaneBase[S6GamingProductOrder, S6Bet]{
    val poolSpecsPane = new PoolSpecsPaneMultiplDays

    getStyleClass += "s6"
    
    override protected def createTableColumns(): Unit = {
      tableViewBets.getColumns.add(new TableColumn[S6Bet, String] {
        getStyleClass += "numbers"
        setText("Numbers")
        this.addCellValuePojoSource(cdf => cdf.getValue.numbers.toSeq.sorted.mkString(", "))
      })
    }

    override protected def createParticipationPoolWidgets(): Option[Node] = Some(poolSpecsPane)
    
    override def setOrder(order: S6GamingProductOrder): this.type = {
      tableViewBets.getItems.setAll(order.bets: _*)
      poolSpecsPane.kvl_firstDate.setValue(dateFormat.format(order.participationPools.firstDate))
      poolSpecsPane.kvl_drawCount.setValue(order.participationPools.drawCount.toString)
      poolSpecsPane.kvl_drawDays.setValue(order.participationPools.drawDays.mkString(", ").toString)
      this
    }
  }

  
  class S77OrderPane extends VBox with TableViewOrderPaneBase[S77GamingProductOrder, S77Bet]{
    val poolSpecsPane = new PoolSpecsPaneMultiplDays

    getStyleClass += "s77"
    
    override protected def createTableColumns(): Unit = {
      tableViewBets.getColumns.add(new TableColumn[S77Bet, String] {
        getStyleClass += "numbers"
        setText("Numbers")
        this.addCellValuePojoSource(cdf => cdf.getValue.numbers.toSeq.sorted.mkString(", "))
      })
    }

    override protected def createParticipationPoolWidgets(): Option[Node] = Some(poolSpecsPane)
    
    override def setOrder(order: S77GamingProductOrder): this.type = {
      tableViewBets.getItems.setAll(order.bets: _*)
      poolSpecsPane.kvl_firstDate.setValue(dateFormat.format(order.participationPools.firstDate))
      poolSpecsPane.kvl_drawCount.setValue(order.participationPools.drawCount.toString)
      poolSpecsPane.kvl_drawDays.setValue(order.participationPools.drawDays.mkString(", ").toString)
      this
    }
  }

  /** Displays the raw data of a `GamingProductOrder`. */
  class DefaultOrderPane extends VBox with OrderPane[GamingProductOrder]{
    val taOrderRawData = new TextAreaWithAutoHeight
    locally{
      getStyleClass.add("default-order-pane")
      taOrderRawData.setEditable(false)
      getChildren.addAll(taOrderRawData)
    }

    def setOrder(order: GamingProductOrder): this.type = {
      taOrderRawData.setText(Json.prettyPrint(order.json))
      this
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
        this.addCellValuePojoSource(_.getValue.productId)
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

    private val kvl_ordersCount = new KeyValuePair("Orders count", simpleLabel = true)

    private val kvl_archiveFile = new KeyValuePair("Archive file")
    private val kvl_extractedToDir = new KeyValuePair("Archive directory")

    private val kvl_poolMetadata_productId = new KeyValuePair("Product Id")
    private val kvl_poolMetadata_poolId = new KeyValuePair("Pool Id")
    private val kvl_poolMetadata_drawDate = new KeyValuePair("drawDate")

    private val kvl_poolMetadata_poolDigestBase64 = new KeyValuePair("Digest-data")
    private val kvl_poolMetadata_poolDigestAlgorithm = new KeyValuePair("Algorithm")
    private val kvl_poolDigestTimestamp = new KeyValuePair("Pool digest timestamp", isMultiLineContent=true)

    private val kvl_validationState = new KeyValuePair("Validation state", simpleLabel = true)
    private val kvl_totalOrdersCount = new KeyValuePair("Total orders count", simpleLabel = true)
    private val kvl_validOrdersCount = new KeyValuePair("Valid orders count", simpleLabel = true)
    private val kvl_invalidCount = new KeyValuePair("Invalid orders count", simpleLabel = true)

    private var betsCountPerProduct_old : Option[Map[GamingProductId, Int]] = null

    override val title : String = "Pool archive details"

    locally {
      getStyleClass.addAll("detail-pane", "archive-detail-pane")
      getChildren.addAll(
        new SectionSeparator("Orders count"),
        kvl_ordersCount,
        grpBetsCountPerProduct,
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
    }


    override def setData(data: ArchiveDetailData): Unit = {

      kvl_ordersCount.setValue(data.orderStats.totalOrdersCount.toString)

      if( (betsCountPerProduct_old == null) || ! (data.orderStats.betsCountPerProduct eq betsCountPerProduct_old)) {

        betsCountPerProduct_old = data.orderStats.betsCountPerProduct

        grpBetsCountPerProduct.getChildren.clear()

        val k: Option[Map[GamingProductId, Int]] = data.orderStats.betsCountPerProduct

        data.orderStats.betsCountPerProduct match {
          case Some(betsCountPerProduct) =>
            betsCountPerProduct.foreach { case (productId, betsCount) =>
              val kvl_betsCountForProduct = new KeyValuePair(s"# ${productId.toString}-Bets", simpleLabel = true)
              kvl_betsCountForProduct.getStyleClass.add("betscountperproduct")
              kvl_betsCountForProduct.setValue(betsCount.toString)
              grpBetsCountPerProduct.getChildren.add(kvl_betsCountForProduct)
            }
          case _ =>
            val kvl_betsCountForProduct = new KeyValuePair(s"Bets count per product", simpleLabel = true)
            kvl_betsCountForProduct.getStyleClass.addAll("betscountperproduct")
            kvl_betsCountForProduct.setValue("Not yet available")
            grpBetsCountPerProduct.getChildren.add(kvl_betsCountForProduct)
        }
      }

      kvl_archiveFile.setValue(data.poolSource.path.toString)
      kvl_extractedToDir.setValue(data.extractedToDir.toString)

      kvl_poolMetadata_productId.setValue(data.poolMetadata.productId)
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
      kvl_totalOrdersCount.setValue(data.totalOrdersCount.toString)
      kvl_validOrdersCount.setValue(data.validOrdersCount.toString)
      kvl_invalidCount.setValue(data.invalidOrdersCount.toString)
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

