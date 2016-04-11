package view

import java.nio.charset.StandardCharsets
import java.time.format.{DateTimeFormatter, FormatStyle}
import java.util.Locale
import javafx.beans.binding.Bindings
import javafx.geometry.Orientation
import javafx.scene.{Node, Parent}
import javafx.scene.control._
import javafx.scene.layout._

import domain._
import domain.products.GamingProduct.GamingProductId
import domain.products.GamingProductOrder
import domain.products.ejs.{EjsBet, EjsGamingProductOrder}
import model.{OrderDirectoryDetailData, ArchiveDetailData, OrderDocDetailData, DetailData}
import play.api.libs.json.{JsObject, Json}
import view.CssClass.Color
import view.DetailView._
import view.JfxImplicits._
import view.controls.TextAreaWithAutoHeight
import view.impl.StructureElements.{ViewHeader, EmptyPanelHint, VSpacer, SectionSeparator}

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
  private val orderDetailPane = new OrderDetailPane
  private val orderResultDetailPane = new OrderResultDetailPane
  private val orderResultSignatureDetailPane = new OrderResultSignatureDetailPane
  private val orderResultSignatureTimestampDetailPane = new OrderResultSignatureTimestampDetailPane
  private val orderSignatureDetailPane = new OrderSignatureDetailPane
  private val archiveDetailPane = new ArchiveDetailPane
  private val orderDirDetailPane = new OrderDirDetailPane

  locally {

    orderDocDetailViewHolder.getChildren.setAll(
      orderDetailPane,
      orderResultDetailPane,
      orderResultSignatureDetailPane,
      orderResultSignatureTimestampDetailPane,
      orderSignatureDetailPane,
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
            case data: ArchiveDetailData => archiveDetailPane
            case data : OrderDirectoryDetailData => orderDirDetailPane
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

      getStyleClass += "detail-pane"

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
          productOrder match {
            case order: EjsGamingProductOrder =>
              val v = new EjsOrderPane
              v.setOrder(order)
              ordersGroup.getChildren.add(v)
            case _ =>
              val v = new GenericOrderPane
              v.setOrder(productOrder, data.doc.rawData)
              ordersGroup.getChildren.add(v)
          }
      }
    }
  }

  class EjsOrderPane extends VBox{
    val tableViewBets = new TableView[EjsBet]

    val dateFormat = DateTimeFormatter.ofLocalizedDate(FormatStyle.MEDIUM)

    private val kvl_firstDate = new KeyValuePair("First date", level=3, cssClass = "partpools")
    private val kvl_drawCount = new KeyValuePair("Draw count", level=3, cssClass = "partpools")

    locally {
      getStyleClass += "ejs-order-pane"

      tableViewBets.getStyleClass += "bets"

      val heightBinding = tableViewBets.fixedCellSizeProperty().multiply(Bindings.size(tableViewBets.getItems()).add(1.16))
      tableViewBets.prefHeightProperty().bind(heightBinding)
      tableViewBets.minHeightProperty().bind(heightBinding)
      tableViewBets.maxHeightProperty().bind(heightBinding)

      tableViewBets.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY)

      tableViewBets.getColumns += new TableColumn[EjsBet, String] {
        getStyleClass += "numbers"
        setText("Numbers")
        this.addCellValuePojoSource(cdf => cdf.getValue.numbers.toSeq.sorted.mkString(", "))
      }

      tableViewBets.getColumns += new TableColumn[EjsBet, String] {
        getStyleClass += "euronumbers"
        setText("Euronumbers")
        this.addCellValuePojoSource(cdf => cdf.getValue.euroNumbers.toSeq.sorted.mkString(", "))
      }

      tableViewBets.getColumns.foreach{ col =>
        col.setResizable(false)
        col.impl_setReorderable(false)
        col.setSortable(false)
      }

      getChildren.addAll(
        new SectionSeparator("Bets", level = 3),
        tableViewBets,
        new VSpacer(),
        new SectionSeparator("Participation pools", level = 3),
        kvl_firstDate,
        kvl_drawCount,
        new VSpacer()
      )
    }

    def setOrder(order: EjsGamingProductOrder): Unit = {
      tableViewBets.getItems.setAll(order.bets: _*)
      kvl_firstDate.setValue(dateFormat.format(order.participationPools.firstDate))
      kvl_drawCount.setValue(order.participationPools.drawCount.toString)
    }
  }

  class GenericOrderPane extends VBox {
    val taOrderRawData = new TextAreaWithAutoHeight
    locally{
      getStyleClass.add("generic-order-pane")
      taOrderRawData.setEditable(false)
      getChildren.addAll(taOrderRawData)
    }

    def setOrder(order: GamingProductOrder, orderRawData : IndexedSeq[Byte]): Unit = {
      val json = Json.parse(orderRawData.toArray)
      val json_pretty = Json.prettyPrint( (json \ "gaming-product-orders" \ order.productURI.toString ).as[JsObject])
      taOrderRawData.setText(json_pretty)
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

