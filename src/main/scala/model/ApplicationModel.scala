package model

import java.io.File
import java.nio.file.{Files, Path}
import java.security.cert.X509Certificate
import java.time._
import java.util.concurrent.{CompletionException, CancellationException, Executor}
import java.util.function.Predicate
import java.util.{Collections, Comparator, UUID}
import javafx.beans.property._
import javafx.collections.{FXCollections, ObservableList}
import javafx.scene.control.TreeItem

import _root_.util.Task.{CancellableMappingAI, CancellableSupplierAI}
import _root_.util.{Task, TaskImpl, Utils}
import domain.PoolResource.Filenames
import domain.PoolValidator._
import domain._
import domain.products.GamingProduct
import domain.products.GamingProduct.GamingProductId
import model.ApplicationModel._
import model.NavigatorTreeItem._
import model.OrderDirNavigatorItem.ProductInfos
import model.base.EventHandlerRegistry
import org.apache.commons.io.FileUtils
import org.slf4j.LoggerFactory
import util.Utils._

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}
import scalafx.Includes._

/**
  * @param runLaterExecutor The UI must be updated from the JavaFX Application thread. Since some operations are performed
  *                         concurrently, their result have to be set to the `AppliationModel` via `Platform.runLater()`.
  *                         Since the call is not suitable for Unit-test the run-later functionality must be pluggable
  *                         which is achieved by `trait RunLater`.
  *                         Important: `runLaterExecutor` must be thread-safe!
  **/
class ApplicationModel(archiveReader : ArchiveReader,
                       resourceProvider: PoolResourceProvider,
                       validator: PoolValidator,
                       settingsManager: ApplicationSettingsManager,
                       private var credentialsManager: CredentialsManager,
                       configFile: File,
                       runLaterExecutor: Executor,
                       asyncTaskExecutor: Executor,
                       implicit val executionContext: ExecutionContext
                      ) {

  private val logger = LoggerFactory.getLogger(getClass)

  //Properties
  private val _poolSourceProp = new SimpleObjectProperty(Option.empty[PoolSource])
  private val _archiveDirProp = new SimpleObjectProperty[Option[File]](None)
  private val _appStateProp = new SimpleObjectProperty[AppState.Value](AppState.Undefined)
  private val _validationStateProp = new SimpleObjectProperty[ValidationState.Value](ValidationState.NotValidated)
  private val _validateArchiveEnabledProp = new SimpleBooleanProperty(false)
  private val _validateSingleOrderEnabledProp = new SimpleBooleanProperty(false)
  private val _navigatorFilterOptions = new SimpleObjectProperty(NavigatorFilterOptions(
    showValidOrders = true,
    showInvalidOrders = true,
    showUnvalidatedOrders = true,
    textFilter = Option.empty[String]
  ))
  private val _showUIDebugControlsProp = new SimpleBooleanProperty(false)
  private val _canOpenArchiveProp = new SimpleBooleanProperty(false)
  private val _selectedNavigatorItemProp = new SimpleObjectProperty[Option[NavigatorItem]](None)
  private val _orderDocDetailDataProp = new SimpleObjectProperty[Option[DetailData]](None)
  private val _backgroundTaskInfoProp = new SimpleObjectProperty[Option[AsyncTask[_]]](None)
  private val _validationViewFilterOptionsProp = new SimpleObjectProperty(
    ValidationViewFilterOptions(showValidOrders = true, showInvalidOrders = true)
  )
  private val _validationViewStateProp = new SimpleObjectProperty(ValidationViewState.NoArchiveLoaded)
  private val _validationViewItemsProp: ObservableList[ValidationViewItem] = {
    FXCollections.observableList(new java.util.LinkedList[ValidationViewItem])
  }
  private val _drawDateInfosProp = new SimpleObjectProperty[Option[DrawDateInfos]](None)
  private[model] val navigatorContentRoot = new TreeItemExt[NavigatorItem](null)
  private val _navigatorContentRootProp = new SimpleObjectProperty(Option.empty[TreeItemExt[NavigatorItem]])

  val navigatorFilterChain = new NavigatorFilterChain

  private var _settings: ApplicationSettings = null
  private val errorEventHandler = new EventHandlerRegistry[Seq[ErrorMsg]]

  private val navItemFileNameComparator = new Comparator[TreeItemExt[NavigatorItem]]() {
    override def compare(o1: TreeItemExt[NavigatorItem], o2: TreeItemExt[NavigatorItem]): Int = {
      o1.getValue.path.toFile.getName.compare(o2.getValue.path.toFile.getName)
    }
  }

  private[model] var archiveExtractionTargetTempDir = Option.empty[File]


  //cached values (needed for performance reasons, obtaining it from navigatorContentRoot.getChildren is too slow..)
  private[model] val _validatedOrders = mutable.HashSet.empty[Path]
  private var _totalOrdersCount = 0
  private var _validatedOrdersCount = 0
  private var _invalidOrdersCount = 0
  private var _validOrdersCount = 0
  private var _cachedArchiveDetailData = Option.empty[ArchiveDetailData]

  private var disposed = false

  private val LineSep = System.getProperty("line.separator", "\n")



  def settings: ApplicationSettings = _settings

  def poolSourceProp: ReadOnlyProperty[Option[PoolSource]] = _poolSourceProp

  def archiveDirProp: ReadOnlyProperty[Option[File]] = _archiveDirProp

  def appStateProp: ReadOnlyProperty[AppState.Value] = _appStateProp

  def validationStateProp: ReadOnlyProperty[ValidationState.Value] = _validationStateProp

  def validateArchiveEnabledProp: ReadOnlyBooleanProperty = _validateArchiveEnabledProp

  def validateSingleOrderEnabledProp: ReadOnlyBooleanProperty = _validateSingleOrderEnabledProp

  def navigatorFilterOptions: ReadOnlyProperty[NavigatorFilterOptions] = _navigatorFilterOptions

  def showUIDebugControlsProp: ReadOnlyBooleanProperty = _showUIDebugControlsProp

  def canOpenArchiveProp: ReadOnlyBooleanProperty = _canOpenArchiveProp

  def selectedNavigatorItemProp: ReadOnlyProperty[Option[NavigatorItem]] = _selectedNavigatorItemProp

  def orderDocDetailDataProp: ReadOnlyProperty[Option[DetailData]] = _orderDocDetailDataProp

  def backgroundTaskInfoProp : ReadOnlyProperty[Option[TaskInfo]] = _backgroundTaskInfoProp.asInstanceOf[SimpleObjectProperty[Option[TaskInfo]]]

  def validationViewFilterOptionsProp: ReadOnlyProperty[ValidationViewFilterOptions] = _validationViewFilterOptionsProp

  def validationViewStateProp: ReadOnlyObjectProperty[ValidationViewState.Value] = _validationViewStateProp

  def validationViewItemsProp: ReadOnlyListProperty[ValidationViewItem] = new ReadOnlyListWrapper(_validationViewItemsProp)

  /** Holds the draw-date from the archive's metainfos (when an archive is loaded) and an optional user-defined draw-date
    * which may override the archive's draw-date in the validation.
    * */
  def drawDateInfosProp: ReadOnlyObjectProperty[Option[DrawDateInfos]] = _drawDateInfosProp

  def navigatorContentRootProp: ReadOnlyProperty[Option[TreeItemExt[NavigatorItem]]] = _navigatorContentRootProp

  def init(): Unit = {
    navigatorContentRoot.setExpanded(true)
    initSettings()
  }

  private def initSettings(): Unit = {
    logger.info(s"loadSettingsFromFile()..using file $configFile")
    settingsManager.loadSettings(configFile.toURI) match {
      case ApplicationSettingsManager.Loaded(s) =>
        _settings = s
        applySettings()
      case ApplicationSettingsManager.LoadFailed(errors) =>
        logger.error(s"initSettings() failed: $LineSep ${errors.mkString(LineSep)}")
        val details = s"${errors.map(x => s"${x.message}: ${x.detail.getOrElse("")}").mkString(LineSep)}"
        _settings = null
        publishErrorEvents(ErrorMsg("Invalid Settings", detail = Some(details)))
    }
    updateCanOpenArchiveProp()
  }

  private def applySettings(): Unit = {
    if (settings.errors.nonEmpty) {
      val details = s"${settings.errors.map(x => s"${x.message}: $LineSep ${x.detail.getOrElse("")}").mkString(LineSep + LineSep)}"
      publishErrorEvents(ErrorMsg("Invalid configuration", detail = Some(details)))
    }

    archiveReader.setTempDir(_settings.archiveExtractionTarget.value.map(_.toPath).orNull)

    initKeysAndCerts()

    _showUIDebugControlsProp.setValue(settings.showUIDebugControls)
  }

  def loadPoolDirectory(directory: File): Future[Unit] = loadPoolSource(PoolSourceDirectory(directory.toPath))

  /**
    * Loads an archiveFile. Info: the loading is performed asynchronously.
    **/
  def loadPoolArchive(archiveFile: File): Future[Unit] = loadPoolSource(PoolSourceArchive(archiveFile.toPath))

  def unload(): Future[Unit] = {
    logger.info(s"unload()..appState: ${appStateProp.getValue}, ${poolSourceProp.getValue}")
    resetModelState()
  }

  private def publishErrorEvents(errors : ErrorMsg*): Unit = {
    errors.foreach{err =>
      err.exception match {
        case Some(t) => logger.error(s"publishErrorEvent(${err}", t)
        case _ => logger.error(s"publishErrorEvent(${err}")
      }
    }
    errorEventHandler.publishEvent(errors)
  }

  private[model] def loadPoolSource(poolSource: PoolSource): Future[Unit] = {
    logger.info(s"loadPoolSource($poolSource)")
    val retValPromise = Promise[Unit]()

    resetModelState().onComplete { resetResult =>
      logger.info("resetModelFuture.onComplete")

      runLater {
        loadPoolSourceImpl(poolSource).onComplete { result =>
          logger.info(s"loadPoolFuture.onComplete: $result => retValPromise.success()")
          retValPromise.success()
        }
      }
    }

    logger.debug(s"loadPoolImpl()..leaving")
    retValPromise.future
  }


  private[model] def loadPoolSourceImpl(poolSource: PoolSource): Future[Unit] = {

    logger.info("loadPoolSourceImpl..start")

    val retValPromise = Promise[Unit]()

    _appStateProp.setValue(AppState.Loading)
    updateValidationViewState()
    _poolSourceProp.setValue(Option(poolSource))

    require(_settings.isValid, s"invalid Settings!\n$settings")

    val createItemsFuture: Task[IndexedSeq[TreeItemExt[NavigatorItem]]] = poolSource match {
      case dirSrc: PoolSourceDirectory =>
        _archiveDirProp.set(Option(dirSrc.path.toFile))
        new TaskImpl[IndexedSeq[TreeItemExt[NavigatorItem]]](info="generateNavItems", new CancellableSupplierAI[IndexedSeq[TreeItemExt[NavigatorItem]]] {
          override def supply(): IndexedSeq[TreeItemExt[NavigatorItem]] = {
            generateNavigatorItems(dirSrc.path.toFile)
          }
        })

      case archiveSrc: PoolSourceArchive =>
        val extractedToDir = Files.createTempDirectory(_settings.archiveExtractionTarget.value.get.toPath,
          archiveSrc.path.toFile.getName).toFile
        archiveExtractionTargetTempDir = Some(extractedToDir)
        extractedToDir.deleteOnExit()
        _archiveDirProp.set(Option(extractedToDir))

        val progressInfoHandler = (info : ArchiveReader.ProgressInfo) => {
          _backgroundTaskInfoProp.getValue match {
            case Some(task@AsyncTask(extractArchiveTaskId, _,_,_,_)) =>
              if(! task.task.isCancelled){
                val progressVal = info.extractedOrdersCount.toDouble / info.totalOrdersCount.toDouble
                runLater {
                  _backgroundTaskInfoProp.setValue(Some(task.copy(progress = progressVal)))
                }
              }
            case _ =>
          }
        }

        val extractArchiveFut = archiveReader.extractArchive(sourceFile = archiveSrc.path.toFile, destDir = extractedToDir, progressInfoHandler)

        setCurrentBackgroundTask(Some(AsyncTask(ApplicationModel.extractArchiveTaskId, extractArchiveFut,
          description="extract pool archive", cancellable = true, progress = 0)))

        val mapping = new CancellableMappingAI[Try[File], IndexedSeq[TreeItemExt[NavigatorItem]]]( (file) => {
          logger.info(s"loadPoolImpl()..extracted ${archiveSrc.path}")
          runLater{
            setCurrentBackgroundTask(None)
          }
          file match {
            case Success(f) =>
              generateNavigatorItems(f)
            case Failure(t) =>
              throw new Exception("extractAsync failed: " + t.getMessage, t)
          }
        })

        val generateTreeItemTask: Task[IndexedSeq[TreeItemExt[NavigatorItem]]] = extractArchiveFut.map(mapping)
        generateTreeItemTask
    }

    createItemsFuture.onComplete{tr =>
      tr match {
        case Success(navTreeItems) =>
          logger.info("loadPoolImpl task.onSuccess")
          runLater {
            updateValidationEnabledProps()
            logger.debug("loadPoolImpl task.onSuccess runLater")

            navigatorContentRoot.setValue(
              ArchiveNavigatorItem(path = archiveDirProp.getValue.get.toPath,
                poolSource = poolSourceProp.getValue.get,
                isValid = None,
                displayName = poolSourceProp.getValue.get.path.toFile.getName,
                validationState = ValidationState.NotValidated)
            )

            logger.info(s"loadPoolImpl()..set orderDir NavTreeItems")

            navigatorContentRoot.filteredChildren_source.setAll(navTreeItems.toIndexedSeq: _*)
            logger.info(s"loadPoolImpl()..set orderDir NavTreeItems..finished")
            _appStateProp.setValue(AppState.Loaded)
            updateValidationViewState()
            _navigatorContentRootProp.setValue(Some(navigatorContentRoot))
          }
          logger.info("retValPromise.success()")
          retValPromise.success()

        case Failure(t) =>
          logger.error(s"loadPoolImpl task.onFailure: ${t.getMessage}")
          runLater {
            logger.error(s"loadPoolImpl task.onFailure: ${t.getMessage} runLater")
            setCurrentBackgroundTask(None)
            updateValidationEnabledProps()
            resetModelState()
            updateValidationViewState()
            val typeStr = poolSource match {
              case s : PoolSourceArchive => "pool-archive"
              case _ => "pool-directory"
            }

            if(! t.isInstanceOf[CancellationException]){
              publishErrorEvents(ErrorMsg(s"Failed to load $typeStr ${poolSource.path.toString}", detail=Some(t.getMessage), exception = Some(t)))
            }

          }
          logger.info("retValPromise.failure()")
          retValPromise.failure(t)
      }
    }

    createItemsFuture.run()

    retValPromise.future
  }

  private def generateNavigatorItems(poolArchiveDir: File): IndexedSeq[TreeItemExt[NavigatorItem]] = {
    logger.info(s"generateNavigatorItems()")

    resourceProvider.getPoolMetadata(poolDirPath = poolArchiveDir.toPath) match {
      case Success(metaInfo) =>
        val drawInfo = DrawDateInfos(archiveDrawDate = metaInfo.drawDate, customDrawDate = None)
        runLater {
          _drawDateInfosProp.setValue(Some(drawInfo))
        }
      case Failure(f) =>
        runLater {
          publishErrorEvents(ErrorMsg(s"Error loading MetaInfos", detail = Some(f.getMessage), exception = Some(f)))
        }
    }

    val orderDirsTry = resourceProvider.getOrderDirPaths(poolArchiveDir.toPath)
    _totalOrdersCount = orderDirsTry.toOption.map(_.size).getOrElse(0)

    orderDirsTry.map{ orderDirs =>
      val result = orderDirs.sortBy(_.getFileName.toString).par.map {
        orderDirPath =>

          if (disposed)
            return IndexedSeq.empty

          //the order-file must be read to add some information to the tree-items (e.g. bets-per-product, retailerOrderReference for searching)
          val order = resourceProvider.getOrder(orderDirPath)
          if(order.isFailure)
            logger.error(s"failed to load order in $orderDirPath", order.failed.get)

          val productInfos = order.toOption.map(createProductInfosFor)

          val navItem = OrderDirNavigatorItem(orderDirPath,
            displayName=orderDirPath.getFileName.toString,
            retailerOrderReference = order.map(_.metaData.retailerOrderReference).getOrElse(""),
            validationState = ValidationState.NotValidated,
            hasInvalidOrderDocs = false,
            productInfos = productInfos.orNull)
          val orderDirItem = new TreeItemExt[NavigatorItem](navItem)

          resourceProvider.getOrderDocPaths(orderDirPath).toOption.foreach{ orderDocFiles =>
            val orderDocItems = orderDocFiles.toSeq.map {
              docFilePath =>
                val docItem = OrderDocNavigatorItem(docFilePath, docFilePath.getFileName.toString)
                new TreeItemExt[NavigatorItem](docItem)
            }
            orderDirItem.filteredChildren_source.setAll(orderDocItems.asInstanceOf[Seq[TreeItemExt[NavigatorItem]]]: _*)
          }
          orderDirItem
      }
      logger.info(s"generateNavigatorItems()..finished")
      result.seq.toIndexedSeq
    }.toOption.getOrElse(IndexedSeq.empty)
  }

  private def createProductInfosFor(order: Order): ProductInfos = {
    val betsCountPerProduct = order.gamingProductOrders.values.map { productOrder =>
      val productId = GamingProduct.gamingProductIdFromURI(productOrder.productURI)
      (productId, productOrder.bets.size)
    }.toMap
    ProductInfos(betCountPerProduct = betsCountPerProduct)
  }

  private def resetModelState(): Future[Unit] = {
    _totalOrdersCount = 0
    resetCachedArchiveData()

    _drawDateInfosProp.setValue(None)
    _appStateProp.setValue(AppState.Undefined)
    _poolSourceProp.setValue(None)
    _archiveDirProp.set(None)
    _navigatorContentRootProp.setValue(None)
    navigatorContentRoot.setValue(null)
    navigatorContentRoot.filteredChildren_source.clear()
    _selectedNavigatorItemProp.setValue(None)
    _orderDocDetailDataProp.setValue(None)
    _validationStateProp.setValue(ValidationState.NotValidated)
    _validationViewStateProp.setValue(ValidationViewState.NoArchiveLoaded)

    updateValidationEnabledProps()
    _validationViewItemsProp.clear()

    Future {
      archiveExtractionTargetTempDir.foreach { archiveTempDir =>
        deleteArchiveTargetTmpDir(isAsync = true)
      }
    }
  }

  def setValidationViewFilterOptions(filterProps: ValidationViewFilterOptions): Unit = {
    logger.info(s"setValidationViewFilterOptions($filterProps)")
    _validationViewFilterOptionsProp.setValue(filterProps)
    updateValidationViewState()
    updateValidationViewItems()
  }

  def setSelectedNavigatorItem(selected: Option[NavigatorItem]): Unit = {
    logger.info(s"setSelectedNavigatorItem($selected)")
    _selectedNavigatorItemProp.value = selected
    updateSelectedNavigatorItem()
    updateDetailData()
    updateValidationEnabledProps()
    updateValidationViewItems()
  }

  private def setNavigatorFilterOptions(options: NavigatorFilterOptions): Unit = {
    _navigatorFilterOptions.setValue(options)
    navigatorContentRoot.setPredicate(navigatorFilterChain
      .setHideValidOrdersFilter(!options.showValidOrders)
      .setHideInvalidOrdersFilter(!options.showInvalidOrders)
      .setHideUnvalidatedOrdersFilter(!options.showUnvalidatedOrders)
      .withTextFilter(options.textFilter)
    )
    updateValidationViewItems()
  }

  def setShowValidOrders(show: Boolean): Unit = {
    setNavigatorFilterOptions(navigatorFilterOptions.getValue.copy(showValidOrders = show))
  }

  def setNavigatorFilterText(filter: Option[String]): Unit = {
    setNavigatorFilterOptions(navigatorFilterOptions.getValue.copy(textFilter = filter))
  }

  def setShowInvalidOrders(show: Boolean): Unit = {
    setNavigatorFilterOptions(navigatorFilterOptions.getValue.copy(showInvalidOrders = show))
  }

  def setShowUnvalidatedOrders(show: Boolean): Unit = {
    setNavigatorFilterOptions(navigatorFilterOptions.getValue.copy(showUnvalidatedOrders = show))
  }

  def setCustomDrawDate(value: Option[ZonedDateTime]): Unit = {
    logger.info(s"setCustomDrawDate($value)")
    drawDateInfosProp.getValue match {
      case Some(oldVal) =>
        val newVal = {
          val tmp = oldVal.copy(customDrawDate = value)
          (Option(tmp.archiveDrawDate) == tmp.customDrawDate) match {
            case true => tmp.copy(customDrawDate = None)
            case false => tmp
          }
        }
        _drawDateInfosProp.setValue(Some(newVal))
        resetValidationResults()
      case _ => publishErrorEvents(ErrorMsg(s"Cannot set user-defined draw-date in the current state"))
    }
  }

  /**
    * Delivers the drawTime for validation, no matter if it is the drawtime from `metadata.json` or a user-defined one.
    * */
  private def getDrawTime : Try[Instant] = {
    drawDateInfosProp.getValue match {
      case Some(drawDateInfo) =>
        Success(drawDateInfo.customDrawDate.getOrElse(drawDateInfo.archiveDrawDate).toInstant)
      case _ => Failure(new Exception("drawDateInfosProp is undefined"))
    }
  }

  private def updateValidationState(): Unit = {

    val totalCount = getTotalOrdersCount

    val validationState = getValidatedOrdersCount match {
      case 0 => ValidationState.NotValidated
      case x if x < totalCount => ValidationState.PartiallyValidated
      case `totalCount` => ValidationState.CompletelyValidated
      case x => sys.error(s"unexpected getValidatedOrdersCount:$x")
    }

    _validationStateProp.setValue(validationState)

    val newArchiveItem: NavigatorItem = navigatorContentRoot.getValue match {
      case archiveItem: ArchiveNavigatorItem =>
        val archiveValid = validationState match {
          case ValidationState.NotValidated => None
          case ValidationState.PartiallyValidated =>
            if ((getInvalidOrdersCount > 0) || hasPoolValidationErrors) Some(false) else None
          case ValidationState.CompletelyValidated =>
            Some( (getTotalOrdersCount == getValidOrdersCount) && ! hasPoolValidationErrors)
        }
        archiveItem.copy(validationState = validationState, isValid = archiveValid)
      case x =>
        sys.error(s"unexpected navigatorContentRoot.getValue: ${navigatorContentRoot.getValue}")
    }

    navigatorContentRoot.setValue(newArchiveItem)
  }

  /**
    * The selected item is a snapshot of a certain TreeItemExt.value it has to be updated
    * when `navigatorContentRoot` or any of its children is updated.
    * */
  private def updateSelectedNavigatorItem(): Unit = {

    // find the navTreeItem for the the currently selected NavigatorItem
    val navTreeItem : Option[TreeItemExt[_ <: NavigatorItem]] = selectedNavigatorItemProp.getValue match {
      case Some(selectedItem : ArchiveNavigatorItem) =>
        Option(navigatorContentRoot)
      case Some(selectedItem : OrderDirNavigatorItem) =>
        findOrderDirNavTreeItem(selectedItem.path)
      case Some(selectedItem : OrderDocNavigatorItem) =>
        val orderDirPath = selectedItem.path.getParent
        findOrderDirNavTreeItem(orderDirPath) match{
          case Some(orderDirTreeItem) =>
            val tmp = orderDirTreeItem.filteredChildren_source.find(_.getValue.path == selectedItem.path)
            tmp
          case _ => None
        }
      case _ => None
    }
    _selectedNavigatorItemProp.setValue(navTreeItem.map(_.getValue))
  }

  private def createArchiveDetailData(): Try[ArchiveDetailData] = {

    archiveDirProp.getValue.map{ archiveDir =>
      resourceProvider.getPoolMetadata(archiveDir.toPath).map { poolMetaInfos =>
        val productInfos = navigatorContentRoot.filteredChildren_source.collect {
          case NavigatorTreeItem(orderDirNavItem: OrderDirNavigatorItem) => orderDirNavItem
        }.map(_.productInfos)

        var betsCountPerProduct = Map.empty[GamingProductId, Int]

        productInfos.foreach { pi =>
          pi.betCountPerProduct.foreach { case (productId, betsCount) =>
            val newCount = betsCountPerProduct.get(productId).getOrElse(0) + betsCount
            betsCountPerProduct = betsCountPerProduct.updated(productId, newCount)
          }
        }

        val orderStats = ArchiveDetailData.OrderStats(totalOrdersCount = getTotalOrdersCount,
          betsCountPerProduct = Some(betsCountPerProduct))

        val poolDigestTimestamp = resourceProvider.getPoolDigestTimestamp(archiveDir.toPath).map(_.rawData)

        ArchiveDetailData(poolSource = poolSourceProp.getValue.get,
          poolMetadata = poolMetaInfos,
          poolDigestTimestamp = poolDigestTimestamp.toOption,
          orderStats = orderStats,
          extractedToDir = archiveDirProp.value.get.toPath,
          validationState = validationStateProp.getValue,
          totalOrdersCount = getTotalOrdersCount,
          validatedOrdersCount = getValidatedOrdersCount,
          validOrdersCount = getValidOrdersCount,
          invalidOrdersCount = getInvalidOrdersCount
        )
      }
    }.getOrElse(Failure(new Exception("archiveDirProp.getValue is undefined")))
  }

  private def updateDetailData(): Unit = {

    val detailData: Option[DetailData] = selectedNavigatorItemProp.getValue match {
      case Some(sel: ArchiveNavigatorItem) =>

        val archiveDetailData = if(_cachedArchiveDetailData.nonEmpty) _cachedArchiveDetailData else {
          _cachedArchiveDetailData = createArchiveDetailData().toOption
          _cachedArchiveDetailData
        }

        archiveDetailData.map{oldArchiveDetailData =>
          oldArchiveDetailData.copy(
            validationState = validationStateProp.getValue,
            totalOrdersCount = getTotalOrdersCount,
            validatedOrdersCount = getValidatedOrdersCount,
            validOrdersCount = getValidOrdersCount,
            invalidOrdersCount = getInvalidOrdersCount
          )
        }

      case Some(sel: OrderDirNavigatorItem) =>
        Option(OrderDirectoryDetailData(path = sel.path,
          orderId = sel.path.getFileName.toString,
          validationState = sel.validationState,
          hasInvalidOrderDocs = sel.hasInvalidOrderDocs,
          validationResults = sel.validationResults,
          productInfos = sel.productInfos))

      case Some(sel: OrderDocNavigatorItem) =>
        
        val detailDataOrError: Either[DetailData, ErrorMsg] = sel.path.toFile.getName match {
          case Filenames.Order =>
            resourceProvider.getOrder(sel.path.getParent) match {
              case Success(data) => Left(OrderDocDetailData(data))
              case Failure(t) => Right(ErrorMsg("Failed to load order detail data", Some(t.getMessage)))
            }
          case Filenames.OrderResult =>
            resourceProvider.getOrderResult(sel.path.getParent) match {
              case Success(data) => Left(OrderDocDetailData(data))
              case Failure(t) => Right(ErrorMsg("Failed to load order.result detail data", Some(t.getMessage)))
            }
          case Filenames.OrderResultSignature =>
            resourceProvider.getOrderResultSignature(sel.path.getParent) match {
              case Success(data) => Left(OrderDocDetailData(data))
              case Failure(t) => Right(ErrorMsg("Failed to load order.result.signature detail data", Some(t.getMessage)))
            }
          case Filenames.OrderResultSignatureTimestamp =>
            resourceProvider.getOrderResultSignatureTimestamp(sel.path.getParent) match {
              case Success(data) => Left(OrderDocDetailData(data))
              case Failure(t) => Right(ErrorMsg("Failed to load order.result.signature.timestamp detail data", Some(t.getMessage)))
            }
          case Filenames.OrderSignature =>
            resourceProvider.getOrderSignature(sel.path.getParent) match {
              case Success(data) => Left(OrderDocDetailData(data))
              case Failure(t) => Right(ErrorMsg("Failed to load order.signature detail data", Some(t.getMessage)))
            }
          case Filenames.OrderMetadata =>
            val order = resourceProvider.getOrder(sel.path.getParent)
            val orderMetadata = resourceProvider.getOrderMetadata(sel.path.getParent)

            val detailData: Try[OrderHedgingDetailData] = for{o <- order; omd <- orderMetadata} yield {
              OrderHedgingDetailData(o, omd.hedgingData, OrderHedgingDetailData.RowData.create(o, omd.hedgingData)              )
            }
            
            detailData match {
              case Success(detailData) => Left(detailData)
              case Failure(f) => 
                Right( ErrorMsg("Failed to load order.metadata details", 
                  Some(Seq(order, orderMetadata).filter(_.isFailure).map(_.failed.get.getMessage).mkString("\n")))
                )
            }
          case f =>
            Right(ErrorMsg(s"unexpected filename: $f"))
        }

        Some(
          detailDataOrError.fold(
            detailData => detailData,
            errorMsg => DetailDataLoadingError(errorMsg)
          )
        )
      case _ => None
    }
    _orderDocDetailDataProp.setValue(detailData)
  }

  private def updateValidationViewItems(): Unit = {

    logger.debug(s"updateValidationViewItems()..size before: ${validationViewItemsProp.getValue.size()}")

    _validationViewStateProp.setValue(ValidationViewState.PreparingData)

    selectedNavigatorItemProp.getValue match {
      case Some(item: ArchiveNavigatorItem) =>
        // show ValidationResults for the whole archive
        findTreeItemForFile(navigatorContentRoot, item.path) match {
          case Some(archiveTreeItem) =>
            val orderValidationItemBuilder = Seq.newBuilder[ValidationViewItem]
            orderValidationItemBuilder.sizeHint(getTotalOrdersCount)

            navigatorContentRoot.getChildren.map { orderTreeItem =>

              val vResults_singleOrder = collectValidationStateItemsForOrder(orderDirTreeItem = orderTreeItem,
                collectErrors = validationViewFilterOptionsProp.getValue.showInvalidOrders,
                collectValidationOk = validationViewFilterOptionsProp.getValue.showValidOrders,
                level = 3)

              if (vResults_singleOrder.nonEmpty) {
                val structuralItem_orderDir = ValidationViewStructuralItem(result = None, PoolResource.PRType.OrderDirectory,
                  name = orderTreeItem.getValue.path.toFile.getName, level = 2)
                orderValidationItemBuilder ++= structuralItem_orderDir +: vResults_singleOrder.toSet.toSeq
              }
            }

            val orderValidationItems = orderValidationItemBuilder.result()

            val structuralItem_pool = ValidationViewStructuralItem(result = None, PoolResource.PRType.PoolDirectory,
              name = archiveTreeItem.getValue.path.toFile.getName, level = 1)

            val poolValidationItems = navigatorContentRoot.getValue.validationResults.map{ checkResult =>
              ValidationViewStateItem(checkResult, level = 1)
            }

            _validationViewItemsProp.setAll( (structuralItem_pool +: poolValidationItems) ++ orderValidationItems: _*)

          case _ => logger.error(s"updateValidationViewItems()..no TreeItem found for $item")
        }
      case Some(item: OrderDirNavigatorItem) =>
        // show ValidationResults for the selected order (directory)
        findTreeItemForFile(navigatorContentRoot, item.path) match {
          case Some(navTreeItem) =>

            val journalItems = collectValidationStateItemsForOrder(orderDirTreeItem = navTreeItem,
              collectErrors = true,
              collectValidationOk = true,
              level = 2)

            if (journalItems.nonEmpty) {

              val orderDirJItem = ValidationViewStructuralItem(
                result = None,
                PoolResource.PRType.OrderDirectory,
                name = item.path.toFile.getName,
                level = 1
              )

              _validationViewItemsProp.setAll(orderDirJItem +: journalItems.toSet.toSeq: _*)
            } else {
              _validationViewItemsProp.clear()
            }
          case _ => logger.error(s"updateValidationViewItems()..no TreeItem found for $item")
        }
      case Some(item: OrderDocNavigatorItem) =>
        // show ValidationResults related to the selected OrderDoc
        findTreeItemForFile(navigatorContentRoot, item.path) match {
          case Some(navTreeItem) =>
            val validationResults = navTreeItem.getValue.validationResults
            val journalItems = validationResults.map(ValidationViewStateItem(_, level = 1))
            _validationViewItemsProp.setAll(journalItems.toSet.toSeq.asInstanceOf[Seq[ValidationViewItem]]: _*)
          case _ => logger.error(s"updateValidationViewItems()..no TreeItem found for $item")
        }

      case _ =>
        _validationViewItemsProp.clear()
    }

    updateValidationViewState()
  }

  private def updateValidationViewState(): Unit = {

    val newJournalState: ValidationViewState.Value = appStateProp.getValue match {
      case AppState.Loading => ValidationViewState.LoadingArchive
      case AppState.Undefined => ValidationViewState.NoArchiveLoaded
      case AppState.Validating => ValidationViewState.ValidationInProgress
      case AppState.Loaded =>
        selectedNavigatorItemProp.getValue match {
          case None => ValidationViewState.NoItemSelected
          case Some(archiveItem: ArchiveNavigatorItem) =>
            if (getValidatedOrdersCount > 0) {
              if (getVisibleOrdersCount > 0)
                ValidationViewState.ValidationResultsAvailable
              else
                ValidationViewState.NoResultsDueToFiltering
            } else {
              ValidationViewState.ItemNotYetValidated
            }
          case Some(sel) =>
            if (sel.isValid.isEmpty) {
              ValidationViewState.ItemNotYetValidated
            } else {
              ValidationViewState.ValidationResultsAvailable
            }
        }
    }
    _validationViewStateProp.setValue(newJournalState)
    logger.debug(s"updateValidationViewState(): newJournalState=$newJournalState")
  }


  private def collectValidationStateItemsForOrder(orderDirTreeItem: TreeItem[NavigatorItem],
                                                  collectErrors: Boolean,
                                                  collectValidationOk: Boolean,
                                                  level: Int): Seq[ValidationViewItem] = {
    val orderDirVResults = orderDirTreeItem.getValue.validationResults.collect {
      case e: CheckFailure if (collectErrors) => ValidationViewStateItem(e, level = level)
      case e: CheckOk if (collectValidationOk) => ValidationViewStateItem(e, level = level)
    }

    val docVResults = orderDirTreeItem.getChildren.flatMap(_.getValue.validationResults).collect {
      case e: CheckFailure if (collectErrors) => ValidationViewStateItem(e, level = level)
      case e: CheckOk if (collectValidationOk) => ValidationViewStateItem(e, level = level)
    }
    orderDirVResults ++ docVResults
  }

  def resetValidationResults(): Unit = resetAllValidationStateInfos()


  def validateParticipationPool(): Future[ValidationState.Value] = {
    if(getValidatedOrdersCount > 0){
      resetAllValidationStateInfos()
    }
    _appStateProp.value = AppState.Validating
    updateValidationEnabledProps()
    logger.info(s"validateParticipationPool()..#orderDocFiles: ${getTotalOrdersCount}")

    val returnValPromise = Promise[ValidationState.Value]()

    val orderValidationCallback = new IntermediateResultCallback {
      override def onOrderValidated(result: OrderValidationResult, countValidatedOrders: Int): Unit = {

        val validationTaskRunning = _backgroundTaskInfoProp.getValue match {
          case Some(task@AsyncTask(validatePoolTaskId,_,_,_,_)) =>
            val progressPercent = (countValidatedOrders.toDouble / getTotalOrdersCount.toDouble)
            val isCanceled = task.task.isCancelled
            if(! isCanceled){
              runLater {
                _backgroundTaskInfoProp.setValue(Some(task.copy(progress = progressPercent)))
                integrateOrderValidationResults(result, doUpdateValidationViewItems=false)
              }
            }
            isCanceled
          case _ =>
            false
        }
      }
    }


    val drawTime = drawDateInfosProp.getValue

    val validationTask = validator.validatePool(archiveDirProp.value.get.toPath, getDrawTime, orderValidationCallback)

    setCurrentBackgroundTask(Some(AsyncTask(validatePoolTaskId, validationTask, description="validate pool",
      cancellable = true,progress = 0))
    )

    validationTask.run()

    validationTask.onComplete{ result =>
      logger.info(s"validateParticipationPool().Task.onComplete: $result")
      runLater {
        setCurrentBackgroundTask(None)
        _appStateProp.value = AppState.Loaded

        result match {
          case Success(poolValidationResult) =>
            logger.info(s"validateParticipationPool.Task.onComplete()..SUCCESS")
            integratePoolValidationResults(poolValidationResult)

          case Failure(t) =>
            resetAllValidationStateInfos()
            t match {
              case e: CompletionException => Option(e.getCause) match {
                case Some(e: CancellationException) =>
                  logger.info(s"validation cancelled by user")
                case _ =>
                  publishErrorEvents(ErrorMsg("Validation failed", detail = Some(t.getMessage)))
              }
              case e: CancellationException => logger.info(s"validation cancelled by user")
              case t => publishErrorEvents(ErrorMsg("Validation failed", detail = Some(t.getMessage)))
            }
        }

        updateValidationState()
        updateValidationViewState()
        updateValidationEnabledProps()
        updateSelectedNavigatorItem()
        updateDetailData()

        updateValidationViewItems()
      }
      logger.info("validateParticipationPool()..returnValPromise.success()")
      returnValPromise.success(validationStateProp.getValue)
    }

    logger.debug("validateParticipationPool()..method FINISHED")
    returnValPromise.future
  }

  private def setCurrentBackgroundTask[T](task: Option[AsyncTask[T]]): Unit = {
    logger.info(s"setCurrentBackgroundTask($task)")
    if(task.nonEmpty)
      require(_backgroundTaskInfoProp.getValue.isEmpty, s"backgroundTask is not empty: ${_backgroundTaskInfoProp.getValue}")
    _backgroundTaskInfoProp.setValue(task)
  }

  def validateSingleOrder(item: NavigatorItem): Unit = {
    logger.info(s"validateSingleOrder($item)")
    val result = validator.validateOrder(item.path, getDrawTime)
    integrateOrderValidationResults(result, doUpdateValidationViewItems=true)
  }

  def cancelValidation(): Unit = {
    if (appStateProp.getValue != AppState.Validating) {
      logger.warn(s"cancelValidation()..nothing to do (appState=${appStateProp.getValue})")
    } else {
      _backgroundTaskInfoProp.getValue match {
        case Some(task@AsyncTask(validatePoolTaskId,_,_,_,_)) =>
          task.task.cancel()
        case x => logger.warn(s"cancelValidation()..no $validatePoolTaskId found!")
      }
    }
  }

  private def integrateOrderValidationResults(result: OrderValidationResult, doUpdateValidationViewItems: Boolean): Unit = {

    //update order counters
    val orderWasNotYetValidated = _validatedOrders.add(result.orderLocation)
    if (orderWasNotYetValidated) {
      _validatedOrdersCount += 1
      result.isValid match {
        case true => _validOrdersCount += 1
        case false => _invalidOrdersCount += 1
      }
    }

    storeOrderValidationResultInNavTree(result)
    updateValidationState()
    updateSelectedNavigatorItem()

    if(doUpdateValidationViewItems) {
      updateValidationViewItems()
    }

    updateDetailData()
    updateValidationViewState()
  }

  def integratePoolValidationResults(poolValidationResult: ArchiveValidationResult): Unit = {

    navigatorContentRoot.getValue match {
      case item: ArchiveNavigatorItem =>
        val updatedItem = item.copy(validationResults = poolValidationResult.poolValidationResults)
        navigatorContentRoot.setValue(updatedItem)
      case x => logger.error(s"integratePoolValidationResults() unexpected item: $x")
    }
  }

  private def runNowOrLater(runNow: Boolean)(block: => Unit): Unit = {
    if (runNow) {
      block
    } else {
      runLater(block)
    }
  }

  private def initKeysAndCerts(): Unit = {
    logger.info("initKeysAndCerts()")
    credentialsManager = credentialsManager.withClearedPublicKeys().withClearedCertificates()

    val publicKeyErrors = Seq.newBuilder[ErrorMsg]
    _settings.credentialsSpecs.publicKeyConfigItems.foreach{ keySpec =>
      keySpec.file match {
        case UIValue(Some(file), error@None) =>
          val publicKey = Utils.readPublicKeyFromPemFile(file)
          credentialsManager = credentialsManager.withPublicKey(keyId=keySpec.keyId, algorithm = keySpec.algorithm, key=publicKey)
          publicKey.failed.foreach{t =>
            publicKeyErrors += ErrorMsg(s"Error loading public key ${keySpec.keyId}", detail=Some(t.getMessage), exception = Some(t))
          }
        case x =>
          publicKeyErrors += ErrorMsg(s"Error loading public key ${keySpec.keyId}",
            detail=keySpec.file.error.map(_.message), exception = keySpec.file.error.flatMap(_.exception))
      }
    }

    val certificateErrors = Seq.newBuilder[ErrorMsg]
    _settings.credentialsSpecs.certConfigItems.foreach{ certSpec =>
      certSpec.file match {
        case UIValue(Some(file), error@None )=>
          val cert = Utils.readX509CertificateFromPemFile(file)
          credentialsManager = credentialsManager.withCertificate(certSpec.name, cert)
          cert.failed.foreach{t =>
            certificateErrors += ErrorMsg(s"Error loading certificate ${certSpec.name}", detail=Some(t.getMessage), exception = Some(t))
          }
        case _ =>
          certificateErrors += ErrorMsg(s"Error loading loading certificate ${certSpec.name}",
            detail=certSpec.file.error.map(_.message), exception = certSpec.file.error.flatMap(_.exception))
      }
    }

    val tsCertErrors = Seq.newBuilder[ErrorMsg]
    val tsCerts: Seq[X509Certificate] = _settings.credentialsSpecs.timestamperCertConfigItems.flatMap{ certSpec =>
      certSpec.file match {
        case UIValue(Some(file), error@None )=>
          val cert = Utils.readX509CertificateFromPemFile(file)
          credentialsManager = credentialsManager.withCertificate(certSpec.name, cert)
          cert.failed.foreach{t =>
            tsCertErrors += ErrorMsg(s"Error loading timestamper certificate ${certSpec.name}", detail=Some(t.getMessage), exception = Some(t))
          }
          cert.toOption
        case _ =>
          tsCertErrors += ErrorMsg(s"Error loading loading timestamper certificate ${certSpec.name}",
            detail=certSpec.file.error.map(_.message), exception = certSpec.file.error.flatMap(_.exception))
          None
      }
    }

    credentialsManager = credentialsManager.withTimestamperCertificates(tsCerts)

    validator.setCredentialsProvider(credentialsManager)

    val errors = publicKeyErrors.result() ++ certificateErrors.result()
    if(errors.nonEmpty) {
      publishErrorEvents(errors: _*)
    }
    updateValidationEnabledProps()
  }

  private def isOrderDirectorySelected: Boolean = selectedNavigatorItemProp.value match {
    case Some(x: OrderDirNavigatorItem) => true
    case _ => false
  }

  /**
    * Find the TreeItem for a given order-directory (fast binary search implementation).
    * Expects the tree to be sorted by `File.getName()`.
    * */
  private def findOrderDirNavTreeItem(path: Path): Option[TreeItemExt[NavigatorItem]] = {
    val dummyTreeItem4BinarySearch = new TreeItemExt[NavigatorItem](
      OrderDirNavigatorItem(path = path, displayName = null, retailerOrderReference = null, validationState=ValidationState.NotValidated,
        hasInvalidOrderDocs=false, validationResults=IndexedSeq.empty, productInfos=null)
    )
    val index = Collections.binarySearch(navigatorContentRoot.filteredChildren_source, dummyTreeItem4BinarySearch,
      navItemFileNameComparator)
    val r = if(index < 0) None else Option(navigatorContentRoot.filteredChildren_source.get(index))
    r
  }

  /**
    * Stores a single `OrderValidationResult` in the Navigator-tree.
    * */
  private[model] def storeOrderValidationResultInNavTree(result: OrderValidationResult): Unit = {
    import PoolResource.PRType.{OrderDirectory, PoolDirectory}

    val orderLevelVResults = result.checkResults.filter(!_.check.affectedResources.contains(OrderDirectory))
    val hasInvalidOrderDocs = result.checkResults.exists(r => !r.isOk && r.check.affectedResources.filterNot(_ == OrderDirectory).nonEmpty)
    val orderDirTreeItem = findOrderDirNavTreeItem(result.orderLocation)

    orderDirTreeItem.foreach { orderTreeItem =>
      Option(orderTreeItem.getValue) match {
        case Some(navItem: OrderDirNavigatorItem) =>
          val updatedItem = navItem.copy(hasInvalidOrderDocs = hasInvalidOrderDocs,
            validationResults = orderLevelVResults, validationState = ValidationState.CompletelyValidated)
            orderTreeItem.setValue(updatedItem)
        case x => sys.error(s"unexpected NavigatorItem: $x")
      }

      PoolResource.PRType.values.filterNot(x => Seq(OrderDirectory, PoolDirectory).contains(x)).map {
        docType =>
          val docSpecificVResults = result.checkResults.filter(_.check.affectedResources.contains(docType))
          val docFileName = PoolResource.docTypeToFileName(docType)
          orderTreeItem.getChildren.find(_.getValue.path.toFile.getName == docFileName) match {
            case Some(docTreeItem) =>
              docTreeItem.setValue( docTreeItem.getValue.withValidationResults(docSpecificVResults) )
            case _ =>
              logger.debug(s"no docTreeItem found for $docFileName")
          }
      }
    }
  }

  private def findTreeItemForFile(treeItem: TreeItemExt[NavigatorItem], path: Path): Option[TreeItemExt[NavigatorItem]] = {
    require(treeItem != null, "treeItem is null!")
    require(treeItem.value != null, "treeItem.value is null!")
    require(path != null, "file is null!")

    if (!path.startsWith(treeItem.getValue.path)) {
      None
    } else {
      if (treeItem.getValue.path == path)
        Some(treeItem)
      else {
        treeItem.filteredChildren_source.find {
          child =>
            path.startsWith(child.getValue.path)
        } match {
          case Some(child) => findTreeItemForFile(child, path)
          case _ => None
        }
      }
    }
  }

  def findNavigatorItem(predicate: TreeItem[NavigatorItem] => Boolean): Option[NavigatorItem] = {
    findTreeItemWithPred(navigatorContentRoot, predicate).map(_.getValue)
  }

  private def findTreeItemWithPred(treeItem: TreeItemExt[NavigatorItem], predicate: TreeItemExt[NavigatorItem] => Boolean): Option[TreeItemExt[NavigatorItem]] = {
    require(treeItem != null, "treeItem is null!")
    require(treeItem.value != null, "treeItem.value is null!")
    if (predicate(treeItem))
      Some(treeItem)
    else {
      treeItem.filteredChildren_source.foreach { child =>
        val r = findTreeItemWithPred(child, predicate)
        if (r.isDefined)
          return r
      }
      None
    }
  }

  def findNavigatorItemFor(file: Path): Option[NavigatorItem] = {
    findTreeItemForFile(navigatorContentRoot, file).map(_.getValue)
  }

  def addErrorEventHandler(handler: Seq[ErrorMsg] => Unit): UUID = errorEventHandler.addEventHandler(handler)

  def removeErrorEventHandler(id: UUID): Option[Seq[ErrorMsg] => Unit] = errorEventHandler.removeEventHandler(id)

  /**
    * Delivers the initial directory for the `FileChooser` when an archive is to be loaded.
    **/
  def getArchiveFileSelectorInitialDirectory: Option[Path] = {
    if (poolSourceProp.value.nonEmpty) Option(poolSourceProp.value.get.path.getParent)
    else {
      Option(System.getProperty("user.dir")).map(new File(_).toPath)
    }
  }

  /**
    * Delivers the initial directory for the `DirectoryChooser` when an archive is to be loaded.
    **/
  def getArchiveDirectorySelectorInitialDirectory: Option[Path] = {
    if (poolSourceProp.value.nonEmpty) Option(poolSourceProp.value.get.path.getParent)
    else {
      Option(System.getProperty("user.dir")).map(new File(_).toPath)
    }
  }

  def getPoolValidationErrorsCount : Int = Option(navigatorContentRoot.getValue).map(_.validationResults.count(!_.isOk)).getOrElse(0)

  def hasPoolValidationErrors : Boolean = getPoolValidationErrorsCount > 0

  def getInvalidOrdersCount: Int = _invalidOrdersCount

  def getValidOrdersCount: Int = _validOrdersCount

  def getValidatedOrdersCount: Int = _validatedOrdersCount

  /**
    * Delivers the total order count of the loaded pool archive (without considering any Navigator-filters).
    **/
  def getTotalOrdersCount: Int = _totalOrdersCount

  /**
    * Delivers the count of `OrderDirNavigatorItems` currently visible in the NavigatorView
    * (the count may be < `getTotalOrdersCount` due to active Navigator-filters).
    **/
  def getVisibleOrdersCount: Int = navigatorContentRoot.getChildren.size

  def getSettings: ApplicationSettings = _settings

  def setSettings(settings: ApplicationSettings): Unit = {
    val oldSettings = _settings
    _settings = settings
    val changed = settings != oldSettings
    logger.info(s"setSettings() ${
      if (!changed) " -> unchanged..abort" else ""
    }")
    if (changed) {
      settingsManager.saveSettings(_settings, configFile.toURI)
      resetAllValidationStateInfos()
      applySettings()
      updateCanOpenArchiveProp()
    }
  }

  def cancelTask(taskId: String): Unit = {
    logger.info(s"cancelTask($taskId)")
    this._backgroundTaskInfoProp.getValue match {
      case Some(taskInfo) if (taskInfo.id == taskId) =>
        taskInfo.task.cancel()
      case _ => logger.warn(s"cancelTask() no task found with id=$taskId")
    }
  }

  private def updateCanOpenArchiveProp(): Unit = {
    _canOpenArchiveProp.setValue((_settings != null) && _settings.archiveExtractionTarget.isValid)
  }

  private def updateValidationEnabledProps(): Unit = {
    val canValidateArchive = archiveDirProp.getValue.nonEmpty && appStateProp.value != AppState.Validating
    _validateArchiveEnabledProp.setValue(canValidateArchive)

    logger.debug(
      s"""
         |updateValidationEnabledProps()
         |canValidateArchive:$canValidateArchive
         |isOrderDirectorySelected: $isOrderDirectorySelected
         |canOpenArchiveProp: ${canOpenArchiveProp.getValue}
       """.stripMargin)

    _validateSingleOrderEnabledProp.setValue(canValidateArchive && isOrderDirectorySelected)
  }

  private def resetCachedArchiveData(): Unit = {
    _validatedOrders.clear()
    _validatedOrdersCount = 0
    _invalidOrdersCount = 0
    _validOrdersCount = 0
    _cachedArchiveDetailData = None
  }

  /**
    * Resets all ValidationState (e.g. in navigatorContentRoot, errorJournalProp, selectedNavigatorItem)
    **/
  private def resetAllValidationStateInfos(): Unit = {
    resetCachedArchiveData()

    _validationViewItemsProp.clear()
    _validationStateProp.setValue(ValidationState.NotValidated)
    _selectedNavigatorItemProp.value = selectedNavigatorItemProp.value.map(_.withValidationResults(IndexedSeq.empty))

    def udpateTreeItemValue(item: TreeItemExt[NavigatorItem], fUpdate: NavigatorItem => NavigatorItem): Unit = {
      item.setValue(fUpdate(item.getValue))
      item.filteredChildren_source.foreach { childNode => udpateTreeItemValue(childNode, fUpdate) }
    }
    require(navigatorContentRoot != null, "navigatorContentRoot is null")

    if (navigatorContentRoot.getValue != null) {
      udpateTreeItemValue(navigatorContentRoot, fUpdate = _.withoutValidationResults)
    }
    updateSelectedNavigatorItem()
    updateDetailData()
    refreshNavigatorFilter()
    updateValidationViewState()
  }

  /** Workaround: when an item in `navigatorContentRoot.getChildren` was updated the `navigatorContentRoot.getChildren`
    * needs to be refiltered.
    * */
  private def refreshNavigatorFilter(): Unit = {
    logger.info("refreshNavigatorFilter()")
    navigatorContentRoot.setPredicate(navigatorFilterChain.copy())
  }

  private def runLater(block: => Unit): Unit = {
    runLaterExecutor.execute(new Runnable() {
      override def run(): Unit = block
    })
  }


  /**
    * All-in-one `Predicate[TreeItem[NavigatorItem]]` that combining all filters of the the `NavigatorView`.
    **/
  case class NavigatorFilterChain(private var textFilter: Option[Predicate[TreeItem[NavigatorItem]]] = None,
                                  private var hideInvalidOrdersFilter: Option[Predicate[TreeItem[NavigatorItem]]] = None,
                                  private var hideValidOrdersFilter: Option[Predicate[TreeItem[NavigatorItem]]] = None,
                                  private var hideUnvalidatedOrdersFilter: Option[Predicate[TreeItem[NavigatorItem]]] = None,
                                  private val activeFilters: Seq[Predicate[TreeItem[NavigatorItem]]] = Nil
                                 ) extends Predicate[TreeItem[NavigatorItem]] {
    def withTextFilter(filter: Option[String]): NavigatorFilterChain = {
      textFilter = filter.map {
        filterStr => genericPredicateOf{treeItem => 
          val displayNameMatches = treeItem.getValue.displayName.contains(filterStr)
          val retailerOrderIdMatches : Boolean = if(displayNameMatches) false else {
            treeItem.getValue match {
              case o : OrderDirNavigatorItem =>
                o.retailerOrderReference.contains(filterStr)
              case  _  => false
            }             
          }
          displayNameMatches || retailerOrderIdMatches
        }
      }
      copy(activeFilters = calcActiveFilters)
    }

    private def calcActiveFilters: Seq[Predicate[TreeItem[NavigatorItem]]] = {
      Seq(textFilter, hideInvalidOrdersFilter, hideValidOrdersFilter, hideUnvalidatedOrdersFilter).flatten
    }

    def setHideValidOrdersFilter(hide: Boolean): NavigatorFilterChain = {
      hideValidOrdersFilter = if (!hide) None
      else {
        Some(
          genericPredicateOf { treeItem =>
            treeItem.getValue match {
              case item: OrderDirNavigatorItem => item.isValid != Some(true)
              case _ => true
            }
          }
        )
      }
      copy(activeFilters = calcActiveFilters)
    }

    def setHideInvalidOrdersFilter(hide: Boolean): NavigatorFilterChain = {
      hideInvalidOrdersFilter = if (!hide) None
      else {
        Some(
          genericPredicateOf { treeItem =>
            treeItem.getValue match {
              case item: OrderDirNavigatorItem => item.isValid != Some(false)
              case _ => true
            }
          }
        )
      }
      copy(activeFilters = calcActiveFilters)
    }

    def setHideUnvalidatedOrdersFilter(hide: Boolean): NavigatorFilterChain = {
      hideUnvalidatedOrdersFilter = if (!hide) None
      else {
        Some(
          genericPredicateOf { treeItem =>
            treeItem.getValue match {
              case item: OrderDirNavigatorItem => item.isValid.nonEmpty
              case _ => true
            }
          }
        )
      }
      copy(activeFilters = calcActiveFilters)
    }

    override def test(item: TreeItem[NavigatorItem]): Boolean = {
      if (activeFilters.isEmpty) true
      else {
        activeFilters.forall(_.test(item))
      }
    }
  }

  private def deleteArchiveTargetTmpDir(isAsync: Boolean): Unit = {
    archiveExtractionTargetTempDir.foreach { archiveTempDir =>
      if (archiveTempDir.exists()) {
        try {
          logger.info(s"deleteArchiveTargetTmpDir()..deleting $archiveTempDir")
          FileUtils.deleteDirectory(archiveTempDir)
          logger.info(s"deleteArchiveTargetTmpDir()..deleted $archiveTempDir.")
        } catch {
          case e: Throwable =>
            runNowOrLater(runNow = !isAsync) {
              publishErrorEvents(ErrorMsg(s"failed: delete temp-dir ${archiveTempDir.toString}",
                detail = Some(e.getMessage), exception = Some(e)))
            }
        }
      }
      archiveExtractionTargetTempDir = None
    }
  }

  def dispose(): Unit = {
    disposed=true
    deleteArchiveTargetTmpDir(isAsync = false)
  }
}


object ApplicationModel {

  case class DrawDateInfos(archiveDrawDate: ZonedDateTime, customDrawDate: Option[ZonedDateTime])

  object AppState extends Enumeration {
    val Undefined, Loading, Loaded, Validating = Value
  }

  object ValidationState extends Enumeration {
    val NotValidated, PartiallyValidated, CompletelyValidated = Value
  }

  sealed trait PoolSource {
    def path: Path
  }

  case class PoolSourceDirectory(path: Path) extends PoolSource

  case class PoolSourceArchive(path: Path) extends PoolSource


  trait TaskInfo{
    def id : String
    def description : String
    def cancellable : Boolean
    def progress : Double
  }

  case class AsyncTask[T](id: String, task: Task[T], description: String, cancellable: Boolean, progress: Double) extends TaskInfo


  private val extractArchiveTaskId  = "Extract pool archive"
  private val validatePoolTaskId  = "Validate pool"
  private val deleteTmpDirTaskId  = "Delete temporaray directory"

}










