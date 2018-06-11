package model

import java.io.File
import java.nio.file.{Path, Paths}
import java.security.cert.X509Certificate
import java.time._
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.{CancellationException, CompletionException, Executor}
import java.util.function.Predicate
import java.util.{Collections, Comparator, UUID}

import _root_.util.Utils._
import domain.PoolResource.Filenames
import domain.PoolValidator._
import domain._
import domain.products.GamingProduct
import domain.products.GamingProduct.GamingProductId
import javafx.application.Platform
import javafx.beans.property._
import javafx.collections.{FXCollections, ObservableList}
import javafx.scene.control.TreeItem
import model.ApplicationModel._
import model.ArchiveDetailData.BetBreakdownRowItem
import model.NavigatorTreeItem._
import model.OrderDirNavigatorItem.ProductInfos
import model.base.EventHandlerRegistry
import monix.eval.Task
import monix.execution.{Cancelable, CancelableFuture, Scheduler => MonixScheduler}
import org.slf4j.LoggerFactory
import scalafx.Includes._
import util.Utils

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}


/**
  * @param runLaterExecutor The UI must be updated from the JavaFX Application thread. Since some operations are performed
  *                         concurrently, their result have to be set to the `ApplicationModel` via `Platform.runLater()`.
  *                         Since the call is not suitable for Unit-test the run-later functionality must be pluggable
  *                         which is achieved by `trait RunLater`.
  *                         Important: `runLaterExecutor` must be thread-safe!
  **/
class ApplicationModel(resourceProviderFactory: PoolResourceProviderFactory,
  validatorFactory: PoolValidatorFactory,
  settingsManager: ApplicationSettingsManager,
  private var credentialsManager: CredentialsManager,
  configFile: File,
  runLaterExecutor: Executor,
  implicit val monixScheduler: MonixScheduler
) {

  private val logger = LoggerFactory.getLogger(getClass)

  //Properties
  private val _poolSourceProp = new SimpleObjectProperty(Option.empty[PoolSource])
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
  private val _backgroundTaskInfoProp = new SimpleObjectProperty[Option[TaskInfo]](None)
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
      o1.getValue.relativePath.toString compare o2.getValue.relativePath.toString
    }
  }

  private[model] var resourceProvider: Option[PoolResourceProvider] = None
  private var archiveStats = new ArchiveStats
  private[model] var _validationStats = new ValidationStats()
  
  //cached values (needed for performance reasons, obtaining it from navigatorContentRoot.getChildren is too slow..)
  private var _cachedArchiveDetailData = Option.empty[ArchiveDetailData]
  private var disposed = false
  private var lastOpenResourcePath = Option.empty[Path]

  //constants
  private val LineSep = System.getProperty("line.separator", "\n")
  private val EmptyValidationResults = IndexedSeq.empty[CheckResult]
  private var AllOrderDocumentValidationsOk = Option.empty[IndexedSeq[CheckResult]]
  private val BackgroundTaskDescription_validatePool = "Validate pool"

  def settings: ApplicationSettings = _settings

  def poolSourceProp: ReadOnlyProperty[Option[PoolSource]] = _poolSourceProp

  def appStateProp: ReadOnlyProperty[AppState.Value] = _appStateProp

  def validationStateProp: ReadOnlyProperty[ValidationState.Value] = _validationStateProp

  def validateArchiveEnabledProp: ReadOnlyBooleanProperty = _validateArchiveEnabledProp

  def validateSingleOrderEnabledProp: ReadOnlyBooleanProperty = _validateSingleOrderEnabledProp

  def navigatorFilterOptions: ReadOnlyProperty[NavigatorFilterOptions] = _navigatorFilterOptions

  def showUIDebugControlsProp: ReadOnlyBooleanProperty = _showUIDebugControlsProp

  def canOpenArchiveProp: ReadOnlyBooleanProperty = _canOpenArchiveProp

  def selectedNavigatorItemProp: ReadOnlyProperty[Option[NavigatorItem]] = _selectedNavigatorItemProp

  def orderDocDetailDataProp: ReadOnlyProperty[Option[DetailData]] = _orderDocDetailDataProp

  /** The PoolValidator supports one asynchronous background task at a time (e.g. "Load archive"). This property provides
    * informations about its current state (e.g. description, progress indication etc.).*/
  def backgroundTaskInfoProp: ReadOnlyProperty[Option[TaskInfo]] = _backgroundTaskInfoProp.asInstanceOf[SimpleObjectProperty[Option[TaskInfo]]]

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
    initKeysAndCerts()
    _showUIDebugControlsProp.setValue(settings.showUIDebugControls)
  }

  def loadPoolDirectory(directory: File): Unit = loadPoolSource(
    PoolSourceDirectory(directory.toPath), validateOnLoading = settings.validatePoolOnLoading
  )

  def loadPoolArchive(archiveFile: File): Unit = loadPoolSource(
    PoolSourceArchive(archiveFile.toPath), validateOnLoading = settings.validatePoolOnLoading
  )

  def unload(): Future[Unit] = {
    logger.info(s"unload()..appState: ${appStateProp.getValue}, ${poolSourceProp.getValue}")
    resetModelState()
  }

  private def publishErrorEvents(errors: ErrorMsg*): Unit = {
    errors.foreach { err =>
      err.exception match {
        case Some(t) => logger.error(s"publishErrorEvent(${err}", t)
        case _ => logger.error(s"publishErrorEvent(${err}")
      }
    }
    errorEventHandler.publishEvent(errors)
  }

  /** @param validateOnLoading when this parameter is `true`, the participation pool archive is validated 
    *                          immediately after loading (convenience option). */
  private[model] def loadPoolSource(poolSource: PoolSource, validateOnLoading: Boolean): Unit = {
    logger.info(s"loadPoolSource($poolSource)")
    resetModelState().onComplete { resetResult =>
      logger.info("resetModelFuture.onComplete")
      runLater {
        loadPoolSourceImpl(poolSource, validateOnLoading)
      }
    }
    logger.debug(s"loadPoolImpl()..leaving")
  }

  private def validatePoolTimestamp(orderIds: Vector[String]): Unit = {
    val taskId = UUID.randomUUID()
    runLater {
      setCurrentBackgroundTask(Some(CancelableMonixTask(cancelable = None, "Validate pool timestamp", id = taskId)))
    }

    def updateProgressIndicator(newProgress: Double): Unit = {
      updateBackgroundTaskInfoPropIfSoConditioned[CancelableMonixTask](taskId, condition = _.progress != newProgress) { oldTaskInfo =>
        Some(oldTaskInfo.withUpdatedProgress(newProgress))
      }
    }

    val vResult: IndexedSeq[CheckResult] = validatorFactory
      .getValidator(resourceProvider.get, credentialsManager, monixScheduler)
      .validatePoolSeal(orderIds, resourceProvider.get.getPoolMetadata(), getDrawTime, Some(updateProgressIndicator))

    logger.info(s"validatePoolTimestamp() - result: ${vResult.mkString("\n")}")

    runLater {
      integratePoolValidationResults(vResult)
      updateBackgroundTaskInfoPropIfSo[CancelableMonixTask](taskId)(_ => None)
      updateValidationState()
      updateValidationViewItems()
    }
  }

  private[model] def loadPoolSourceImpl(poolSource: PoolSource, validateOnLoading: Boolean): Unit = {
    logger.info("loadPoolSourceImpl..start")
    if (resourceProvider.nonEmpty) {
      logger.error(s"loadPoolSourceImpl($poolSource)..resourceProvider is non-empty!")
    }
    resourceProvider = resourceProviderFactory.getProvider(poolSource).toOption
    _appStateProp.setValue(AppState.Loading)
    lastOpenResourcePath = Some(poolSource.path)
    updateValidationViewState()
    _poolSourceProp.setValue(Option(poolSource))

    val initResourceProviderFuture = resourceProvider.map(_.init()).getOrElse(Future.successful())

    initResourceProviderFuture.onComplete {
      case Success(_) =>
        val generateItemsTaskId = UUID.randomUUID()

        runLater {
          setCurrentBackgroundTask(Some(CancelableMonixTask(cancelable = None, "Loading pool archive", id = generateItemsTaskId)))
          navigatorContentRoot.setValue(ArchiveNavigatorItem(
            relativePath = poolSource.path,
            displayName = poolSourceProp.getValue.get.path.toFile.getName,
            poolSource = poolSourceProp.getValue.get,
            isValid = None,
            validationState = ValidationState.NotValidated
          ))
          _navigatorContentRootProp.setValue(Some(navigatorContentRoot))
        }

        val tsBeforeGenNavItems = Instant.now()

        def updateProgress(newProgress: Double): Unit = updateBackgroundTaskInfoPropIfSoConditioned[CancelableMonixTask](
          generateItemsTaskId, condition = _.progress != newProgress
        ) {
          oldTask => Some(oldTask.withUpdatedProgress(newProgress))
        }

        val createNavigatorItemsTaskFuture = generateNavigatorItems(validateOnLoading, progressCb = updateProgress)
          .onCancelRaiseError(new CancellationException("generateNavigatorItems() was cancelled")).runAsync

        runLater {
          updateBackgroundTaskInfoPropIfSo[CancelableMonixTask](generateItemsTaskId) { currentTask =>
            Some(currentTask.copy(cancelable = Some(createNavigatorItemsTaskFuture)))
          }
        }

        createNavigatorItemsTaskFuture.onComplete { tr =>
          tr match {
            case Success(navTreeItems) =>
              logger.info(s"generateNavigatorItems() succeeded - #items: ${navTreeItems.size}, duration: ${Duration.between(tsBeforeGenNavItems, Instant.now()).toMillis} ms")
              updateBackgroundTaskInfoPropIfSo[CancelableMonixTask](generateItemsTaskId)(_ => None)
              runLater {
                navigatorContentRoot.setValue(ArchiveNavigatorItem(
                  relativePath = Paths.get(""),
                  displayName = poolSourceProp.getValue.get.path.toFile.getName,
                  poolSource = poolSourceProp.getValue.get,
                  isValid = None,
                  validationState = ValidationState.NotValidated
                ))

                updateValidationEnabledProps()
                if (validateOnLoading) {
                  updateValidationState()
                  updateValidationViewItems()
                }
                logger.info(s"loadPoolImpl()..set orderDir NavTreeItems")
                _navigatorContentRootProp.setValue(None) //IMPORTANT, otherwise the TreeView will stuck with high #navTreeItems 
                navigatorContentRoot.filteredChildren_source.setAll(navTreeItems.asJavaCollection)
                logger.info(s"loadPoolImpl()..set orderDir NavTreeItems..finished")
                _appStateProp.setValue(AppState.Loaded)
                updateValidationViewState()
                _navigatorContentRootProp.setValue(Some(navigatorContentRoot))
              }
              if (validateOnLoading) {
                validatePoolTimestamp(navTreeItems.map(x => x.getValue).collect{ case item: OrderDirNavigatorItem  => item.orderId}.toVector)
              }

            case Failure(t) =>
              Option(t).collect { case c: CancellationException => c }.fold(
                logger.error(s"loadPoolSourceImpl - task.onFailure: ${t.getMessage}")
              )(c =>
                logger.info(s"loadPoolSourceImpl - cancelled by user: ${c.getMessage}")
              )
              runLater {
                updateBackgroundTaskInfoPropIfSo[CancelableMonixTask](generateItemsTaskId)(_ => None)
                setCurrentBackgroundTask(None)
                updateValidationEnabledProps()
                resetModelState()
                updateValidationViewState()
                if (!t.isInstanceOf[CancellationException]) {
                  publishErrorEvents(ErrorMsg(s"Failed to load ${poolSource.getClass.getSimpleName} ${poolSource.path.toString}", detail = Some(t.getMessage), exception = Some(t)))
                }
              }
          }
        }
      case Failure(t) =>
        logger.error(s"resourceProvider.init() failed: ${t.getMessage}", t)
    }
  }

  private def generateNavigatorItems(validateOnLoading: Boolean, progressCb: Double => Unit): Task[IndexedSeq[TreeItemExt[NavigatorItem]]] = {
    logger.info(s"generateNavigatorItems()")
    val tsBefore = Instant.now()

    val poolMetadata = resourceProvider.get.getPoolMetadata()
    poolMetadata match {
      case Success(metaInfo) => runLater {
        _drawDateInfosProp.setValue(Some(DrawDateInfos(archiveDrawDate = metaInfo.drawDate, customDrawDate = None)))
      }
      case Failure(f) => runLater {
        publishErrorEvents(ErrorMsg(s"Error loading MetaInfos", detail = Some(f.getMessage), exception = Some(f)))
      }
    }

    val validator = validatorFactory.getValidator(resourceProvider.get, credentialsManager, monixScheduler)

    archiveStats.totalOrdersCount = resourceProvider.get.getOrdersCount().getOrElse(0)
    logger.info(s"generateNavigatorItems() - resourceProvider.get.getOrderDirPaths() #orders:${getTotalOrdersCount}, duration: ${Duration.between(tsBefore, Instant.now())}")

    val i = new AtomicLong(0)
    resourceProvider.get.getOrderDocsObservable().mapParallelUnordered(parallelism = Runtime.getRuntime().availableProcessors()) {
      case Success(docsForOneOrder) =>
        Task {
          val orderDirItem = generateNavigatorItemsForOneOrder(docsForOneOrder)
          val orderDirItemDowncasted = orderDirItem.asInstanceOf[TreeItemExt[NavigatorItem]]
          val retailer = orderDirItem.getValue.retailer
          val origin = orderDirItem.getValue.origin
          
          archiveStats.synchronized {
            //update orders breakdown
            val retailerMap = archiveStats.ordersCountBreakdown.getOrElseUpdate(retailer, mutable.Map.empty[Option[Origin], Int])
            val currentCount = retailerMap.getOrElse(origin, 0)
            retailerMap.update(origin, currentCount + 1)

            //update bet count breakdown
            orderDirItem.getValue.productInfos.foreach { productInfo =>
              productInfo.betCountPerProduct.foreach { case (productId, count) =>
                val retailerMap = archiveStats.betCountBreakdown.getOrElseUpdate(productId, mutable.Map.empty[Retailer, mutable.Map[Option[Origin], Int]])
                val originMap = retailerMap.getOrElseUpdate(retailer, mutable.Map.empty[Option[Origin], Int])
                val currentCount = originMap.getOrElse(origin, 0)
                originMap.update(origin, currentCount + count)
              }
            }
          }
          
          val progressPercent: Double = Math.round((i.incrementAndGet() / getTotalOrdersCount.toDouble) * 100.0) / 100.0
          if (progressPercent != backgroundTaskInfoProp.value.map(_.progress).getOrElse(0)) {
            progressCb(progressPercent)
          }
          if (validateOnLoading) {
            val vr = validator.validateOrderDocs(docsForOneOrder, getDrawTime, poolMetadata)
            updateValidationStatsThreadSafe(vr)
            attachValidationResults(orderDirItemDowncasted, vr)
          }
          Some(orderDirItemDowncasted)
        }

      case Failure(t) =>
        logger.error(s"failed to get OrderDocs: ${t.getMessage}", t)
        Task.pure(None)
    }.collect { case Some(treeItem) => treeItem }
      .foldLeftL(IndexedSeq.newBuilder[TreeItemExt[NavigatorItem]]) { case (seqBuilder, elem) => seqBuilder += elem }
      .map(_.result().sortBy(_.getValue.displayName))
  }

  private def generateNavigatorItemsForOneOrder(docsForOneOrder: OrderDocs): TreeItemExt[OrderDirNavigatorItem] = {
    val order = docsForOneOrder.order
    if (order.isFailure)
      logger.error(s"failed to load order in ${docsForOneOrder.orderId}", order.failed.get)
    val navItem = OrderDirNavigatorItem(
      relativePath = Paths.get(docsForOneOrder.orderId),
      retailer = order.map(_.metaData.retailer).getOrElse(Symbol("no retailer")),
      origin = order.toOption.flatMap(_.metaData.origin),
      retailerOrderReference = order.map(_.metaData.retailerOrderReference).getOrElse(""),
      validationState = ValidationState.NotValidated,
      hasInvalidOrderDocs = false,
      productInfos = order.toOption.map(createProductInfosFor))
    val orderDirItem = new TreeItemExt[NavigatorItem](navItem)

    docsForOneOrder.getAll.foreach { orderDocTry =>
      orderDocTry.foreach { orderDoc => 
        orderDirItem.filteredChildren_source.add(new TreeItemExt[NavigatorItem](OrderDocNavigatorItem(relativePath = orderDoc.docPath)))
      }
    }
    orderDirItem.asInstanceOf[TreeItemExt[OrderDirNavigatorItem]]
  }

  private def createProductInfosFor(order: Order): ProductInfos = {
    val betsCountPerProduct = order.gamingProductOrders.values.map { productOrder =>
      val productId = GamingProduct.gamingProductIdFromURI(productOrder.productURI)
      (productId, productOrder.bets.size)
    }.toMap
    ProductInfos(betCountPerProduct = betsCountPerProduct)
  }

  private def resetModelState(): Future[Unit] = {
    archiveStats = new ArchiveStats
    resetCachedArchiveData()

    _drawDateInfosProp.setValue(None)
    _appStateProp.setValue(AppState.Undefined)
    _poolSourceProp.setValue(None)
    _navigatorContentRootProp.setValue(None)
    navigatorContentRoot.setValue(null)
    navigatorContentRoot.filteredChildren_source.clear()
    _selectedNavigatorItemProp.setValue(None)
    _orderDocDetailDataProp.setValue(None)
    _validationStateProp.setValue(ValidationState.NotValidated)
    _validationViewStateProp.setValue(ValidationViewState.NoArchiveLoaded)

    updateValidationEnabledProps()
    _validationViewItemsProp.clear()
    val resultFuture = resourceProvider.map(_.dispose()).getOrElse(Future.successful())
    resourceProvider = None
    resultFuture
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
  private def getDrawTime: Try[Instant] = {
    drawDateInfosProp.getValue match {
      case Some(drawDateInfo) =>
        Success(drawDateInfo.customDrawDate.getOrElse(drawDateInfo.archiveDrawDate).toInstant)
      case _ => Failure(new Exception("drawDateInfosProp is undefined"))
    }
  }

  private[model] def updateValidationState(): Unit = {
    val totalCount = getTotalOrdersCount
    val validationState = validationStats.validatedOrdersCount match {
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
            if ((validationStats.invalidOrdersCount > 0) || hasPoolValidationErrors) Some(false) else None
          case ValidationState.CompletelyValidated =>
            Some((getTotalOrdersCount == validationStats.validOrdersCount) && !hasPoolValidationErrors)
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
    **/
  private def updateSelectedNavigatorItem(): Unit = {
    // find the navTreeItem for the the currently selected NavigatorItem
    val navTreeItem: Option[TreeItemExt[_ <: NavigatorItem]] = selectedNavigatorItemProp.getValue match {
      case Some(selectedItem: ArchiveNavigatorItem) =>
        Option(navigatorContentRoot)
      case Some(selectedItem: OrderDirNavigatorItem) =>
        findOrderDirNavTreeItem(selectedItem.relativePath)
      case Some(selectedItem: OrderDocNavigatorItem) =>
        val orderDirPath = selectedItem.relativePath.getParent
        findOrderDirNavTreeItem(orderDirPath) match {
          case Some(orderDirTreeItem) =>
            orderDirTreeItem.filteredChildren_source.find(_.getValue.relativePath == selectedItem.relativePath)
          case _ => None
        }
      case _ => None
    }
    _selectedNavigatorItemProp.setValue(navTreeItem.map(_.getValue))
  }

  private def createArchiveDetailData(): Try[ArchiveDetailData] = {
    poolSourceProp.getValue.map { poolSource =>
      resourceProvider.get.getPoolMetadata().map { poolMetaInfos =>
        val orderStats = ArchiveDetailData.OrderStats(totalOrdersCount = getTotalOrdersCount,
          betCountBreakdown = archiveStats.getBetCountBreakdown,
          ordersCountBreakdown = archiveStats.getOrdersCountBreakdown
        )
        val rowData: Vector[BetBreakdownRowItem] = for {
          retailer <- orderStats.allRetailers.toVector.sortBy(_.name)
          origin <- orderStats.originsFor(retailer).toVector.sortBy(_.map(_.name).getOrElse(""))
        } yield {
          val betCountPerProductMap = orderStats.betCountBreakdown.mapValues { case retailerMap =>
            val count = retailerMap.get(retailer).flatMap(_.get(origin)).getOrElse(0)
            count
          }
          BetBreakdownRowItem(retailer, origin, orderStats.getOrdersCountFor(retailer, origin), betCountPerProductMap)
        }

        val poolDigestTimestamp = resourceProvider.get.getPoolDigestTimestamp().map(_.rawData)
        ArchiveDetailData(poolSource = poolSourceProp.getValue.get,
          poolMetadata = poolMetaInfos,
          poolDigestTimestamp = poolDigestTimestamp.toOption,
          orderStats = orderStats,
          betBreakdownRowData = rowData,
          extractedToDir = poolSource.path,
          validationState = validationStateProp.getValue,
          totalOrdersCount = getTotalOrdersCount,
          validatedOrdersCount = validationStats.validatedOrdersCount,
          validOrdersCount = validationStats.validOrdersCount,
          invalidOrdersCount = validationStats.invalidOrdersCount
        )
      }
    }.getOrElse(Failure(new Exception("poolSourceProp.getValue is undefined")))
  }

  private def updateDetailData(): Unit = {
    val detailData: Option[DetailData] = selectedNavigatorItemProp.getValue match {
      case Some(sel: ArchiveNavigatorItem) =>
        val archiveDetailData = if (_cachedArchiveDetailData.nonEmpty) _cachedArchiveDetailData else {
          _cachedArchiveDetailData = createArchiveDetailData().toOption
          _cachedArchiveDetailData
        }
        archiveDetailData.map { oldArchiveDetailData =>
          oldArchiveDetailData.copy(
            validationState = validationStateProp.getValue,
            totalOrdersCount = getTotalOrdersCount,
            validatedOrdersCount = validationStats.validatedOrdersCount,
            validOrdersCount = validationStats.validOrdersCount,
            invalidOrdersCount = validationStats.invalidOrdersCount
          )
        }

      case Some(sel: OrderDirNavigatorItem) =>
        val path = poolSourceProp.value.map(_.path.resolve(sel.relativePath)).getOrElse(sel.relativePath)
        Option(OrderDirectoryDetailData(path = path,
          orderId = sel.displayName,
          validationState = sel.validationState,
          hasInvalidOrderDocs = sel.hasInvalidOrderDocs,
          validationResults = sel.validationResults,
          productInfos = sel.productInfos))

      case Some(sel: OrderDocNavigatorItem) =>
        val orderId = sel.relativePath.getParent.getFileName.toString
        val detailDataOrError: Either[ErrorMsg, DetailData] = createOrderDocDetailData(orderId, orderDocFileName = sel.displayName)
        Some(
          detailDataOrError.fold(
            errorMsg => {
              logger.error(s"failed to load DetailData: ${errorMsg}")
              DetailDataLoadingError(errorMsg)
            },
            detailData => detailData
          )
        )
      case _ => None
    }
    _orderDocDetailDataProp.setValue(detailData)
  }

  private def createOrderDocDetailData(orderId: OrderId, orderDocFileName: String): Either[ErrorMsg, DetailData] = {
    val detailDataOrError: Either[ErrorMsg, DetailData] = orderDocFileName match {
      case Filenames.Order =>
        resourceProvider.get.getOrder(orderId) match {
          case Success(data) => Right(OrderDocDetailData(data))
          case Failure(t) => Left(ErrorMsg("Failed to load order detail data", Some(t.getMessage)))
        }
      case Filenames.OrderResult =>
        resourceProvider.get.getOrderResult(orderId) match {
          case Success(data) => Right(OrderDocDetailData(data))
          case Failure(t) => Left(ErrorMsg("Failed to load order.result detail data", Some(t.getMessage)))
        }
      case Filenames.OrderResultSignature =>
        resourceProvider.get.getOrderResultSignature(orderId) match {
          case Success(data) => Right(OrderDocDetailData(data))
          case Failure(t) => Left(ErrorMsg("Failed to load order.result.signature detail data", Some(t.getMessage)))
        }
      case Filenames.OrderResultSignatureTimestamp =>
        resourceProvider.get.getOrderResultSignatureTimestamp(orderId) match {
          case Success(data) => Right(OrderDocDetailData(data))
          case Failure(t) => Left(ErrorMsg("Failed to load order.result.signature.timestamp detail data", Some(t.getMessage)))
        }
      case Filenames.OrderSignature =>
        resourceProvider.get.getOrderSignature(orderId) match {
          case Success(data) => Right(OrderDocDetailData(data))
          case Failure(t) => Left(ErrorMsg("Failed to load order.signature detail data", Some(t.getMessage)))
        }
      case Filenames.OrderMetadata =>
        val order = resourceProvider.get.getOrder(orderId)
        val orderMetadata = resourceProvider.get.getOrderMetadata(orderId)
        val detailData: Try[OrderHedgingDetailData] = for {o <- order; omd <- orderMetadata} yield {
          OrderHedgingDetailData(o, omd.hedgingData, OrderHedgingDetailData.RowData.create(o, omd.hedgingData))
        }
        detailData match {
          case Success(detailData) => Right(detailData)
          case Failure(f) =>
            Left(ErrorMsg("Failed to load order.metadata details",
              Some(Seq(order, orderMetadata).filter(_.isFailure).map(_.failed.get.getMessage).mkString("\n")))
            )
        }
      case f =>
        Left(ErrorMsg(s"unexpected filename: $f"))
    }
    detailDataOrError
  }

  private def updateValidationViewItems(): Unit = {
    logger.debug(s"updateValidationViewItems()..size before: ${validationViewItemsProp.getValue.size()}")
    _validationViewStateProp.setValue(ValidationViewState.PreparingData)
    selectedNavigatorItemProp.getValue match {
      case Some(item: ArchiveNavigatorItem) =>
        // show ValidationResults for the whole archive
        findTreeItemForFile(navigatorContentRoot, item.relativePath) match {
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
                  name = orderTreeItem.getValue.relativePath.toFile.getName, level = 2)
                orderValidationItemBuilder ++= structuralItem_orderDir +: vResults_singleOrder
              }
            }
            val orderValidationItems = orderValidationItemBuilder.result()
            val structuralItem_pool = ValidationViewStructuralItem(result = None, PoolResource.PRType.PoolDirectory,
              name = archiveTreeItem.getValue.relativePath.toFile.getName, level = 1)
            val poolValidationItems = navigatorContentRoot.getValue.validationResults.map { checkResult =>
              ValidationViewStateItem(checkResult, level = 1)
            }
            _validationViewItemsProp.setAll((structuralItem_pool +: poolValidationItems) ++ orderValidationItems: _*)

          case _ =>
            logger.error(s"updateValidationViewItems()..no TreeItem found for $item")
        }
      case Some(item: OrderDirNavigatorItem) =>
        findTreeItemForFile(navigatorContentRoot, item.relativePath) match {
          case Some(navTreeItem) =>

            val journalItems = collectValidationStateItemsForOrder(orderDirTreeItem = navTreeItem,
              collectErrors = true,
              collectValidationOk = true,
              level = 2)

            if (journalItems.nonEmpty) {
              val orderDirJItem = ValidationViewStructuralItem(
                result = None,
                PoolResource.PRType.OrderDirectory,
                name = item.relativePath.toFile.getName,
                level = 1
              )
              _validationViewItemsProp.setAll(orderDirJItem +: journalItems.toSet.toSeq: _*)
            } else {
              _validationViewItemsProp.clear()
            }
          case _ =>
            logger.error(s"updateValidationViewItems()..no TreeItem found for $item")
        }
      case Some(item: OrderDocNavigatorItem) =>
        // show ValidationResults related to the selected OrderDoc
        findTreeItemForFile(navigatorContentRoot, item.relativePath) match {
          case Some(navTreeItem) =>
            val validationResults = navTreeItem.getValue.validationResults
            val journalItems = validationResults.map(ValidationViewStateItem(_, level = 1))
            _validationViewItemsProp.setAll(journalItems.toSet.toSeq.asInstanceOf[Seq[ValidationViewItem]]: _*)
          case _ =>
            logger.error(s"updateValidationViewItems()..no TreeItem found for $item")
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
            if (validationStats.validatedOrdersCount > 0) {
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
    //remove duplicates since some validation results appear as well on the order-level as on the order-document level
    (orderDirVResults ++ docVResults).toSet.toIndexedSeq
  }

  def resetValidationResults(): Unit = resetAllValidationStateInfos()


  def validateParticipationPool(): Unit = {
    if (validationStats.validatedOrdersCount > 0) {
      resetAllValidationStateInfos()
    }
    _appStateProp.value = AppState.Validating
    updateValidationEnabledProps()
    logger.info(s"validateParticipationPool()..#orderDocFiles: ${getTotalOrdersCount}")

    val validationTaskId = UUID.randomUUID()
    setCurrentBackgroundTask(Some(CancelableMonixTask(cancelable = None, BackgroundTaskDescription_validatePool, id = validationTaskId)))

    val orderValidationCallback = new IntermediateResultCallback {
      override def onOrderValidated(result: OrderValidationResult, countValidatedOrders: Int): Unit = {
        val validationTaskRunning = backgroundTaskInfoProp.getValue match {
          case Some(task: CancelableMonixTask) if task.id == validationTaskId =>
            val newProgress = Math.round((countValidatedOrders / getTotalOrdersCount.toDouble) * 100.0) / 100.0

            updateBackgroundTaskInfoPropIfSoConditioned[CancelableMonixTask](
              validationTaskId, condition = _.progress != newProgress
            ) { oldTaskInfo =>
              Some(oldTaskInfo.withUpdatedProgress(newProgress))
            }

            runLater {
              integrateOrderValidationResults(result, doUpdateValidationViewItems = false)
            }
            true
          case _ =>
            false
        }
      }
    }

    val validationTaskFuture: CancelableFuture[ArchiveValidationResult] = validatorFactory
      .getValidator(resourceProvider.get, credentialsManager, monixScheduler)
      .validatePool(getDrawTime, orderValidationCallback)
      .onCancelRaiseError(new CancellationException("validatePool() was cancelled")).runAsync

    runLater {
      updateBackgroundTaskInfoPropIfSo[CancelableMonixTask](validationTaskId) { oldTask =>
        Some(oldTask.copy(cancelable = Some(validationTaskFuture)))
      }
    }

    validationTaskFuture.onComplete { result =>
      logger.info(s"validateParticipationPool().Task completed")
      runLater {
        setCurrentBackgroundTask(None)
        _appStateProp.value = AppState.Loaded
        result match {
          case Success(archiveValidationResult) =>
            logger.info(s"validateParticipationPool.Task.onComplete()..SUCCESS")
            integratePoolValidationResults(archiveValidationResult.poolValidationResults)

          case Failure(t) =>
            resetAllValidationStateInfos()
            t match {
              case e: CompletionException => Option(e.getCause) match {
                case Some(c: CancellationException) =>
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
    }
    logger.debug("validateParticipationPool()..method FINISHED")
  }

  private def setCurrentBackgroundTask[T](task: Option[TaskInfo]): Unit = {
    logger.info(s"setCurrentBackgroundTask($task)")
    if (task.nonEmpty && backgroundTaskInfoProp.getValue.nonEmpty) {
      logger.error(s"setCurrentBackgroundTask($task) - backgroundTask is not empty: ${_backgroundTaskInfoProp.getValue}")
    }
    _backgroundTaskInfoProp.setValue(task)
  }

  private def updateBackgroundTaskInfoPropIfSo[T <: TaskInfo : ClassTag](id: TaskId)(fUpdate: T => Option[T]): Unit = {
    updateBackgroundTaskInfoPropIfSoConditioned[T](id, condition = _ => true)(fUpdate)
  }

  private def updateBackgroundTaskInfoPropIfSoConditioned[T <: TaskInfo : ClassTag](id: TaskId, condition: T => Boolean)(fUpdate: T => Option[T]): Unit = {
    def conditionMet: Boolean = backgroundTaskInfoProp.value match {
      case Some(task: T) if task.id == id => condition(task)
      case _ => false
    }
    if (conditionMet) {
      runLaterIfNeeded {
        if (conditionMet) {
          _backgroundTaskInfoProp.setValue(fUpdate(backgroundTaskInfoProp.value.get.asInstanceOf[T]))
        }
      }
    }
  }

  def validateSingleOrder(item: OrderDirNavigatorItem): Unit = {
    logger.info(s"validateSingleOrder($item)")
    val orderId = item.relativePath.getFileName.toString
    val taskId = UUID.randomUUID()
    setCurrentBackgroundTask(Some(CancelableMonixTask(cancelable = None, s"Validate selected order", id = taskId)))
    val future = validatorFactory.getValidator(resourceProvider.get, credentialsManager, monixScheduler)
      .validateOrder(orderId, getDrawTime).onCancelRaiseError(new CancellationException("cancelled")).runAsync
    future.onComplete {
      case Success(result) =>
        updateBackgroundTaskInfoPropIfSo[CancelableMonixTask](taskId)(_ => None)
        runLater(integrateOrderValidationResults(result, doUpdateValidationViewItems = true))
      case Failure(t) =>
        val err = s"Failed validate order $orderId: ${t.getMessage}"
        logger.error(err, t)
        updateBackgroundTaskInfoPropIfSo[CancelableMonixTask](taskId)(_ => None)
        publishErrorEvents(ErrorMsg(err))
    }
    updateBackgroundTaskInfoPropIfSo[CancelableMonixTask](taskId)(oldTask => Some(oldTask.copy(cancelable = Some(future))))
  }

  def cancelValidation(): Unit = {
    if (appStateProp.getValue != AppState.Validating) {
      logger.warn(s"cancelValidation()..nothing to do (appState=${appStateProp.getValue})")
    } else {
      _backgroundTaskInfoProp.getValue match {
        case Some(task: CancelableMonixTask) if task.description == BackgroundTaskDescription_validatePool =>
          task.cancelable.foreach(_.cancel())
        case x => logger.warn(s"cancelValidation()..no isCancellable task '$BackgroundTaskDescription_validatePool' found!")
      }
    }
  }

  private def integrateOrderValidationResults(result: OrderValidationResult, doUpdateValidationViewItems: Boolean): Unit = {
    findOrderDirNavTreeItem(result.orderLocation) match {
      case Some(orderDirTreeItem) =>
        integrateOrderValidationResults(orderDirTreeItem, result, doUpdateValidationViewItems)
      case _ => logger.error(s"storeOrderValidationResultInNavTree() for ${result.orderId} failed: no TreeItem found")
    }
  }
  
  private def updateValidationStatsThreadSafe(result: OrderValidationResult): Unit = {
    _validationStats.synchronized {
      val orderWasNotYetValidated = validationStats.validatedOrderIds.add(result.orderId)
      if (orderWasNotYetValidated) {
        validationStats.validatedOrdersCount += 1
        result.isValid match {
          case true => validationStats.validOrdersCount += 1
          case false => validationStats.invalidOrdersCount += 1
        }
      }
    }
  }

  private def integrateOrderValidationResults(
    orderTreeItem: TreeItemExt[NavigatorItem], result: OrderValidationResult, doUpdateValidationViewItems: Boolean
  ): Unit = {
    updateValidationStatsThreadSafe(result)
    attachValidationResults(orderTreeItem, result)
    updateValidationState()
    updateSelectedNavigatorItem()
    if (doUpdateValidationViewItems) {
      updateValidationViewItems()
    }
    updateDetailData()
    updateValidationViewState()
  }

  def integratePoolValidationResults(poolValidationResults: IndexedSeq[CheckResult]): Unit = {
    navigatorContentRoot.getValue match {
      case item: ArchiveNavigatorItem =>
        val updatedItem = item.copy(validationResults = poolValidationResults)
        navigatorContentRoot.setValue(updatedItem)
      case x => logger.error(s"integratePoolValidationResults() unexpected item: $x")
    }
  }

  private def initKeysAndCerts(): Unit = {
    logger.info("initKeysAndCerts()")
    credentialsManager = credentialsManager.withClearedPublicKeys().withClearedCertificates()

    val publicKeyErrors = Seq.newBuilder[ErrorMsg]
    _settings.credentialsSpecs.publicKeyConfigItems.foreach { keySpec =>
      keySpec.file match {
        case UIValue(Some(file), error@None) =>
          val publicKey = Utils.readPublicKeyFromPemFile(file)
          credentialsManager = credentialsManager.withPublicKey(keyId = keySpec.keyId, algorithm = keySpec.algorithm, key = publicKey)
          publicKey.failed.foreach { t =>
            publicKeyErrors += ErrorMsg(s"Error loading public key ${keySpec.keyId}", detail = Some(t.getMessage), exception = Some(t))
          }
        case x =>
          publicKeyErrors += ErrorMsg(s"Error loading public key ${keySpec.keyId}",
            detail = keySpec.file.error.map(_.message), exception = keySpec.file.error.flatMap(_.exception))
      }
    }

    val certificateErrors = Seq.newBuilder[ErrorMsg]
    _settings.credentialsSpecs.certConfigItems.foreach { certSpec =>
      certSpec.file match {
        case UIValue(Some(file), error@None) =>
          val cert = Utils.readX509CertificateFromPemFile(file)
          credentialsManager = credentialsManager.withCertificate(certSpec.name, cert)
          cert.failed.foreach { t =>
            certificateErrors += ErrorMsg(s"Error loading certificate ${certSpec.name}", detail = Some(t.getMessage), exception = Some(t))
          }
        case _ =>
          certificateErrors += ErrorMsg(s"Error loading loading certificate ${certSpec.name}",
            detail = certSpec.file.error.map(_.message), exception = certSpec.file.error.flatMap(_.exception))
      }
    }

    val tsCertErrors = Seq.newBuilder[ErrorMsg]
    val tsCerts: Seq[X509Certificate] = _settings.credentialsSpecs.timestamperCertConfigItems.flatMap { certSpec =>
      certSpec.file match {
        case UIValue(Some(file), error@None) =>
          val cert = Utils.readX509CertificateFromPemFile(file)
          credentialsManager = credentialsManager.withCertificate(certSpec.name, cert)
          cert.failed.foreach { t =>
            tsCertErrors += ErrorMsg(s"Error loading timestamper certificate ${certSpec.name}", detail = Some(t.getMessage), exception = Some(t))
          }
          cert.toOption
        case _ =>
          tsCertErrors += ErrorMsg(s"Error loading loading timestamper certificate ${certSpec.name}",
            detail = certSpec.file.error.map(_.message), exception = certSpec.file.error.flatMap(_.exception))
          None
      }
    }
    credentialsManager = credentialsManager.withTimestamperCertificates(tsCerts)
    val errors = publicKeyErrors.result() ++ certificateErrors.result()
    if (errors.nonEmpty) {
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
    **/
  private def findOrderDirNavTreeItem(path: Path): Option[TreeItemExt[NavigatorItem]] = {
    val dummyTreeItem4BinarySearch = new TreeItemExt[NavigatorItem](
      OrderDirNavigatorItem(relativePath = path, retailer = Symbol(""), origin=None, retailerOrderReference = "", validationState = ValidationState.NotValidated,
        hasInvalidOrderDocs = false, validationResults = EmptyValidationResults, productInfos = None)
    )
    val index = Collections.binarySearch(navigatorContentRoot.filteredChildren_source, dummyTreeItem4BinarySearch,
      navItemFileNameComparator)
    if (index < 0) None else Option(navigatorContentRoot.filteredChildren_source.get(index))
  }

  /**
    * Stores a single `OrderValidationResult` in the Navigator-tree.
    **/
  private[model] def storeOrderValidationResultInNavTree(result: OrderValidationResult): Unit = {
    findOrderDirNavTreeItem(result.orderLocation) match {
      case Some(orderDirTreeItem) =>
        attachValidationResults(orderDirTreeItem, result)
      case _ => logger.error(s"storeOrderValidationResultInNavTree() for ${result.orderId} failed: no TreeItem found")
    }
  }

  private def attachValidationResults(
    orderTreeItem: TreeItemExt[NavigatorItem],
    result: OrderValidationResult
  ): Unit = {

    import PoolResource.PRType.{OrderDirectory, PoolDirectory}

    val hasInvalidOrderDocs = result.checkResults.exists(r => !r.isOk && r.check.affectedResources.filterNot(_ == OrderDirectory).nonEmpty)
    val orderLevelVResults: IndexedSeq[CheckResult] = {
      val orderDocValidationResults = result.checkResults.filter(!_.check.affectedResources.contains(OrderDirectory))
      if (hasInvalidOrderDocs) orderDocValidationResults else {
        if (AllOrderDocumentValidationsOk.isEmpty) {
          AllOrderDocumentValidationsOk = Some(orderDocValidationResults)
        }
        if (AllOrderDocumentValidationsOk.get == orderDocValidationResults) AllOrderDocumentValidationsOk.get else orderDocValidationResults
      }
      orderDocValidationResults
    }

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
        orderTreeItem.getChildren.find(_.getValue.relativePath.toFile.getName == docFileName) match {
          case Some(docTreeItem) =>
            docTreeItem.setValue(docTreeItem.getValue.withValidationResults(docSpecificVResults))
          case _ =>
            logger.debug(s"no docTreeItem found for $docFileName")
        }
    }
  }

  private def findTreeItemForFile(treeItem: TreeItemExt[NavigatorItem], path: Path): Option[TreeItemExt[NavigatorItem]] = {
    require(treeItem != null, "treeItem is null!")
    require(treeItem.value != null, "treeItem.value is null!")
    require(path != null, "file is null!")

    val treeItemPath = treeItem.getValue.relativePath
    if (!treeItemPath.toString.isEmpty && !path.startsWith(treeItem.getValue.relativePath)) {
      None
    } else {
      if (treeItem.getValue.relativePath == path)
        Some(treeItem)
      else {
        treeItem.filteredChildren_source.find {
          child =>
            path.startsWith(child.getValue.relativePath)
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
  def getArchiveFileSelectorInitialDirectory: Option[Path] = poolSourceProp.value.fold(
    lastOpenResourcePath.map(Some(_)).getOrElse(Option(System.getProperty("user.dir")).map(new File(_).toPath))
  )(x => Some(x.path))
    .map( path => if(path.toFile.isFile) path.getParent else path)

  /**
    * Delivers the initial directory for the `DirectoryChooser` when an archive is to be loaded.
    **/
  def getArchiveDirectorySelectorInitialDirectory: Option[Path] = getArchiveFileSelectorInitialDirectory

  def getPoolValidationErrorsCount: Int = Option(navigatorContentRoot.getValue).map(_.validationResults.count(!_.isOk)).getOrElse(0)

  def hasPoolValidationErrors: Boolean = getPoolValidationErrorsCount > 0

  def validationStats: ValidationStats = _validationStats

  /**
    * Delivers the total order count of the loaded pool archive (without considering any Navigator-filters).
    **/
  def getTotalOrdersCount: Int = archiveStats.totalOrdersCount

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

  def cancelTask(taskId: TaskId): Unit = {
    logger.info(s"cancelTask($taskId)")
    this._backgroundTaskInfoProp.getValue match {
      case Some(taskInfo: CancelableMonixTask) if (taskInfo.id == taskId) =>
        taskInfo.cancelable.foreach(_.cancel())
      case _ => logger.warn(s"cancelTask() no task found with id=$taskId")
    }
  }

  private def updateCanOpenArchiveProp(): Unit = {
    _canOpenArchiveProp.setValue((_settings != null) && _settings.archiveExtractionTarget.isValid)
  }

  private def updateValidationEnabledProps(): Unit = {
    val canValidateArchive = poolSourceProp.getValue.nonEmpty && appStateProp.value != AppState.Validating
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
    _validationStats = new ValidationStats()
    _cachedArchiveDetailData = None
  }

  /**
    * Resets all ValidationState (e.g. in navigatorContentRoot, errorJournalProp, selectedNavigatorItem)
    **/
  private def resetAllValidationStateInfos(): Unit = {
    resetCachedArchiveData()

    _validationViewItemsProp.clear()
    _validationStateProp.setValue(ValidationState.NotValidated)
    _selectedNavigatorItemProp.value = selectedNavigatorItemProp.value.map(_.withValidationResults(EmptyValidationResults))

    def udpateTreeItemValue(item: TreeItemExt[NavigatorItem], fUpdate: NavigatorItem => NavigatorItem): Unit = {
      item.setValue(fUpdate(item.getValue))
      item.filteredChildren_source.foreach { childNode => udpateTreeItemValue(childNode, fUpdate) }
    }

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

  private def runLaterIfNeeded(block: => Unit): Unit = if (Platform.isFxApplicationThread()) block else runLater(block)

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
        filterStr =>
          genericPredicateOf { treeItem =>
            val displayNameMatches = treeItem.getValue.displayName.contains(filterStr)
            displayNameMatches || 
              matchOrderDirNavigatorItem(treeItem)(_.retailerOrderReference.contains(filterStr)) ||
              matchOrderDirNavigatorItem(treeItem)(_.retailer.name.contains(filterStr)) ||
              matchOrderDirNavigatorItem(treeItem)(_.origin.map(_.name.contains(filterStr)).contains(true)) ||
              matchOrderDirNavigatorItem(treeItem)(_.productInfos.map(_.betCountPerProduct.keySet.exists(_.name.contains(filterStr))).contains(true))
          }
      }
      copy(activeFilters = calcActiveFilters)
    }
    
    private def matchOrderDirNavigatorItem(treeItem: TreeItem[NavigatorItem])(pred: OrderDirNavigatorItem => Boolean): Boolean = treeItem.getValue match {
      case o: OrderDirNavigatorItem =>
        pred(o)
      case _ => false
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

  def dispose(): Unit = {
    disposed = true
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

  class ArchiveStats(
    var totalOrdersCount: Int = 0,
    val betCountBreakdown: mutable.Map[GamingProductId, mutable.Map[Retailer, mutable.Map[Option[Origin], Int]]] = mutable.Map.empty[GamingProductId, mutable.Map[Retailer, mutable.Map[Option[Origin], Int]]],
    val ordersCountBreakdown: mutable.Map[Retailer, mutable.Map[Option[Origin], Int]] = mutable.Map.empty[Retailer, mutable.Map[Option[Origin], Int]]
  ) {
    def getBetCountBreakdown: Map[GamingProductId, Map[Retailer, Map[Option[Origin], Int]]] = betCountBreakdown.mapValues(_.mapValues(_.toMap).toMap).toMap
    
    def getBetCountsPerProduct: Map[GamingProductId, Int] = betCountBreakdown.mapValues(_.values.map(_.values.sum).sum).toMap
    
    def getOrdersCountBreakdown: Map[Retailer, Map[Option[Origin], Int]] = ordersCountBreakdown.mapValues(_.toMap).toMap
  }
  
  class ValidationStats(
    val validatedOrderIds: mutable.HashSet[String] = mutable.HashSet.empty[String],
    var validatedOrdersCount: Int = 0,
    var invalidOrdersCount: Int = 0,
    var validOrdersCount: Int = 0
  )
  
  sealed trait PoolSource {
    def path: Path
  }

  case class PoolSourceDirectory(path: Path) extends PoolSource

  case class PoolSourceArchive(path: Path) extends PoolSource

  type TaskId = UUID

  trait TaskInfo {
    def id: TaskId

    def description: String

    def isCancellable: Boolean

    def progress: Double

    def withUpdatedProgress(newValue: Double): TaskInfo
  }

  case class CancelableMonixTask(cancelable: Option[Cancelable], description: String, id: TaskId, progress: Double = 0) extends TaskInfo {
    override def isCancellable: Boolean = cancelable.nonEmpty

    override def withUpdatedProgress(newValue: Double): CancelableMonixTask = copy(progress = newValue)
  }
}
