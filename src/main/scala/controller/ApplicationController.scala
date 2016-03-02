package controller

import java.io.File
import java.lang.Boolean
import java.time.ZonedDateTime
import javafx.beans.property.ReadOnlyProperty
import javafx.beans.value.{ChangeListener, ObservableValue}

import model.ApplicationModel.AppState
import model._
import util.ThrottlingHelper
import view.ApplicationView

import scala.concurrent.duration._
import scalafx.application.Platform
import scalafx.event.subscriptions.Subscription



class ApplicationController {
  private var appModel: ApplicationModel = _
  private var appView: ApplicationView = _
  private val detailDataThrottling = new ThrottlingHelper[Option[DetailData]](200 millis)

  locally {
    detailDataThrottling.setDelegate{ data => appView.setDetailData(data) }
  }

  def assign(applicationModel: ApplicationModel, applicationView: ApplicationView): Unit = {
    this.appModel = applicationModel
    this.appView = applicationView
    appView.init(this)

    wirePropWithInit(appModel.validationViewFilterOptionsProp) { (_, newVal) =>
      appView.validationStateView.setFilterOptions(newVal)
    }

    wirePropWithInit(appModel.showUIDebugControlsProp) { (_, newVal) =>
      appView.setShowUIDebugControls(newVal)
    }

    wirePropWithInit(appModel.validationViewStateProp) { (_, newVal) =>
      appView.validationStateView.setValidationViewState(newVal)
    }

    wirePropWithInit(appModel.selectedNavigatorItemProp) { (_, newVal) =>
      appView.setSelectedNaviagorItem(newVal)
    }

    wirePropWithInit(appModel.drawDateInfosProp) { (_, newVal) =>
      appView.setDrawDateInfos(newVal)
    }

    wirePropWithInit(appModel.validateSingleOrderEnabledProp) { (_, newVal) =>
      appView.setSingleOrderValidationEnabled(newVal)
    }

    wirePropWithInit(appModel.validateArchiveEnabledProp) { (oldVal, newVal) =>
      appView.setArchiveValidationEnabled(newVal)
    }

    wirePropWithInit(appModel.navigatorFilterOptions) { (oldVal, newVal) =>
      appView.navigator.setFilterOptions(newVal)
    }

    wirePropWithInit(appModel.orderDocDetailDataProp) { (oldVal, newVal) =>
      //during `AppState.Validating` the DetailView can only be updated with a limited rate ( => UI-reactivity)
      detailDataThrottling.update(newVal)
    }

    wirePropWithInit(appModel.appStateProp) { (oldState, newState) =>
      appView.setAppState(newState)

      //during `AppState.Validating` the DetailView can only be updated with a limited rate ( => UI-reactivity)
      detailDataThrottling.setThrottlingEnabled(
        enabled = (AppState.Validating == newState),
        deliverOnDisabling = true
      )
    }

    wirePropWithInit(appModel.navigatorContentRootProp){ (_, newContentRoot) =>
      appView.navigator.setRootItem(newContentRoot)
    }

    wirePropWithInit(appModel.backgroundTaskInfoProp) { (oldInfo, newInfo) =>
      appView.validationStateView.updateBackgroundTaskInfo(newInfo)
    }

    wirePropWithInit(appModel.poolSourceProp) { (_, newVal) =>

      appView.setPoolSource(newVal)
    }

    appView.validationStateView.setItems(appModel.validationViewItemsProp)

    appModel.addErrorEventHandler { errorEvents =>
      appView.showErrorDialog(errorEvents)
    }
  }

  /**
    * Creates an `ChangeListener[T]` which invokes `fValueChange` on changes. Furthermore, `fValueChange` is invoked once
    * with `prop.getValue`.
    **/
  def wirePropWithInit[T](prop: ReadOnlyProperty[T])(fValueChange: (T, T) => Unit): Subscription = {
    val listener = new ChangeListener[T] {
      def changed(observable: ObservableValue[_ <: T], oldValue: T, newValue: T): Unit = {
        fValueChange(oldValue, newValue)
      }
    }
    fValueChange.apply(null.asInstanceOf[T], prop.getValue)
    prop.addListener(listener)
    new Subscription() {
      override def cancel(): Unit = prop.removeListener(listener)
    }
  }

  // =======================================================================================================
  // View-to-Model communication methods
  // =======================================================================================================

  def loadPoolArchive(file: File): Unit = {
    appModel.loadPoolArchive(file)
  }

  def loadPoolDirectory(file: File): Unit = {
    appModel.loadPoolDirectory(file)
  }

  def setSelectedNavigatorItem(selected: Option[NavigatorItem]): Unit = {
    appModel.setSelectedNavigatorItem(selected)
  }

  def setShowValidOrders(show: Boolean): Unit = {
    appModel.setShowValidOrders(show)
  }

  def setNavigatorFilterText(filter: Option[String]): Unit = {
    appModel.setNavigatorFilterText(filter)
  }

  def setShowInvalidOrders(show: Boolean): Unit = {
    appModel.setShowInvalidOrders(show)
  }

  def setShowUnvalidatedOrders(show: Boolean): Unit = {
    appModel.setShowUnvalidatedOrders(show)
  }

  def setCustomDrawDate(value: Option[ZonedDateTime]): Unit = {
    appModel.setCustomDrawDate(value)
  }

  def setValidationStateViewFilterOptions(filterOptions: ValidationViewFilterOptions): Unit = {
    appModel.setValidationViewFilterOptions(filterOptions)
  }

  def validateSelectedOrder(): Unit = {
    appModel.selectedNavigatorItemProp.getValue match {
      case Some(item: OrderDirNavigatorItem) => appModel.validateSingleOrder(item)
      case _ =>
    }
  }

  def resetValidationResults(): Unit = {
    appModel.resetValidationResults()
  }

  def validateAllOrders(): Unit = {
    appModel.validateParticipationPool()
  }

  def cancelValidation(): Unit = {
    appModel.cancelValidation()
  }

  def isValidateSingleOrderEnabled: Boolean = {
    appModel.validateArchiveEnabledProp.getValue
  }

  def validateSingleOrder(item: NavigatorItem): Unit = {
    appModel.validateSingleOrder(item)
  }

  def requestOpenSettingsView(): Unit = {
    appView.editSettings(appModel.getSettings, showUIDebugControls = appModel.showUIDebugControlsProp.getValue)
  }

  def requestOpenArchiveSelector(): Unit = {
    appView.showArchiveFileSelector(appModel.getArchiveFileSelectorInitialDirectory)
  }

  def requestOpenPoolDirectorySelector(): Unit = {
    appView.showArchiveDirectorySelector(appModel.getArchiveDirectorySelectorInitialDirectory)
  }

  def closeArchive(): Unit = {
    appModel.unload()
  }

  def updateSettings(settings: ApplicationSettings): Unit = {
    appModel.setSettings(settings)
  }

  def cancelTask(taskId: String): Unit = {
    appModel.cancelTask(taskId)
  }

  def exit(): Unit = {
    appModel.dispose()
    Platform.exit()
  }

}


