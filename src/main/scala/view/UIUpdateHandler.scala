package view

/**
  * Helper trait used to avoid notification-loops and control-flow errors that may occur when UI-classes
  * or `Controls` are updated in response to `Property`-changes in the UI.
  * */
trait UIUpdateHandler {

  private var uiUpdateCounter = 0

  def whenModifiedByUserInput(controllerCall: => Unit): Unit = {
    if(uiUpdateCounter == 0){
      controllerCall
    }
  }

  def updateUI(uiUpdateBlock: => Unit): Unit = {
    uiUpdateCounter += 1
    uiUpdateBlock
    uiUpdateCounter -= 1
  }
}