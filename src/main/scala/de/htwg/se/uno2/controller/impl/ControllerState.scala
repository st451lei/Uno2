package de.htwg.se.uno2.controller.impl

trait ControllerState:
  def playCard(controller: Controller, index: Int): Unit
  def drawCard(controller: Controller): Unit
  def chooseColor(controller: Controller, input: String): Unit

object NormalState extends ControllerState:

  override def playCard(controller: Controller, index: Int): Unit =
    controller.playCardInternal(index)

  override def drawCard(controller: Controller): Unit =
    controller.drawCardInternal()
    
  override def chooseColor(controller: Controller, token: String): Unit =
    ()
    
object AwaitingColorState extends ControllerState:
  override def playCard(controller: Controller, index: Int): Unit =
    ()
    
  override def drawCard(controller: Controller): Unit =
    ()
    
  override def chooseColor(controller: Controller, token: String): Unit =
    controller.chooseColorInternal(token)
    
  
