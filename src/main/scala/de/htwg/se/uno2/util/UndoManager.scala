package de.htwg.se.uno2.util

class UndoManager:
  private var undoStack: List[Command] = Nil
  private var redoStack: List[Command] = Nil
  
  def doStep(command: Command): Unit =
    undoStack = command :: undoStack
    redoStack = Nil
    command.doStep()
    
  def undoStep(): Unit =
    undoStack match
      case Nil => ()
      case head :: tail =>
        head.undoStep()
        undoStack = tail
        redoStack = head :: redoStack
        
  def redoStep(): Unit =
    redoStack match
      case Nil => ()
      case head :: tail =>
        head.redoStep()
        redoStack = tail
        undoStack = head :: undoStack
