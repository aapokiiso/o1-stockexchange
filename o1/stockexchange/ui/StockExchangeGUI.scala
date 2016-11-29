package o1.stockexchange.ui

import scala.swing._
import scala.swing.event._
import javax.swing.UIManager
import o1.stockexchange.StockExchange


////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file.
//////////////////////////////////////////////////////////////


object StockExchangeGUI extends SimpleSwingApplication {

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName) 

  def top = new MainFrame {

    // Access to the internal logic of the application: 
  
    val game = new StockExchange
    val player = game.broker
    
    // Components: 

    val quarterInfo = new TextArea(7, 80) {
      editable = false
      wordWrap = true
      lineWrap = true
    }
    val turnOutput = new TextArea(7, 80) {
      editable = false
      wordWrap = true
      lineWrap = true
    }
    val input = new TextField(40) {
      minimumSize = preferredSize
    }
    this.listenTo(input.keys)

    // Events: 

    this.reactions += {
      case keyEvent: KeyPressed =>
        if (keyEvent.source == this.input && keyEvent.key == Key.Enter && !this.game.isOver) {
          val command = this.input.text.trim
          if (command.nonEmpty) {
            this.input.text = ""
            this.playTurn(command)
          }
        }
    }

    // Layout: 

    this.contents = new GridBagPanel { 
      import scala.swing.GridBagPanel.Anchor._
      import scala.swing.GridBagPanel.Fill
      layout += new Label("Location:") -> new Constraints(0, 0, 1, 1, 0, 1, NorthWest.id, Fill.None.id, new Insets(8, 5, 5, 5), 0, 0)
      layout += new Label("Command:")  -> new Constraints(0, 1, 1, 1, 0, 0, NorthWest.id, Fill.None.id, new Insets(8, 5, 5, 5), 0, 0)
      layout += new Label("Events:")   -> new Constraints(0, 2, 1, 1, 0, 0, NorthWest.id, Fill.None.id, new Insets(8, 5, 5, 5), 0, 0)
      layout += quarterInfo           -> new Constraints(1, 0, 1, 1, 1, 1, NorthWest.id, Fill.Both.id, new Insets(5, 5, 5, 5), 0, 0)
      layout += input                  -> new Constraints(1, 1, 1, 1, 1, 0, NorthWest.id, Fill.None.id, new Insets(5, 5, 5, 5), 0, 0)
      layout += turnOutput             -> new Constraints(1, 2, 1, 1, 1, 1, SouthWest.id, Fill.Both.id, new Insets(5, 5, 5, 5), 0, 0)
    }
    
    // Menu:
    
    this.menuBar = new MenuBar {
      contents += new Menu("Program") {
        val quitAction = Action("Quit") { dispose() }
        contents += new MenuItem(quitAction)
      }
    }  

    // Set up the initial state of the GUI:
    
    this.title = game.title
    this.updateInfo(this.game.welcomeMessage)
    this.location = new Point(50, 50)
    this.minimumSize = new Dimension(200, 200)
    this.pack()
    this.input.requestFocusInWindow()

    
    def playTurn(command: String) = {
      val turnReport = this.game.playTurn(command)
      if (this.player.hasQuit) {
        this.dispose()
      } else {
        this.updateInfo(turnReport)
        this.input.enabled = !this.game.isOver
      }
    }
    
    def updateInfo(info: String) = {
      if (!this.game.isOver) {
        this.turnOutput.text = info
      } else {
        this.turnOutput.text = info + "\n\n" + this.game.goodbyeMessage
      }
      this.quarterInfo.text = this.game.quarter.fullDescription
    }

    
  }
    
}  
  
