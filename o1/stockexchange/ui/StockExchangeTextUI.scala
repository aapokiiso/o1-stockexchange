package o1.stockexchange.ui

import o1.stockexchange._
import scala.io.StdIn._

object StockExchangeTextUI extends App {
  
  private val game = new StockExchange
  private val player = game.broker
  this.run()

  
  private def run() = {
    println(this.game.welcomeMessage)
    while (!this.game.isOver) {
      this.playTurn()
    } 
    println("\n" + this.game.goodbyeMessage)
  }


  private def playTurn() = {
    println()
    val command = readLine("Command: ")
    val turnReport = this.game.playTurn(command)
    println(turnReport) 
  }

}


