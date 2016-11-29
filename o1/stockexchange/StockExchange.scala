package o1.stockexchange

import scala.collection.immutable.Map

class StockExchange {
  
  val title = "OMXH Wild Ride"
  
  private val stockLoader = new StockLoader
  
  val companies: Map[Int, Company] = this.stockLoader.companies
  val quarters: Iterator[Quarter] = this.stockLoader.quarters
  val broker: Broker = new Broker()
  
  private var currentQuarter: Option[Quarter] = None
  this.nextQuarter()
  
  /**
   * Game ends when going over last quarter.
   */
  def isOver = !currentQuarter.isDefined
  
  def quarter = this.currentQuarter
  
  def welcomeMessage = {
    s"Welcome to ${this.title}!\n" + 
    "Your job, as an armchair stock broker, is to invest in companies on the Helsinki stock market.\n" +
    "Investing starts from the mid-90s, spanning through the IT-buble and its crash, the subsequent years of economic growth\n" +
    "and the 2008 housing bubble, all the way to the present day (late fall of 2016).\n" +
    s"Luckily you don't have to start empty-handed, as your father has given you a small loan of ${this.broker.capital} markka.\n" +
    "This game has been certified SFV (Safe For Väyrynen), as no markkas will be turned to euros at any time.\n\n" + 
    "If you need help with playing, type 'help' into the console. Good luck, and invest your money wisely!"
  }
  
  def goodbyeMessage = {
    "@todo"
  }
  
  def playTurn(command: String): String = {
    val action = Action(command)
    action.execute(this)
  }

  def nextQuarter(): Unit = {
    if (this.quarters.hasNext) {
      val nextQuarter = this.quarters.next()
      this.currentQuarter = Some(nextQuarter)
      this.broker.pricesheet = nextQuarter.pricesheet
    } else {
      this.currentQuarter = None
      this.broker.pricesheet = Map[Company, Double]()
    }
  }
  
  def stockByTicker(ticker: String): Option[QuarterStock] = {
    this.quarter match {
      case Some(quarter) => quarter.stockByTicker(ticker)
      case None => None
    }
  }
  
}

object StockExchange {
  
  def formatPrice(price: Double) = Math.round(price * 100.00) / 100.00
  
  def formatPercent(percent: Double) = Math.round(percent * 100.00) / 1.00
  
}