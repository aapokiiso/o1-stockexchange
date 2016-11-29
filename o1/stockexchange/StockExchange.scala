package o1.stockexchange

import scala.collection.immutable.Map

class StockExchange {
  
  val title = "OMXH Wild Ride"
  
  private val stockLoader = new StockLoader
  
  val companies: Map[Int, Company] = this.stockLoader.companies
  val quarters: Vector[Quarter] = this.stockLoader.quarters
  val broker: Broker = new Broker()
  
  private var currentQuarter: Option[Quarter] = Some(this.quarters.head)
  
  /**
   * Game ends when going over last quarter.
   */
  def isOver = !currentQuarter.isDefined
  
  def quarter = this.currentQuarter
  
  def welcomeMessage = {
    s"Welcome to '${this.title}'!\n" + 
    "Your objective as an armchair stock broker is to invest in companies on the Helsinki stock market.\n" +
    s"Investing starts from the quarter ${this.quarters.head.name} and spans until ${this.quarters.last.name}.\n" +
    s"Luckily you don't have to start empty-handed, as your father has given you a small loan of ${this.broker.capital} markka.\n" +
    "If you need help with playing, type 'help' into the console. Good luck, and invest your money wisely!"
  }
  
  def goodbyeMessage = {
    ""
  }
  
  def playTurn(command: String): String = {
    val action = Action(command)
    action.execute(this)
  }
  
}
