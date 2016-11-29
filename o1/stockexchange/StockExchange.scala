package o1.stockexchange

import scala.collection.mutable.Buffer
import scala.collection.mutable.{Map => MutableMap}
import scala.collection.immutable.{Map => ImmutableMap}

class StockExchange {
  
  val title = "OMXH Wild Ride"
  
  private var _isOver = false
  
  private val stockLoader = new StockLoader(StockExchange.StocksPath)
  private val stocksList: ImmutableMap[Int, ImmutableMap[String, String]] = this.stockLoader.stocksList(StockExchange.StocksListFile)
  
  val companies: ImmutableMap[Int, Company] = this.companiesFromStocks(this.stocksList)
  
  val quarters: Vector[Quarter] = this.quartersFromStocks(this.stocksList)
  private var currentQuarter: Quarter = this.quarters.head
  
  val broker: Broker = new Broker(initialCapital = StockExchange.BrokerInitialCapital)
  
  def isOver = this._isOver
  
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
  
  def quarter = this.currentQuarter
  
  private def companiesFromStocks(stocksList: ImmutableMap[Int, ImmutableMap[String, String]]): ImmutableMap[Int, Company] = {
    stocksList.map((s) => s._1 -> new Company(s._1, s._2("ticker"), s._2("name")))
  }
  
  private def quartersFromStocks(stocksList: ImmutableMap[Int, ImmutableMap[String, String]]): Vector[Quarter] = {
    val quartersData = MutableMap[String, Buffer[QuarterStock]]()
    
    // Combined quarters data from all stocks.
    for (stockId <- this.stocksList.keys) {
      val stockQuarters = this.stockLoader.stockQuarters(stockId + ".txt")
      
      for ((quarterName, averagePrice) <- stockQuarters) {
        val stock = new QuarterStock(this.companies(stockId), averagePrice)
        
        quartersData.get(quarterName) match {
          case Some(quarterStocks) => quarterStocks += stock
          case None => {
            // New quarter, initialize stocks buffer.
            quartersData(quarterName) = Buffer[QuarterStock](stock)
          }
        }
      }
    }
    
    val quarters = quartersData.toVector.map((q) => new Quarter(q._1, q._2.toVector))
    
    // Sort quarters chronologically (1/1990, 3/1991, ... 4/2016)
    val sortedQuarters = quarters.sortWith(_ < _)
    
    sortedQuarters
  }
  
}

object StockExchange {
  
  val StocksPath = "resources/stocks"
  val StocksListFile = "index.txt"
  
  val BrokerInitialCapital = 20000.00
  
}