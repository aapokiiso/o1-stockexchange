package o1.stockexchange

import scala.collection.mutable.Buffer
import scala.collection.mutable.{Map => MutableMap}

class StockExchange {
  
  private val stockLoader = new StockLoader(StockExchange.StocksPath)
  private val stocks: MutableMap[Int, String] = this.stockLoader.stocksList(StockExchange.StocksListFile)
  private val quarters: Buffer[Quarter] = Buffer[Quarter]()
  
  for ((stockId, companyName) <- this.stocks) {
    val stockQuarters = this.stockLoader.stockQuarters(stockId + ".txt")
    
    for ((quarterName, averagePrice) <- stockQuarters) {
      this.quarterByName(quarterName) match {
        case Some(quarter) => quarter.stocks(stockId) = averagePrice
        case None => {
          val quarter = new Quarter(quarterName)
          quarter.stocks(stockId) = averagePrice
          this.quarters += quarter
        }
      }
    }
  }
  
  private def quarterByName(name: String): Option[Quarter] = this.quarters.find(_.name == name)
  
}

object StockExchange {
  
  val StocksPath = "stocks"
  val StocksListFile = "index.txt"
  
}