package o1.stockexchange

import scala.collection.mutable.{Map => MutableMap}
import scala.collection.immutable.{Map => ImmutableMap}

class StockExchange {
  
  private val stockLoader = new StockLoader(StockExchange.StocksPath)
  private val stocksList: ImmutableMap[Int, String] = this.stockLoader.stocksList(StockExchange.StocksListFile)
  private val quarters: Vector[Quarter] = this.quartersFromStocks(this.stocksList)
  
  private def quartersFromStocks(stocksList: Map[Int, String]): Vector[Quarter] = {
    val quartersData = MutableMap[String, MutableMap[Int, Double]]()
    
    // Combined quarters data from all stocks.
    for ((stockId, companyName) <- this.stocksList) {
      val stockQuarters = this.stockLoader.stockQuarters(stockId + ".txt")
      
      for ((quarterName, averagePrice) <- stockQuarters) {
        quartersData.get(quarterName) match {
          case Some(quarterStocks) => quarterStocks(stockId) = averagePrice
          case None => {
            quartersData(quarterName) = MutableMap[Int, Double](stockId -> averagePrice)
          }
        }
      }
    }
    
    // Map quarters data to Quarter objects.
    val quarters = quartersData.map((quarter) => {
      val quarterName = quarter._1
      val quarterStocks = quarter._2
      
      new Quarter(quarterName, quarterStocks.toMap)
    })
    
    // Sort quarters chronologically (1/1990, 3/1991, ... 4/2016)
    val sortedQuarters = quarters.toVector.sortWith(_ < _)
    
    sortedQuarters
  }
  
}

object StockExchange {
  
  val StocksPath = "stocks"
  val StocksListFile = "index.txt"
  
}