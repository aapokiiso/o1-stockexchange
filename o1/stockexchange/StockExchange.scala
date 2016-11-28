package o1.stockexchange

import scala.collection.mutable.Buffer
import scala.collection.mutable.{Map => MutableMap}
import scala.collection.immutable.{Map => ImmutableMap}

class StockExchange {
  
  private val stockLoader = new StockLoader(StockExchange.StocksPath)
  private val stocksList: ImmutableMap[Int, String] = this.stockLoader.stocksList(StockExchange.StocksListFile)
  private val companies: ImmutableMap[Int, Company] = this.companiesFromStocks(this.stocksList)
  private val quarters: Vector[Quarter] = this.quartersFromStocks(this.stocksList)
  
  private def companiesFromStocks(stocksList: Map[Int, String]): ImmutableMap[Int, Company] = {
    stocksList.map((s) => s._1 -> new Company(s._1, s._2))
  }
  
  private def quartersFromStocks(stocksList: Map[Int, String]): Vector[Quarter] = {
    val quartersData = MutableMap[String, Buffer[QuarterStock]]()
    
    // Combined quarters data from all stocks.
    for ((stockId, companyName) <- this.stocksList) {
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
  
  val StocksPath = "stocks"
  val StocksListFile = "index.txt"
  
}