package o1.stockexchange

import scala.collection.mutable.Map

class StockExchange {
  
  private val _stockLoader = new StockLoader(StockExchange.StocksPath)
  private val _stocks: Map[Int, String] = _stockLoader.loadStocksFromFile(StockExchange.StocksListFile)
  
  // @todo loop through stocks, load them and create quarter instances etc
  val stock = this._stockLoader.loadStock("1000.txt")
  println(stock)
  
}

object StockExchange {
  
  val StocksPath = "stocks"
  val StocksListFile = "index.txt"
  
}