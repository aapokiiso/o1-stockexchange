package o1.stockexchange

import scala.io.Source
import scala.collection.mutable.Map

class StockLoader(val stocksPath: String) {
  
  def loadStocksFromFile(stocksFile: String): Map[Int, String] = {
    val filePath = this.stocksPath + StockLoader.DirSeparator + stocksFile
    val stocksSource = Source.fromFile(filePath)
    val stocks = Map[Int, String]()
    
    try {
      for (line <- stocksSource.getLines()) {
        val lineParts = line.split(StockLoader.CsvSeparator)
        val stockId = lineParts(0).toInt
        val stockName = lineParts(1)
        stocks(stockId) = stockName
      }
    } finally {
      stocksSource.close()
    }
    
    stocks
  }
  
  def loadStock(stockFile: String): Map[String, Double] = {
    val filePath = this.stocksPath + StockLoader.DirSeparator + stockFile
    val stockSource = Source.fromFile(filePath)
    val stockQuarters = Map[String, Double]()
    
    try {
      // Skip first line (header line)
      val stockDayLines = stockSource.getLines().toVector.tail
      
      val stockDaysInQuarters = this._groupStockDaysInQuarters(stockDayLines)
      
      // Add quarters to map with average price.
      for (stockQuarter <- stockDaysInQuarters) {
        val quarterName = stockQuarter._1
        val quarterDayLines = stockQuarter._2
        val quarterAveragePrice = this._getStockDaysAveragePrice(quarterDayLines)
        
        stockQuarters(quarterName) = quarterAveragePrice
      }
    } finally {
      stockSource.close()
    }
    
    stockQuarters
  }
  
  private def _groupStockDaysInQuarters(stockDayLines: Vector[String]) = {
    val groupedStockDays = stockDayLines.groupBy((line: String) => {
      val stockDateStr = line.split(StockLoader.CsvSeparator)(0);
      
      // Use quarter name (ex. "2/2015") to group.
      this._getQuarterNameByDateStr(stockDateStr)
    })
    
    groupedStockDays
  }
  
  private def _getStockDaysAveragePrice(stockDayLines: Vector[String]) = {
    val pricesSum = stockDayLines.foldLeft(0.0)((sum: Double, line: String) => {
      val closingPriceStr = line.split(StockLoader.CsvSeparator)(4)
      val closingPrice = closingPriceStr.replace(',', '.').toDouble
      
      sum + closingPrice
    })
    
    pricesSum / stockDayLines.size
  }

  private def _getQuarterNameByDateStr(dateStr: String): String = {
    val date = StockLoader.StockDateFormat.parse(dateStr)
    val cal = StockLoader.StockDateCalendar
    cal.setTime(date)
    val month = cal.get(java.util.Calendar.MONTH)
    val year = cal.get(java.util.Calendar.YEAR)
    
    ((month / 3) + 1) + "/" + year
  }
  
}

object StockLoader {
  
  val DirSeparator = "/"
  val CsvSeparator = ";"
  
  val StockDateFormat = new java.text.SimpleDateFormat("dd.MM.yyyy")
  val StockDateCalendar = java.util.Calendar.getInstance()
  
}