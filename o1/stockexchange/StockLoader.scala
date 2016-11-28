package o1.stockexchange

import scala.io.Source
import scala.collection.immutable.Map

class StockLoader(val stocksPath: String) {
  
  /**
   * Returns map of stock IDs and company names.
   * 
   * Map format: stock ID -> Company data (ticker, name)
   * 
   * Stock ID is nothing else but a numeric ID
   * used by Kauppalehti to identify stocks.
   * See "scrape-stocks.js" in this project's root for details.
   * 
   * @param stocksFile - path to file to load stock data from,
   * 										 relative to the stock loader's base path.
   */
  def stocksList(stocksFile: String): Map[Int, Map[String, String]] = {
    val filePath = this.stocksPath + StockLoader.DirSeparator + stocksFile
    val stocksSource = Source.fromFile(filePath)
    
    try {
      val stocks = stocksSource.getLines().map((line) => {
        val stockId = this.stockListValue(line, "id").toInt
        val companyData = Map(
            "ticker" -> this.stockListValue(line, "ticker"), 
            "name" -> this.stockListValue(line, "name")
            )
        stockId -> companyData
      })
    
      stocks.toMap
    } finally {
      stocksSource.close()
    }
  }
  
  /**
   * Returns quarterly average prices for a single stock.
   * We are not interested in day-to-day prices 
   * (which we have on file), but the average quarterly prices.
   * 
   * Map format: Quarter name -> Average price for the quarter
   * Quarter name is of format "quarter/year", example "3/2006"
   * 
   * @param stockFile - path to file to load stock data from,
   * 										relative to the stock loader's base path.
   */
  def stockQuarters(stockFile: String): Map[String, Double] = {
    val filePath = this.stocksPath + StockLoader.DirSeparator + stockFile
    val stockSource = Source.fromFile(filePath)
    
    try {
      // Skip first line (CSV header line)
      val stockDayLines = stockSource.getLines().toVector.tail
      
      val stockDaysInQuarters = this.groupStockDaysInQuarters(stockDayLines)
      
      val stockQuarters = stockDaysInQuarters.map((stockQuarter) => {
        val quarterName = stockQuarter._1
        val quarterDayLines = stockQuarter._2
        val quarterAveragePrice = this.stockDaysAveragePrice(quarterDayLines)
        
        quarterName -> quarterAveragePrice
      })
    
      stockQuarters
    } finally {
      stockSource.close()
    }
  }
  
  private def groupStockDaysInQuarters(stockDayLines: Vector[String]) = {
    val groupedStockDays = stockDayLines.groupBy((line: String) => {
      val stockDateStr = this.stockValue(line, "date")
      // Use quarter name (ex. "2/2015") to group.
      this.quarterNameByDateStr(stockDateStr)
    })
    
    groupedStockDays
  }

  private def quarterNameByDateStr(dateStr: String): String = {
    val date = StockLoader.StockDateFormat.parse(dateStr)
    
    // Use Java's Calendar to reliably get month / year from date str.
    // Use singleton Calendar instance for convenience.
    val cal = StockLoader.StockDateCalendar
    cal.setTime(date)
    val month = cal.get(java.util.Calendar.MONTH)
    val year = cal.get(java.util.Calendar.YEAR)
    
    Quarter.name(this.monthQuarter(month), year)
  }
  
  private def monthQuarter(month: Int) = (month / 3) + 1
  
  private def stockDaysAveragePrice(stockDayLines: Vector[String]) = {
    val pricesSum = stockDayLines.foldLeft(0.0)((sum: Double, line: String) => {
      val closingPrice = this.stockValue(line, "close").replace(',', '.').toDouble
      sum + closingPrice
    })
    
    pricesSum / stockDayLines.size
  }
  
  private def stockListValue(line: String, columnName: String) = this.csvValue(line, columnName, StockLoader.StockListColumnIndices)
  
  private def stockValue(line: String, columnName: String) = this.csvValue(line, columnName, StockLoader.StockColumnIndices)
  
  private def csvValue(line: String, columnName: String, columnsList: Map[String, Int]) = line.split(StockLoader.CsvSeparator)(columnsList(columnName))
  
}

object StockLoader {
  
  val DirSeparator = "/"
  val CsvSeparator = ";"
  
  val StockListColumns = Vector("id", "ticker", "name")
  val StockListColumnIndices = (StockListColumns.zip(StockListColumns.indices)).toMap
  
  val StockColumns = Vector("date", "open", "high", "low", "close", "volume")
  val StockColumnIndices = (StockColumns.zip(StockColumns.indices)).toMap
  
  val StockDateFormat = new java.text.SimpleDateFormat("dd.MM.yyyy")
  val StockDateCalendar = java.util.Calendar.getInstance()
  
}