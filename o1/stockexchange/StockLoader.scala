package o1.stockexchange

import scala.io.Source
import scala.collection.mutable.Buffer
import scala.collection.immutable.Map

class StockLoader {
  
  private var stocksListCache: Option[Map[Int, Map[String, String]]] = None
  private var companiesCache: Option[Map[Int, Company]] = None
  
  def companies: Map[Int, Company] = {
    this.companiesCache match {
      case Some(companies) => companies
      case None => {
        val companies = this.stocksList.map((s) => {
          val stockId = s._1
          val stockData = s._2
          
          val summary = if (stockData("summary").size > 0) Some(stockData("summary")) else None
          
          stockId -> new Company(stockId, stockData("ticker"), stockData("name"), summary)
        })
        this.companiesCache = Some(companies)
        companies
      }
    }
  }
  
  def quarters: Iterator[Quarter] = {
    val quartersData = scala.collection.mutable.Map[String, Buffer[QuarterStock]]()
    
    // Combined quarters data from all stocks.
    for (stockId <- this.stocksList.keys) {
      val stockQuarters = this.quartersByStockIdFromFile(stockId)
      
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
    
    val quarters = quartersData.toVector.map((q) => {
      val quarterName = q._1
      val quarterStocks = q._2.toVector
      
      new Quarter(quarterName, quarterStocks)
    })
    
    // Sort quarters chronologically (1/1990, 3/1991, ... 4/2016)
    val sortedQuarters = quarters.sortWith( _ < _ )
    
    sortedQuarters.toIterator
  }
  
  private def stocksList = {
    this.stocksListCache match {
      case Some(stocksList) => stocksList
      case None => {
        val stocksList = this.stocksListFromFile
        this.stocksListCache = Some(stocksList)
        stocksList
      }
    }
  }
  
  /**
   * Returns map of stock IDs and company names.
   * 
   * Map format: stock ID -> Company data (ticker, name, summary)
   * 
   * Stock ID is nothing else but a numeric ID
   * used by Kauppalehti to identify stocks.
   * See "walkthrough.txt" in this project's root for details.
   */
  private def stocksListFromFile: Map[Int, Map[String, String]] = {
    val stocksSource = Source.fromFile(StockLoader.StocksListFile)
    val tickers = Buffer[String]()
    
    try {
      val stocks = stocksSource.getLines().map((line) => {
        val stockId = this.stockListValue(line, "id").toInt
        val ticker = this.getUniqueTicker(tickers, this.stockListValue(line, "ticker"))
        tickers += ticker
        val companyData = Map(
            "ticker" -> ticker,
            "name" -> this.stockListValue(line, "name"),
            "summary" -> this.stockListValue(line, "summary")
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
   * @param stockId - Kauppalehti stock / company ID to load quarters for.
   */
  private def quartersByStockIdFromFile(stockId: Int): Map[String, Double] = {
    val filePath = StockLoader.StocksDir + StockLoader.DirSeparator + stockId + StockLoader.FileExt
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
  
  private def getUniqueTicker(tickers: Buffer[String], ticker: String): String = {
    var i = 0
    var uniqueTicker = ticker
    
    while (tickers.contains(ticker) && i < StockLoader.TickerSuffixOptions.size) {
      uniqueTicker = ticker + StockLoader.TickerSuffixOptions(i)
      i += 1
    }
    
    uniqueTicker
  }
  
  private def stockListValue(line: String, columnName: String) = this.csvValue(line, columnName, StockLoader.StockListColumnIndices)
  
  private def stockValue(line: String, columnName: String) = this.csvValue(line, columnName, StockLoader.StockColumnIndices)
  
  private def csvValue(line: String, columnName: String, columnsList: Map[String, Int]) = line.split(StockLoader.CsvSeparator, -1)(columnsList(columnName))
  
}

object StockLoader {
  
  val DirSeparator = "/"
  val CsvSeparator = ";"
  val FileExt = ".txt"
  
  val StocksDir = "resources" + DirSeparator + "stocks"
  val StocksListFile = StocksDir + DirSeparator + "index" + FileExt
  
  val StockListColumns = Vector("id", "ticker", "name", "summary")
  val StockListColumnIndices = (StockListColumns.zip(StockListColumns.indices)).toMap
  
  val StockColumns = Vector("date", "open", "high", "low", "close", "volume")
  val StockColumnIndices = (StockColumns.zip(StockColumns.indices)).toMap
  
  val StockDateFormat = new java.text.SimpleDateFormat("dd.MM.yyyy")
  val StockDateCalendar = java.util.Calendar.getInstance()
  
  val TickerSuffixOptions = ('A' to 'Z').reverse
  
}