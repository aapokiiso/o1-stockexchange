package o1.stockexchange

class Broker(var name: String = Broker.FallbackName) {
  
  var pricesheet = Map[Company, Double]()
  private var currentCapital: Double = Broker.InitialCapital
  private val holdingQtys = scala.collection.mutable.Map[Company, Int]()
  private val investedSums = scala.collection.mutable.Map[Company, Double]()
      
  private var quitCommandGiven = false
  
  def capital = this.currentCapital
  
  def portfolioWorth = this.holdingQtys.keys.map(this.holdingWorth).sum
  
  def totalWorth = this.capital + this.portfolioWorth
  
  def isProfitable = this.totalWorth >= Broker.InitialCapital
  
  def portfolioGrowth = {
    val percents = this.holdingQtys.keys.map(this.holdingGrowth)
    
    percents.sum / percents.size
  }
  
  def buy(company: Company, qty: Int): Boolean = {
    val purchasePrice = this.currentPrice(company, qty)
    val canBuy = this.capital >= purchasePrice
    
    if (canBuy) {
      this.currentCapital -= purchasePrice
      
      this.increaseInvestedSum(company, purchasePrice)
      this.increaseHoldingQty(company, qty)
    }
    
    canBuy
  }
  
  def sell(company: Company, sellQty: Option[Int] = None): Boolean = { 
    val qty = sellQty match {
      case Some(qty) => {
        val holdQty = this.holdingQty(company)
        if (qty > holdQty) holdQty else qty
      }
      case None => this.holdingQty(company)
    }
    
    val sellPrice = this.currentPrice(company, qty)
    this.currentCapital += sellPrice
    
    this.decreaseInvestedSum(company, sellPrice)
    this.decreaseHoldingQty(company, qty)
    
    true
  }
  
  def holdingWorth(company: Company): Double = {
    val holdQty = this.holdingQtys(company)
    val stockPrice = this.currentPrice(company)

    stockPrice * holdQty.toDouble
  }
  
  def holdingGrowth(company: Company) = {
    val holdQty = this.holdingQtys(company)
    val investedSum = this.investedSums(company)
    val stockPrice = this.currentPrice(company)
    val totalWorth = stockPrice * holdQty
    
    (totalWorth - investedSum) / investedSum
  }
  
  def holdingQty(company: Company): Int = {
    this.holdingQtys.get(company) match {
      case Some(qty) => qty
      case None => 0
    }
  }
  
  def currentPrice(company: Company, qty: Double = 1.0) = {
    val price = this.pricesheet.get(company) match {
      case Some(price) => price
      case None => 0
    }
    
    price * qty
  }
  
  def hasQuit = this.quitCommandGiven
  
  def quit(): Unit = {
    this.quitCommandGiven = true
  }
  
  def status = {
    val formattedCapital = StockExchange.formatPrice(this.capital)
    val formattedPortfolioWorth = StockExchange.formatPrice(this.portfolioWorth)
    val formattedWorth = StockExchange.formatPrice(this.totalWorth)
    val formattedGrowth = StockExchange.formatPercent(this.portfolioGrowth)
    
    s"Name: ${this.name}\n\n" +
    s"Capital: ${formattedCapital}\n" + 
    s"Portfolio: ${formattedPortfolioWorth}\n" +
    s"Total: ${formattedWorth}\n\n" +
    s"Portfolio (total growth ${formattedGrowth}): \n" +
    this.portfolioDescription
  }
  
  def portfolioDescription: String = {
    val headingRow = Vector(Vector("Company", "Ticker", "Hold quantity", "Invested sum (mk)", "Total worth (mk)", "Growth (%)"))
    val itemRows = this.holdingQtys.keys.map((company) => {
      val holdQty = this.holdingQtys(company)
      val investedSum = StockExchange.formatPrice(this.investedSums(company), false)
      val totalWorth = StockExchange.formatPrice(this.holdingWorth(company), false)
      val growthPercent = StockExchange.formatPercent(this.holdingGrowth(company), false)
      
      Vector(company.name, company.ticker, holdQty, investedSum, totalWorth, growthPercent)
    })
    val rows = Vector.concat(headingRow, itemRows)
    
    Tabulator.format(rows)
  }
  
  override def toString = this.status
  
  private def increaseHoldingQty(company: Company, qty: Int) = {
    this.changeHoldingQty(company, qty.abs)
  }
  
  private def decreaseHoldingQty(company: Company, qty: Int) = {
    this.changeHoldingQty(company, -1 * qty.abs)
  }
  
  private def changeHoldingQty(company: Company, qtyChange: Int): Unit = {
    this.holdingQtys.get(company) match {
      case Some(existingQty) => this.holdingQtys(company) = existingQty + qtyChange
      case None => this.holdingQtys(company) = qtyChange
    }
  }
  
  private def increaseInvestedSum(company: Company, purchasePrice: Double) = {
    this.changeInvestedSum(company, purchasePrice.abs)
  }
  
  private def decreaseInvestedSum(company: Company, purchasePrice: Double) = {
    this.changeInvestedSum(company, -1 * purchasePrice.abs)
  }
  
  private def changeInvestedSum(company: Company, purchasePrice: Double): Unit = {
    this.investedSums.get(company) match {
      case Some(investedSum) => {
        val changedSum = investedSum + purchasePrice
        this.investedSums(company) = if (changedSum > 0) changedSum else 0
      }
      case None => this.investedSums(company) = purchasePrice
    }
  }
  
}

object Broker {
  
  val FallbackName = "S. Einäkatu"
  val InitialCapital = 1000000.0 
  
}