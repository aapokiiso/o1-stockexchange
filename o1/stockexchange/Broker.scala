package o1.stockexchange

class Broker(var name: String = Broker.FallbackName) {
  
  var pricesheet = Map[Company, Double]()
  private var currentCapital: Double = Broker.InitialCapital
  private val holdQtys = scala.collection.mutable.Map[Company, Int]()
  private val investedSums = scala.collection.mutable.Map[Company, Double]()
      
  private var quitCommandGiven = false
  
  def capital = this.currentCapital
  
  def portfolioGrowth = {
    val percents = this.holdQtys.keys.map(portfolioItemGrowth)
    
    percents.sum / percents.size
  }
  
  def buy(company: Company, amount: Int): Boolean = {
    val stockPrice = this.pricesheet(company)
    val purchasePrice = stockPrice * amount
    val canBuy = this.capital >= purchasePrice
    
    if (canBuy) {
      this.currentCapital -= purchasePrice
      
      this.increaseInvestedSum(company, purchasePrice)
      this.increaseHoldQty(company, amount)
    }
    
    canBuy
  }
  
  def sell(company: Company, sellAmount: Option[Int] = None): Boolean = { 
    val amount = sellAmount match {
      case Some(amount) => {
        val holdQty = this.holdQty(company)
        if (amount > holdQty) holdQty else amount
      }
      case None => this.holdQty(company)
    }
    
    val stockPrice = this.pricesheet(company)
    val sellPrice = stockPrice * amount
    this.currentCapital += sellPrice
    
    this.decreaseInvestedSum(company, sellPrice)
    this.decreaseHoldQty(company, amount)
    
    true
  }
  
  def portfolioItemWorth(company: Company) = {
    val holdQty = this.holdQtys(company)
    val stockPrice = this.pricesheet(company)

    stockPrice * holdQty
  }
  
  def portfolioItemGrowth(company: Company) = {
    val holdQty = this.holdQtys(company)
    val investedSum = this.investedSums(company)
    val stockPrice = this.pricesheet(company)
    val totalWorth = stockPrice * holdQty
    
    (totalWorth - investedSum) / investedSum
  }
  
  def holdQty(company: Company): Int = {
    this.holdQtys.get(company) match {
      case Some(qty) => qty
      case None => 0
    }
  }
  
  def hasQuit = this.quitCommandGiven
  
  def quit(): Unit = {
    this.quitCommandGiven = true
  }
  
  def status = {
    val formattedCapital = StockExchange.formatPrice(this.capital)
    val formattedGrowth = StockExchange.formatPercent(this.portfolioGrowth)
    
    s"Name: ${this.name}\n" +
    s"Capital: ${formattedCapital}\n" +
    s"Total growth: ${formattedGrowth}\n\n" +
    "Portfolio: \n" +
    this.portfolioDescription
  }
  
  def portfolioDescription: String = {
    val headingRow = Vector(Vector("Company name", "Hold quantity", "Invested sum (mk)", "Total worth (mk)", "Growth (%)"))
    val itemRows = this.holdQtys.keys.map((company) => {
      val holdQty = this.holdQtys(company)
      val investedSum = StockExchange.formatPrice(this.investedSums(company), false)
      val totalWorth = StockExchange.formatPrice(this.portfolioItemWorth(company), false)
      val growthPercent = StockExchange.formatPercent(this.portfolioItemGrowth(company), false)
      
      Vector(company.name, holdQty, investedSum, totalWorth, growthPercent)
    })
    val rows = Vector.concat(headingRow, itemRows)
    
    Tabulator.format(rows)
  }
  
  override def toString = this.status
  
  private def increaseHoldQty(company: Company, qty: Int) = {
    this.changeHoldQty(company, qty.abs)
  }
  
  private def decreaseHoldQty(company: Company, qty: Int) = {
    this.changeHoldQty(company, -1 * qty.abs)
  }
  
  private def changeHoldQty(company: Company, qtyChange: Int): Unit = {
    this.holdQtys.get(company) match {
      case Some(existingQty) => this.holdQtys(company) = existingQty + qtyChange
      case None => this.holdQtys(company) = qtyChange
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
  val InitialCapital = 20000.0 
  
}