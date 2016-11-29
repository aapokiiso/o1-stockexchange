package o1.stockexchange

class Broker(val name: String = Broker.FallbackName) {
  
  var pricesheet = Map[Company, Double]()
  private var currentCapital: Double = Broker.InitialCapital
  private val holdQtys = scala.collection.mutable.Map[Company, Int]()
  private val investedSums = scala.collection.mutable.Map[Company, Double]()
      
  private var quitCommandGiven = false
  
  def capital = this.currentCapital
  
  def formattedCapital = StockExchange.formatPrice(this.capital)
  
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
  
  def holdQty(company: Company): Int = {
    this.holdQtys.get(company) match {
      case Some(qty) => qty
      case None => 0
    }
  }
  
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
  
  def hasQuit = this.quitCommandGiven
  
  def quit(): Unit = {
    this.quitCommandGiven = true
  }
  
  def status = {
    s"Name: ${this.name}\n" +
    s"Capital: ${this.formattedCapital}\n" +
    "Portfolio: \n" +
    this.portfolioDescription
  }
  
  def portfolioDescription: String = {
    // @todo buy price and change in worth    
    val headingRow = Vector(Vector("Company name", "Hold quantity", "Invested sum (mk.)", "Total worth (mk.)", "Growth (%)"))
    val itemRows = this.holdQtys.map((item) => {
      val companyName = item._1.name
      val holdQty = item._2
      val investedSum = StockExchange.formatPrice(this.investedSums(item._1))
      val stockPrice = this.pricesheet(item._1)
      val totalWorth = StockExchange.formatPrice(stockPrice * holdQty)
      val growthPercent = StockExchange.formatPercent((totalWorth - investedSum) / investedSum)
      
      Vector(companyName, holdQty, investedSum, totalWorth, growthPercent)
    })
    val rows = Vector.concat(headingRow, itemRows)
    
    Tabulator.format(rows)
  }
  
  override def toString = this.status
  
}

object Broker {
  
  val FallbackName = "S. Einäkatu"
  val InitialCapital = 20000.0 
  
}