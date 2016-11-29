package o1.stockexchange

class Broker(val name: String = Broker.FallbackName) {
  
  var pricesheet = Map[Company, Double]()
  private var currentCapital: Double = Broker.InitialCapital
  private val portfolio = scala.collection.mutable.Map[Company, Int]()
  private var quitCommandGiven = false
  
  def capital = this.currentCapital
  
  def buy(company: Company, amount: Int): Boolean = {
    val stockPrice = this.pricesheet(company)
    val requiredCapital = stockPrice * amount
    val canBuy = this.capital >= requiredCapital
    
    if (canBuy) {
      this.currentCapital -= requiredCapital
      this.changeHoldQuantity(company, amount)
    }
    
    canBuy
  }
  
  def sell(company: Company, sellAmount: Option[Int] = None): Boolean = { 
    val amount = sellAmount match {
      case Some(amount) => amount
      case None => this.holdQuantity(company)
    }
    
    val stockPrice = this.pricesheet(company)
    val redeemedCapital = stockPrice * amount
    this.currentCapital += redeemedCapital
    this.changeHoldQuantity(company, amount)
    
    true
  }
  
  def holdQuantity(company: Company): Int = {
    this.portfolio.get(company) match {
      case Some(qty) => qty
      case None => 0
    }
  }
  
  private def changeHoldQuantity(company: Company, qtyChange: Int): Unit = {
    this.portfolio.get(company) match {
      case Some(existingQty) => this.portfolio(company) = existingQty + qtyChange
      case None => this.portfolio(company) = qtyChange
    }
  }
  
  def hasQuit = this.quitCommandGiven
  
  def quit(): Unit = {
    this.quitCommandGiven = true
  }
  
  def status = {
    s"Name: ${this.name}\n" +
    s"Capital: ${this.capital}\n" +
    "Portfolio: \n" +
    this.portfolioDescription
  }
  
  def portfolioDescription: String = {
    // @todo buy price and change in worth    
    val headingRow = Vector(Vector("Company name", "Hold quantity", "Total worth (mk.)"))
    val itemRows = this.portfolio.map((item) => {
      val companyName = item._1.name
      val holdQty = item._2
      val stockPrice = this.pricesheet(item._1)
      val totalWorth = Math.round(stockPrice * holdQty * 100.00) / 100.00
      
      Vector(companyName, holdQty, totalWorth)
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