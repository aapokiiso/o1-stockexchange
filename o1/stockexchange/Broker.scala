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
      this.changeAmountOwned(company, amount)
    }
    
    canBuy
  }
  
  def sell(company: Company, sellAmount: Option[Int] = None): Boolean = {
    val amount = sellAmount match {
      case Some(i) => i
      case None => this.amountOwned(company)
    }
    
    val stockPrice = this.pricesheet(company)
    val redeemedCapital = stockPrice * amount
    this.currentCapital += redeemedCapital
    this.changeAmountOwned(company, amount)
    
    true
  }
  
  def amountOwned(company: Company): Int = {
    this.portfolio.get(company) match {
      case Some(amount) => amount
      case None => 0
    }
  }
  
  private def changeAmountOwned(company: Company, amount: Int): Unit = {
    this.portfolio.get(company) match {
      case Some(existingAmount) => this.portfolio(company) = existingAmount + amount
      case None => this.portfolio(company) = amount
    }
  }
  
  def hasQuit = this.quitCommandGiven
  
  def quit(): Unit = {
    this.quitCommandGiven = true
  }
  
  def status = {
    var description = s"${this.name}\n" +
    s"Capital: ${this.capital}\n" +
    "Portfolio: \n" +
    s"\tCompany name - Holding amount - Total worth (mk.)\n"
     // @todo buy price and change in worth    
    this.portfolio.foreach((item) => {
      val companyName = item._1.name
      val holdAmount = item._2
      val stockPrice = this.pricesheet(item._1)
      val totalWorth = stockPrice * holdAmount
      description += s"\t${companyName} - ${holdAmount} - ${totalWorth}\n"
    })
    
    description
  }
  
  override def toString = this.status
  
}

object Broker {
  
  val FallbackName = "S. Einäkatu"
  val InitialCapital = 20000.0 
  
}