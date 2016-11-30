package o1.stockexchange

class QuarterStock(val company: Company, val price: Double) {
  
  def formattedPrice: String = StockExchange.formatPrice(this.price)
  
  def fullDescription = {
    var description = s"${this.company.name} (${this.company.ticker}):\n"
    
    this.company.summary match {
      case Some(companySummary) => description += s"Summary: ${companySummary}\n"
      case None => description += ""
    }
    
    description += s"Current price: ${this.formattedPrice}"
    
    description
  }
  
  def rowDescription = s"${this.company.name} (${this.company.ticker}): ${this.formattedPrice}"
  
  override def toString = this.rowDescription
  
}
