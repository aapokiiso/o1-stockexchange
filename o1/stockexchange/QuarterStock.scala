package o1.stockexchange

class QuarterStock(val company: Company, val price: Double) {
  
  def formattedPrice: Double = StockExchange.formatPrice(this.price)
  
  def fullDescription = {
    // todo add company description
    s"${this.company.name} (${this.company.ticker}):\n" +
    s"Current price: ${this.formattedPrice}"
  }
  
  def rowDescription = s"${this.company.name} (${this.company.ticker}): ${this.formattedPrice}"
  
  override def toString = this.rowDescription
  
}
