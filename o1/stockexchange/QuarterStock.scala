package o1.stockexchange

class QuarterStock(val company: Company, val rawPrice: Double) {
  
  def price: Double = Math.round(this.rawPrice * 100.00) / 100.00
  
  def description = {
    // todo add company description
    s"${this.company.name} (${this.company.ticker}):\n" +
    s"Current price: ${this.price}"
  }
  
  override def toString = this.description
  
}
