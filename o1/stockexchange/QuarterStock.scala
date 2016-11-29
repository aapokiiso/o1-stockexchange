package o1.stockexchange

class QuarterStock(val company: Company, val price: Double) {
  
  def description = {
    // todo add company description
    s"${this.company.name} (${this.company.ticker}):\n" +
    s"Current price: ${this.price}"
  }
  
  override def toString = this.description
  
}
