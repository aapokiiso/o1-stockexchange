package o1.stockexchange

class Company(val id: Int, val ticker: String, val name: String, val summary: Option[String] = None) {
  
  override def toString = s"${this.name} (${this.ticker})"
  
}
