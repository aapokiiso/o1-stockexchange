package o1.stockexchange

class Quarter(val name: String, val stocks: Vector[QuarterStock]) {
  
  val quarter = Quarter.quarter(this.name)
  val year = Quarter.year(this.name)
  
  def <(another: Quarter): Boolean = this.year < another.year || (this.year == another.year && this.quarter < another.quarter)
  
  def >(another: Quarter): Boolean = this.year > another.year || (this.year == another.year && this.quarter > another.quarter)
  
  def stocksDiff(another: Quarter) = this.stocks.filter((stock) => {
    var existsInAnother = false
    for (anotherStock <- another.stocks) {
      if (!existsInAnother && anotherStock.company == stock.company) {
        existsInAnother = true
      }
    }
    !existsInAnother
  })
  
  def pricesheet: Map[Company, Double] = this.stocks.map((quarterStock) => quarterStock.company -> quarterStock.price).toMap
  
  def stockByTicker(ticker: String): Option[QuarterStock] = this.stocks.find( _.company.ticker == ticker )
  
  // @todo
  def description: String = {
    s"Quarter ${this.name}\n" + 
    this.stocksDescription
  }
  
  def stocksDescription: String = {
    this.stocks.map( _.rowDescription ).mkString("\n")
  }
  
  def stocksDiffDescription(another: Quarter): String = {
    this.stocksDiff(another).map( _.rowDescription ).mkString("\n")
  }
  
  override def toString = this.name
  
} 

object Quarter {
  
  val NameSeparator = "/"
  
  def name(quarter: Int, year: Int): String = quarter + Quarter.NameSeparator + year
  
  def quarter(name: String): Int = name.split(Quarter.NameSeparator)(0).toInt
  
  def year(name: String): Int = name.split(Quarter.NameSeparator)(1).toInt
  
}