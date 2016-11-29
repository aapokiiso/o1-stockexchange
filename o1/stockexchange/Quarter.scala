package o1.stockexchange

class Quarter(val name: String, val stocks: Vector[QuarterStock]) {
  
  val quarter = Quarter.quarter(this.name)
  val year = Quarter.year(this.name)
  
  def <(another: Quarter): Boolean = this.year < another.year || (this.year == another.year && this.quarter < another.quarter)
  
  def >(another: Quarter): Boolean = this.year > another.year || (this.year == another.year && this.quarter > another.quarter)
  
  def pricesheet: Map[Company, Double] = this.stocks.map((quarterStock) => quarterStock.company -> quarterStock.price).toMap
  
  def stockByTicker(ticker: String): Option[QuarterStock] = this.stocks.find( _.company.ticker == ticker )
  
  // @todo
  def description: String = {
    this.name +
    this.stocksDescription
  }
  
  def stocksDescription: String = {
    this.stocks.map( _.rowDescription ).mkString("\n")
  }
  
  override def toString = this.description
  
} 

object Quarter {
  
  val NameSeparator = "/"
  
  def name(quarter: Int, year: Int): String = quarter + Quarter.NameSeparator + year
  
  def quarter(name: String): Int = name.split(Quarter.NameSeparator)(0).toInt
  
  def year(name: String): Int = name.split(Quarter.NameSeparator)(1).toInt
  
}