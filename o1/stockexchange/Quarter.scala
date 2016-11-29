package o1.stockexchange

class Quarter(val name: String, val stocks: Vector[QuarterStock]) {
  
  val quarter = Quarter.quarter(this.name)
  val year = Quarter.year(this.name)
  
  def <(another: Quarter): Boolean = this.year < another.year || (this.year == another.year && this.quarter < another.quarter)
  
  def >(another: Quarter): Boolean = this.year > another.year || (this.year == another.year && this.quarter > another.quarter)
  
  // @todo
  def fullDescription: String = {
    "poop"
  }
  
  // @todo
  override def toString = ""
  
}

object Quarter {
  
  val NameSeparator = "/"
  
  def name(quarter: Int, year: Int): String = quarter + Quarter.NameSeparator + year
  
  def quarter(name: String): Int = name.split(Quarter.NameSeparator)(0).toInt
  
  def year(name: String): Int = name.split(Quarter.NameSeparator)(1).toInt
  
}