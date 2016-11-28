package o1.stockexchange

import scala.collection.immutable.Map

class Quarter(val name: String, val stocks: Map[Int, Double]) {
  
  val quarter = Quarter.quarter(this.name)
  val year = Quarter.year(this.name)
  
  def <(another: Quarter): Boolean = this.year < another.year || (this.year == another.year && this.quarter < another.quarter)
  
  def >(another: Quarter): Boolean = this.year > another.year || (this.year == another.year && this.quarter > another.quarter)
  
}

object Quarter {
  
  val NameSeparator = "/"
  
  def name(quarter: Int, year: Int): String = quarter + Quarter.NameSeparator + year
  
  def quarter(name: String): Int = name.split(Quarter.NameSeparator)(0).toInt
  
  def year(name: String): Int = name.split(Quarter.NameSeparator)(1).toInt
  
}