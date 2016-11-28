package o1.stockexchange

import scala.collection.mutable.{Map => MutableMap}

class Quarter(val name: String) {
  
  val stocks = MutableMap[Int, Double]()
  
}
