package o1.stockexchange

class Broker(val name: String = Broker.FallbackName, initialCapital: Double) {
  
  private var currentCapital: Double = initialCapital
  private var quitCommandGiven = false
  
  def capital = this.currentCapital
  
  def hasQuit = this.quitCommandGiven
  
  def quit() = {
    this.quitCommandGiven = true
    ""
  }
  
}

object Broker {
  
  val FallbackName = "S. Heinäkatu"
  
}