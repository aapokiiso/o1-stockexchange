package o1.stockexchange

class Broker(val name: String = Broker.FallbackName) {
  
  private var currentCapital: Double = Broker.InitialCapital
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
  val InitialCapital = 20000.0 
  
}