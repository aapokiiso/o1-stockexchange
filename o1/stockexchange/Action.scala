package o1.stockexchange

trait Action {
  
  def execute(exchange: StockExchange): String
  
}

object Action {
  
  private class HelpAction extends Action {
    def execute(exchange: StockExchange): String = {
      "Help! @todo"
    }
  }
  
  private class NotFoundAction extends Action {
    def execute(exchange: StockExchange): String = {
      "Command not recognized."
    }
  }
  
  def apply(input: String): Action = {
    val commandText = input.trim.toLowerCase
    val verb        = commandText.takeWhile( _ != ' ' )
    val modifiers   = commandText.drop(verb.length).trim
    
    verb match {
      case "help" => new HelpAction()
      case default => new NotFoundAction()
    }
  }
  
}