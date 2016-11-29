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
  
  private class StatusAction extends Action {
    def execute(exchange: StockExchange): String = {
      "@todo"
    }
  }
  
  private class NextQuarterAction extends Action {
    def execute(exchange: StockExchange): String = {
      exchange.nextQuarter()
    }
  }
  
  private class ListStocksAction extends Action {
    def execute(exchange: StockExchange): String = {
      "@todo"
    }
  }
  
  private class ExamineStockAction extends Action {
    def execute(exchange: StockExchange): String = {
      "@todo"
    }
  }
  
  private class BuyStockAction extends Action {
    def execute(exchange: StockExchange): String = {
      "@todo"
    }
  }
  
  private class SellStockAction extends Action {
    def execute(exchange: StockExchange): String = {
      "@todo"
    }
  }
  
  private class QuitAction extends Action {
    def execute(exchange: StockExchange): String = {
      exchange.broker.quit()
    }
  }
  
  private class UndefinedAction extends Action {
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
      case "status" => new StatusAction()
      case "nextquarter" => new NextQuarterAction()
      case "liststocks" => new ListStocksAction()
      case "examine" => new ExamineStockAction()
      case "buy" => new BuyStockAction()
      case "sell" => new SellStockAction()
      case "quit" => new QuitAction()
      case default => new UndefinedAction()
    }
  }
  
}