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
      exchange.broker.status
    }
  }
  
  private class NextQuarterAction extends Action {
    def execute(exchange: StockExchange): String = {
      exchange.nextQuarter()
      "@todo quarter changed"      
    }
  }
  
  private class ListStocksAction extends Action {
    def execute(exchange: StockExchange): String = {
      exchange.quarter match {
        case Some(quarter) => quarter.stocksDescription
        case None => ""
      }
    }
  }
  
  private class ExamineStockAction(modifiers: Vector[String]) extends Action {
    val ticker = if (modifiers.size == 1) modifiers(0).toUpperCase() else ""
    
    def execute(exchange: StockExchange): String = {
      exchange.quarter match {
        case Some(quarter) => {
          quarter.stockByTicker(ticker) match {
            case Some(stock) => stock.fullDescription
            case None => ""
          }
        }
        case None => ""
      }
    }
  }
  
  private abstract class ChangeStockAction(modifiers: Vector[String]) extends Action {
    val ticker = if (modifiers.size >= 1) modifiers(0).toUpperCase() else ""
    val amount = if (modifiers.size == 2) modifiers(1).toInt else 0
    
    def execute(exchange: StockExchange): String = {      
      val stock = exchange.stockByTicker(this.ticker)
        
      stock match {
        case Some(stock) => {
          this.changeStock(exchange.broker, stock.company, this.amount)
        }
        case None => "@todo stock not found"
      }
    }
    
    def changeStock(broker: Broker, company: Company, amount: Int): String
  }
  
  private class BuyStockAction(modifiers: Vector[String]) extends ChangeStockAction(modifiers) {
    def changeStock(broker: Broker, company: Company, amount: Int): String = {
      if (amount > 0) {
        val buySuccess = broker.buy(company, amount)
        if (buySuccess) {
          "@todo buy success"
        } else {
          "@todo not enough capital"
        }
      } else {
        "@todo no qty"
      }
    }
  }
  
  private class SellStockAction(modifiers: Vector[String]) extends ChangeStockAction(modifiers) {
    def changeStock(broker: Broker, company: Company, sellAmount: Int): String = {
      var amount: Option[Int] = None
      if (sellAmount > 0) {
        amount = Some(sellAmount)
      }
      
      val sellSuccess = broker.sell(company, amount)
      if (sellSuccess) {
        "@todo sell success"
      } else {
        "@todo sell failed"
      }
    }
  }
  
  private class QuitAction extends Action {
    def execute(exchange: StockExchange): String = {
      exchange.broker.quit()
      ""
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
    val modifiers   = commandText.drop(verb.length).trim.split(" ").toVector
    
    verb match {
      case "help" => new HelpAction()
      case "status" => new StatusAction()
      case "nextquarter" => new NextQuarterAction()
      case "liststocks" => new ListStocksAction()
      case "examine" => new ExamineStockAction(modifiers)
      case "buy" => new BuyStockAction(modifiers)
      case "sell" => new SellStockAction(modifiers)
      case "quit" => new QuitAction()
      case default => new UndefinedAction()
    }
  }
  
}