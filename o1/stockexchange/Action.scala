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
      ""      
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
  
  private class ExamineStockAction extends Action {
    def execute(exchange: StockExchange): String = {
      "@todo"
    }
  }
  
  private abstract class ChangeStockAction(modifiers: Vector[String]) extends Action {
    val ticker = if (modifiers.size == 2) modifiers(0).toUpperCase() else ""
    val amount = if (modifiers.size == 2) modifiers(1).toInt else 0
    
    def execute(exchange: StockExchange): String = {      
      if (this.amount > 0) {
        val company = exchange.companyByTicker(this.ticker)
        
        company match {
          case Some(company) => {
            val buySuccess = this.changeStock(exchange.broker, company, this.amount)

            if (buySuccess) {
              "@todo change successful"
            } else {
              "@todo not enough money"
            }
          }
          case None => "@todo company not found"
        }
      } else {
        "@todo invalid amount"
      }
    }
    
    def changeStock(broker: Broker, company: Company, amount: Int): Boolean
  }
  
  private class BuyStockAction(modifiers: Vector[String]) extends ChangeStockAction(modifiers) {
    def changeStock(broker: Broker, company: Company, amount: Int): Boolean = broker.buy(company, amount)
  }
  
  private class SellStockAction(modifiers: Vector[String]) extends ChangeStockAction(modifiers) {
    def changeStock(broker: Broker, company: Company, sellAmount: Int): Boolean = {
      var amount: Option[Int] = None
      if (sellAmount > 0) {
        amount = Some(sellAmount)
      }
      
      broker.sell(company, amount)
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
      case "examine" => new ExamineStockAction()
      case "buy" => new BuyStockAction(modifiers)
      case "sell" => new SellStockAction(modifiers)
      case "quit" => new QuitAction()
      case default => new UndefinedAction()
    }
  }
  
}