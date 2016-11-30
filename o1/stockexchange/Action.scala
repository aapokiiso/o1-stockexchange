package o1.stockexchange

trait Action {
  
  def execute(exchange: StockExchange): String
  
}

object Action {
  
  private class HelpAction extends Action {
    def execute(exchange: StockExchange): String = {
      "The objective of the game is to stay profitable until the end of the game (approx. 80 quarters / turns).\n" +
      "In this case the word 'profitable' means that capital + portfolio worth > 0.\n\n" +
      "Commands:\n\n" + 
      "help                                     Displays this help message.\n\n" +
      "quit                                     Quit the game (works only in the GUI).\n\n" +
      "status                                   Displays your broker info and portfolio.\n\n" +
      "rename <plaintext name>                  Renames your broker character.\n\n" +
      "liststocks                               Shows available stocks on the exchange.\n\n" +
      "examine <stock ticker code>              Shows additional information about the stock.\n" +
      "                                         You can view a list of stocks and their ticker codes with 'liststocks'.\n\n" +
      "buy <stock ticker code> <quantity>       Buys a number of stocks from the given company at the current price.\n" +
      "                                         Example: 'buy VIK 10' (buys 10pcs of Viking Line stock).\n\n" +
      "sell <stock ticker code> (<quantity>)    Sells a number of stocks from the given company at the current price.\n" +
      "                                         Quantity is optional - if omitted, all stocks from this company are sold.\n\n" +
      "nextquarter                              Proceed to the next quarter. The list of companies and their prices on\n" +
      "                                         the exchange will be updated."
    }
  }
  
  private class StatusAction extends Action {
    def execute(exchange: StockExchange): String = {
      exchange.broker.status
    }
  }
  
  private class RenameBrokerAction(modifiers: Vector[String]) extends Action {
    val name = modifiers.mkString(" ")
    
    def execute(exchange: StockExchange): String = {
      exchange.broker.name = this.name
      
      s"You have been renamed as ${this.name}"
    }
  }
  
  private class NextQuarterAction extends Action {
    def execute(exchange: StockExchange): String = {
      var prevQuarter: Option[Quarter] = None
      exchange.quarter match {
        case Some(quarter) => prevQuarter = Some(quarter)
        case None => prevQuarter = None
      }
      
      exchange.nextQuarter()
      
      exchange.quarter match {
        case Some(quarter) => {
          var message = s"Moved on to the quarter ${quarter.name}" 
          prevQuarter match {
            case Some(prevQuarter) => {
              if (prevQuarter.stocksDiff(quarter).size > 0) {
                message += "\n\nCompanies leaving the exchange this quarter:\n\n" +
                prevQuarter.stocksDiffDescription(quarter)
              }
              
              if (quarter.stocksDiff(prevQuarter).size > 0) {
                message += "\n\nCompanies introduced to the exchange this quarter:\n\n" +
                quarter.stocksDiffDescription(prevQuarter)
              }
            }
            case None => ""
          }
          
          message
        }
        case None => "" // Goodbye message is shown
      }
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
            case None => s"No stock found for '${this.ticker}'"
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
        case None => s"No stock found for '${this.ticker}'"
      }
    }
    
    def changeStock(broker: Broker, company: Company, amount: Int): String
  }
  
  private class BuyStockAction(modifiers: Vector[String]) extends ChangeStockAction(modifiers) {
    def changeStock(broker: Broker, company: Company, qty: Int): String = {
      if (qty > 0) {
        val purchasePrice = StockExchange.formatPrice(broker.currentPrice(company, qty))
        
        val buySuccess = broker.buy(company, qty)
        if (buySuccess) {
          s"Bought ${qty} of ${company.ticker} for ${purchasePrice}"
        } else {
          val capital = StockExchange.formatPrice(broker.capital)
          s"Not enough capital to buy ${qty} of ${company.ticker}.\n" +
          s"Purchase price: ${purchasePrice}\n" +
          s"Current capital: ${capital}"
        }
      } else {
        "Purchase quantity was not specified. See 'help' for brokering instructions."
      }
    }
  }
  
  private class SellStockAction(modifiers: Vector[String]) extends ChangeStockAction(modifiers) {
    def changeStock(broker: Broker, company: Company, sellQty: Int): String = {
      var qty: Option[Int] = None
      if (sellQty > 0) {
        qty = Some(sellQty)
      }
      
      var qtyStr = ""
      var priceStr = ""
      qty match {
        case Some(qty) => {
          qtyStr = qty.toString
          priceStr = StockExchange.formatPrice(broker.currentPrice(company, qty))
        }
        case None => {
          val holdQty = broker.holdingQty(company)
          qtyStr = s"all ${holdQty}" 
          priceStr = StockExchange.formatPrice(broker.currentPrice(company, holdQty))
        }
      }
      
      val sellSuccess = broker.sell(company, qty)
      if (sellSuccess) {
        s"Sold ${qtyStr} of ${company.ticker} for ${priceStr}"
      } else {
        s"Failed to sell ${qtyStr} of ${company.ticker}. It's probably your fault. It's always your fault!"
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
      "Command not recognized. Please turn 360° to the right and try again."
    }
  }
  
  def apply(input: String): Action = {
    val commandText = input.trim
    val verb        = commandText.takeWhile( _ != ' ' )
    val modifiers   = commandText.drop(verb.length).trim.split(" ").toVector
    
    verb match {
      case "help" => new HelpAction()
      case "status" => new StatusAction()
      case "rename" => new RenameBrokerAction(modifiers)
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