import baseClasses._
import com.github.nscala_time.time.Imports.DateTime

object Main {

  def main(args: Array[String]): Unit = {
    val date = DateTime.parse( "2023-02-14")
    val otherDate = DateTime.parse( "2023-05-14")

    // Financials
    val euros = Cash(name = "Euros", description = "Vault", startDate = date, currency = "€", balance = 120)
    val dollars = Cash(name = "Dollars", description = "Vault", startDate = date, currency = "$", balance = 60)
    val yen = Cash(name = "Yen", description = "Vault", startDate = date, currency = "¥", balance = 20000)

    val ingSight = Sight(name = "ING Checking Account", description = "Basic account", startDate = date, currency = "€", balance = 600)
    val chaseSight = Sight(name = "Chase Checking Account", description = "Basic account", startDate = date, currency = "$", balance = 200)
    val coinbaseSight = Sight(name = "Coinbase Crypto Account", description = "Basic account", startDate = date, currency = "฿", balance = 0.8)

    val europeanStock = Investment(name = "AF", description = "Air France - KLM", startDate = date, currency = "€", balance = 300)
    val americanStock = Investment(name = "AAPL", description = "Apple", startDate = date, currency = "$", balance = 700)

    val europeanSavings = Savings(name = "ING Savings Account", description = "Basic account", startDate = date, currency = "€", balance = 12000)
    val americanSavings = Savings(name = "Chase Savings Account", description = "Basic account", startDate = date, currency = "$", balance = 2000)

    val europeanTravelBudget = Budget(name = "European City Trip", description = "Visit Paris", startDate = DateTime.parse( "2023-02-13"), currency = "€", balance = 340, budget = 500, frequency = "once", reached = false)
    val americanTravelBudget = Budget(name = "American City Trip", description = "Visit New York", startDate = DateTime.parse( "2023-02-13"), currency = "$", balance = 900, budget = 1100, frequency = "once", reached = false)
    
    val laptopGoal = Goal(name = "Macbook M2", description = "Apple laptop", startDate = date, currency = "€", balance = 1000, target = 3000, endDate = otherDate, reached = false, consumed = false)
    val vacuumGoal = Goal(name = "Dyson vacuum cleaner", description = "To vacuum", startDate = date, currency = "€", balance = 100, target = 300, endDate = otherDate, reached = false, consumed = false)

    val europeanDebt = Debt(name = "Mastercard", description = "Credit card", startDate = date, currency = "€", balance = 350, interest = 0, percentage = 0, frequency = "monthly", endDate = null, paid = false)
    val americanDebt = Debt(name = "Express", description = "Credit card", startDate = date, currency = "$", balance = 120, interest = 0, percentage = 0, frequency = "monthly", endDate = null, paid = false)


     // Pools
    val commonCash = Pool[Cash](name = "Common cash", description = "Commonly used", pool = List(euros, dollars))
    val travelCash = Pool[Cash](name = "Travel cash", description = "For trips", pool = List(yen))

    val sightAccounts = Pool[Sight](name = "Sight accounts", description = "Daily transactions", pool = List(ingSight, chaseSight, coinbaseSight))

    val stocks = Pool[Investment](name = "Stocks", description = "Long-term", pool = List(europeanStock, americanStock))

    val savingsAccounts = Pool[Savings](name = "Savings accounts", description = "Very long-term", pool = List(europeanSavings, americanSavings))

    val cityTripBudgets = Pool[Budget](name = "City trip budgets", description = "For holidays", pool = List(europeanTravelBudget, americanTravelBudget))

    val technologyGoals = Pool[Goal](name = "Technology purchases", description = "Short-term", pool = List(laptopGoal, vacuumGoal))

    val creditCards = Pool[Debt](name = "Credit card payoffs", description = "Monthly", pool = List(europeanDebt, americanDebt))

    // Account
    val overview = Account(name = "Overview", description = "Financial Summary", unallocated = 0, cash = List(commonCash, travelCash), sights = List(sightAccounts),
      investments = List(stocks), savings = List(savingsAccounts), budgets = List(cityTripBudgets), goals = List(technologyGoals), debts = List(creditCards))
//
//
//    // --- Method tests ---
//
//    // Account
    println(overview.accountDebit())
    println(overview.accountCredit())
    println(overview.accountNet())

    println(overview.describeAllFinancials())

  }
}
