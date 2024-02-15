package baseClasses

import com.github.nscala_time.time.Imports._

class Financials(private val name: String, private val description: String, private val startDate: DateTime, private val balance: Double) {
  def getName: String = name
  def getDescription: String = description
  def getStartDate: DateTime = startDate
  def getBalance: Double = balance
  def setName(newName: String): Financials = new Financials(newName, description, startDate, balance)
  def setDescription(newDescription: String): Financials = new Financials(name, newDescription, startDate, balance)
  def setStartDate(newStartDate: String): Financials = new Financials(name, description, DateTime.parse(newStartDate), balance)
  def setBalance(newBalance: Double): Financials = new Financials(name, description, startDate, newBalance)
  def addBalance(amount: Double): Financials = new Financials(name, description, startDate, balance + amount)
  def removeBalance(amount: Double): Financials = new Financials(name, description, startDate, balance - amount)
  def apply(): String = s"Type: ${this.getClass}\n Name: $name\n Description: $description\n StartDate: $startDate\n Balance: $balance"
}

case class Sight(private val name: String, private val description: String, private val startDate: DateTime, private val balance: Double)
  extends Financials(name, description, startDate: DateTime, balance)
case class Investment(private val name: String, private val description: String, private val startDate: DateTime, private val balance: Double)
                      extends Financials(name, description, startDate: DateTime, balance) {
  override def apply(): String = s"Type: Investment \n Name: $name\n Description: $description\n StartDate: $startDate\n Balance: $balance"
}

case class Savings(private val name: String, private val description: String, private val startDate: DateTime, private val balance: Double)
                      extends Financials(name, description, startDate: DateTime, balance)

case class Budget(private val name: String, private val description: String, private val startDate: DateTime, private val balance: Double,
                  private val budget: Double, private val frequency: String, private val reached: Boolean)
                  extends Financials(name, description, startDate, balance) {
  def getBudget: Double = budget
  def getFrequency: String = frequency
  def isReached(): Boolean = reached
  def setBudget(newAmount: Double): Budget = this.copy(budget = newAmount)
  def setFrequency(newFrequency: String): Budget = this.copy(frequency = newFrequency)
  def setReached(condition: Boolean): Budget = this.copy(reached = condition)
  def budgetConsumed(): Double = this.budget - this.balance
  def budgetRemaining(): Double = this.balance
  def getRenewalDate(): DateTime = ???
  override def apply(): String = s"Type: ${this.getClass}\n Name: $name\n Description: $description\n StartDate: $startDate\n Balance: $balance\n" +
    s"Budget: $budget\n Frequency: $frequency\n Reached: $reached"
}

case class Debt(private val name: String, private val description: String, private val startDate: DateTime, private val balance: Double,
                private val interest: Double, private val percentage: Double, private val frequency: String, private val endDate: DateTime,  private val paid: Boolean)
                extends Financials(name, description, startDate, balance) {
  def getInterest: Double = interest
  def getPercentage: Double = percentage
  def getFrequency: String = frequency
  def getEndDate: DateTime = endDate
  def isPaid(): Boolean = paid
  def setInterest(newInterest: Double): Debt = this.copy(interest = newInterest)
  def setPercentage(newPercentage: Double): Debt = this.copy(percentage = newPercentage)
  def setFrequency(newFrequency: String): Debt = this.copy(frequency = newFrequency)
  def setEndDate(newEndDate: String): Debt = this.copy(endDate = DateTime.parse(newEndDate))
  def setPaid(condition: Boolean): Debt = this.copy(paid = condition)
  override def apply(): String = s"Type: ${this.getClass}\n Name: $name\n Description: $description\n StartDate: $startDate\n Balance: $balance\n" +
    s"Interest: $interest\n Percentage: $percentage\n Frequency: $frequency\n EndDate: $endDate\n Paid: $paid"
}

case class Goal(private val name: String, private val description: String, private val startDate: DateTime, private val balance: Double,
                private val target: Double, private val endDate: DateTime, private val reached: Boolean, private val consumed: Boolean)
                extends Financials(name, description, startDate, balance) {
  def getTarget: Double = target
  def getEndDate: DateTime = endDate
  def isReached(): Boolean = reached
  def isConsumed(): Boolean = consumed
  def setTarget(amount: Double): Goal = this.copy(target = amount)
  def setEndDate(newEndDate: String): Goal = this.copy(endDate = DateTime.parse(newEndDate))
  def setReached(condition: Boolean): Goal = this.copy(reached = condition)
  def setConsumed(condition: Boolean): Goal = this.copy(consumed = condition)
  override def apply(): String = s"Type: ${this.getClass}\n Name: $name\n Description: $description\n StartDate: $startDate\n Balance: $balance\n" +
    s"Target: $target\n EndDate: $endDate\n Reached: $reached\n Consumed: $consumed"
}
