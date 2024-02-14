package baseClasses

import com.github.nscala_time.time.Imports._

class Financials(private val name: String, private val description: String, private val startDate: DateTime, private val currentBalance: Double) {
  def getName: String = name
  def getDescription: String = description
  def getStartDate: DateTime = startDate
  def getBalance: Double = currentBalance
  def setName(newName: String): Financials = new Financials(newName, description, startDate, currentBalance)
  def setDescription(newDescription: String): Financials = new Financials(name, newDescription, startDate, currentBalance)
  def setStartDate(newStartDate: String): Financials = new Financials(name, description, DateTime.parse(newStartDate), currentBalance)
  def setBalance(newBalance: Double): Financials = new Financials(name, description, startDate, newBalance)
  def addBalance(amount: Double): Financials = new Financials(name, description, startDate, currentBalance + amount)
  def removeBalance(amount: Double): Financials = new Financials(name, description, startDate, currentBalance - amount)
  def apply(): String = s"Type: ${this.getClass}\n Name: $name\n Description: $description\n StartDate: $startDate\n Balance: $currentBalance"
}

case class Investment(private val name: String, private val description: String, private val startDate: DateTime, private val currentBalance: Double)
                      extends Financials(name, description, startDate: DateTime, currentBalance) {
  override def apply(): String = s"Type: Investment \n Name: $name\n Description: $description\n StartDate: $startDate\n Balance: $currentBalance"
}

case class Savings(private val name: String, private val description: String, private val startDate: DateTime, private val currentBalance: Double)
                      extends Financials(name, description, startDate: DateTime, currentBalance)

case class Budget(private val name: String, private val description: String, private val startDate: DateTime, private val currentBalance: Double,
                  private val budgetedBalance: Double, private val frequency: String, private val reached: Boolean)
                  extends Financials(name, description, startDate, currentBalance) {
  def getBudgetedBalance: Double = budgetedBalance
  def getFrequency: String = frequency
  def isReached(): Boolean = reached
  def setBudgetedBalance(newAmount: Int): Budget = this.copy(budgetedBalance = newAmount)
  def setFrequency(newFrequency: String): Budget = this.copy(frequency = newFrequency)
  def setReached(condition: Boolean): Budget = this.copy(reached = condition)
  def getRenewalDate(): DateTime = ???
  override def apply(): String = s"Type: ${this.getClass}\n Name: $name\n Description: $description\n StartDate: $startDate\n Balance: $currentBalance\n" +
    s"Budget: $budgetedBalance\n Frequency: $frequency\n Reached: $reached"
}

case class Debt(private val name: String, private val description: String, private val startDate: DateTime, private val currentBalance: Double,
                private val endDate: DateTime, private val interestPercentage: Double, private val frequency: String, private val paid: Boolean)
                extends Financials(name, description, startDate, currentBalance) {
  def getEndDate: DateTime = endDate
  def getInterestPercentage: Double = interestPercentage
  def getFrequency: String = frequency
  def isPaid(): Boolean = paid
  def setEndDate(newEndDate: String): Debt = this.copy(endDate = DateTime.parse(newEndDate))
  def setInterestPercentage(newInterestPercentage: Double): Debt = this.copy(interestPercentage = newInterestPercentage)
  def setFrequency(newFrequency: String): Debt = this.copy(frequency = newFrequency)
  def setPaid(condition: Boolean): Debt = this.copy(paid = condition)
  override def apply(): String = s"Type: ${this.getClass}\n Name: $name\n Description: $description\n StartDate: $startDate\n Balance: $currentBalance\n" +
    s"EndDate: $endDate\n InterestPercentage: $interestPercentage\n Frequency: $frequency\n Paid: $paid"
}

case class Goal(private val name: String, private val description: String, private val startDate: DateTime, private val currentBalance: Double,
                private val target: Double, private val endDate: DateTime, private val reached: Boolean, private val consumed: Boolean)
                extends Financials(name, description, startDate, currentBalance) {
  def getTarget: Double = target
  def getEndDate: DateTime = endDate
  def isReached(): Boolean = reached
  def isConsumed(): Boolean = consumed
  def setTarget(amount: Double): Goal = this.copy(target = amount)
  def setEndDate(newEndDate: String): Goal = this.copy(endDate = DateTime.parse(newEndDate))
  def setReached(condition: Boolean): Goal = this.copy(reached = condition)
  def setConsumed(condition: Boolean): Goal = this.copy(consumed = condition)
  override def apply(): String = s"Type: ${this.getClass}\n Name: $name\n Description: $description\n StartDate: $startDate\n Balance: $currentBalance\n" +
    s"Target: $target\n EndDate: $endDate\n Reached: $reached\n Consumed: $consumed"
}
