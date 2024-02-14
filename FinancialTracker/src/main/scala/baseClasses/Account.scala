package baseClasses

import scala.annotation.tailrec
import scala.reflect.ClassTag

case class Account(private val name: String, private val description: String, private val staging: Double,
                   private val savings: List[Pool[Savings]], private val investments: List[Pool[Investment]],
                   private val budgets: List[Pool[Budget]], private val goals: List[Pool[Goal]],
                   private val debts: List[Pool[Debt]], private val active: Boolean) {
  def getName: String = name
  def getDescription: String = description
  def getStaging: Double = staging
  def setName(newName: String): Account = this.copy(name = newName)
  def setDescription(newDescription: String): Account = this.copy(description = newDescription)
  def setStaging(amount: Double): Account = this.copy(staging = amount)
  def addStaging(amount: Double): Account = this.copy(staging = staging + amount)
  def removeStaging(amount: Double): Account = this.copy(staging = staging - amount)
  def addToPools[A <: Financials](poolList: List[Pool[A]], pool: Pool[A]): Account = {
    (poolList, pool) match {
      case s: (List[Pool[Savings]], Pool[Savings]) => this.copy(savings = savings :+ pool)
      case i: (List[Pool[Investment]], Pool[Investment]) => this.copy(investments = poolList :+ pool)
      case b: (List[Pool[Budget]], Pool[Budget]) => this.copy(budgets = poolList :+ pool)
      case g: (List[Pool[Goal]], Pool[Goal]) => this.copy(goals = poolList :+ pool)
      case d: (List[Pool[Debt]], Pool[Debt]) => this.copy(debts = poolList :+ pool)
      case other: (_, _) => throw new RuntimeException("Type mismatch")
    }
  }
  def removeFromPools[A <: Financials](poolList: List[Pool[A]], pool: Pool[A]): Account = {
    (poolList, pool) match {
      case s: (List[Pool[Savings]], Pool[Savings]) => this.copy(savings = poolList.filter(_ == pool))
      case i: (List[Pool[Investment]], Pool[Investment]) => this.copy(investments = poolList.filter(_ == pool))
      case b: (List[Pool[Budget]], Pool[Budget]) => this.copy(budgets = poolList.filter(_ == pool))
      case g: (List[Pool[Goal]], Pool[Goal]) => this.copy(goals = poolList.filter(_ == pool))
      case d: (List[Pool[Debt]], Pool[Debt]) => this.copy(debts = poolList.filter(_ == pool))
      case other: (_, _) => throw new RuntimeException("Type mismatch")
    }
  }
  def accountDebit(): Double = calculatePoolListTotal(savings) + calculatePoolListTotal(investments) + calculatePoolListTotal(budgets) + calculatePoolListTotal(goals)
  def accountCredit(): Double = calculatePoolListTotal(debts)
  def accountNet(): Double = accountDebit() - accountCredit()
  def poolsTotal(list: List[Pool[Savings]]): Double = calculatePoolListTotal(list)

  def listAllPools(): String = {
    s"Savings: ${listSpecificPools(savings)}\n Investments: ${listSpecificPools(investments)}\n Budgets: ${listSpecificPools(budgets)}\n " +
      s"Goals: ${listSpecificPools(goals)}\n Debts: ${listSpecificPools(debts)}\n"
  }
  def listSpecificPools[A <: Financials](pools: List[Pool[A]]): String = pools.mkString(",")

  def listAllFinancials(): String = {
    s"Savings: ${listPoolListFinancials(savings)}\n Investments: ${listPoolListFinancials(investments)}\n Budgets: ${listPoolListFinancials(budgets)}\n " +
      s"Goals: ${listPoolListFinancials(goals)}\n Debts: ${listPoolListFinancials(debts)}\n"}
  def listSpecificFinancials[A <: Financials](list: List[Pool[A]]): String = listPoolListFinancials(list)

  def describeAllFinancials(): String = {
    s"Financial Overview\n\nSavings:\n ${describePoolListFinancials(savings)}\nInvestments:\n ${describePoolListFinancials(investments)}\nBudgets:\n ${describePoolListFinancials(budgets)}\n" +
      s"Goals:\n ${describePoolListFinancials(goals)}\nDebts:\n ${describePoolListFinancials(debts)}\n"
  }
  def describeSpecificFinancials(list: List[Pool[Savings]]): String = describePoolListFinancials(list)
  def apply(): String = s"Type: Account\n Name: $name\n Description: $description\n Staging: $staging\n" +
    s"Savings: $savings\n Investment: $investments\n Budgets: $budgets\n Goals: $goals\n Debts: $debts\n Active: $active"

  private def calculatePoolListTotal[A <: Financials](list: List[Pool[A]]): Double = {
    @tailrec
    def calculatePoolListTotalTail(n: Int = 0, accumulator: Double = 0): Double = {
      if (n == list.length) accumulator
      else calculatePoolListTotalTail(n + 1, accumulator + list(n).poolTotal)
    }
    calculatePoolListTotalTail()
  }

  private def listPoolListFinancials[A <: Financials](list: List[Pool[A]]): String = {
    @tailrec
    def listPoolListFinancialsTail(n: Int = 0, accumulator: String = ""): String = {
      if (n == list.length) accumulator
      else listPoolListFinancialsTail(n + 1, accumulator + s"${list(n).listFinancials()}")
    }
    listPoolListFinancialsTail()
  }

  private def describePoolListFinancials[A <: Financials](list: List[Pool[A]]): String = {
    @tailrec
    def describePoolListFinancialsTail(n: Int = 0, accumulator: String = ""): String = {
      if (n == list.length) accumulator
      else describePoolListFinancialsTail(n + 1, accumulator + s"${list(n).describeFinancials()}")
    }
    describePoolListFinancialsTail()
  }
}

