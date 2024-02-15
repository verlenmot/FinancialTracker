package baseClasses

import scala.annotation.tailrec

case class Account(private val name: String, private val description: String, private val unallocated: Double,
                   private val cash: List[Pool[Cash]], private val sights: List[Pool[Sight]],
                   private val savings: List[Pool[Savings]], private val investments: List[Pool[Investment]],
                   private val budgets: List[Pool[Budget]], private val goals: List[Pool[Goal]], private val debts: List[Pool[Debt]]){
  def getName: String = name
  def getDescription: String = description
  def getStaging: Double = unallocated
  def setName(newName: String): Account = this.copy(name = newName)
  def setDescription(newDescription: String): Account = this.copy(description = newDescription)
  def setStaging(amount: Double): Account = this.copy(unallocated = amount)
  def addStaging(amount: Double): Account = this.copy(unallocated = unallocated + amount)
  def removeStaging(amount: Double): Account = this.copy(unallocated = unallocated - amount)
  def addToPools[A <: Financials](poolList: List[Pool[A]], pool: Pool[A]): Account = {
    (poolList, pool) match {
      case ca: (List[Pool[Cash]], Pool[Cash]) => this.copy(cash = cash :+ pool)
      case si: (List[Pool[Sight]], Pool[Sight]) => this.copy(sights = sights :+ pool)
      case sa: (List[Pool[Savings]], Pool[Savings]) => this.copy(savings = savings :+ pool)
      case in: (List[Pool[Investment]], Pool[Investment]) => this.copy(investments = poolList :+ pool)
      case bu: (List[Pool[Budget]], Pool[Budget]) => this.copy(budgets = poolList :+ pool)
      case go: (List[Pool[Goal]], Pool[Goal]) => this.copy(goals = poolList :+ pool)
      case de: (List[Pool[Debt]], Pool[Debt]) => this.copy(debts = poolList :+ pool)
      case other: (_, _) => throw new RuntimeException("Type mismatch")
    }
  }
  def removeFromPools[A <: Financials](poolList: List[Pool[A]], pool: Pool[A]): Account = {
    (poolList, pool) match {
      case ca: (List[Pool[Cash]], Pool[Cash]) => this.copy(cash = poolList.filter(_ == pool))
      case si: (List[Pool[Sight]], Pool[Sight]) => this.copy(sights = poolList.filter(_ == pool))
      case sa: (List[Pool[Savings]], Pool[Savings]) => this.copy(savings = poolList.filter(_ == pool))
      case in: (List[Pool[Investment]], Pool[Investment]) => this.copy(investments = poolList.filter(_ == pool))
      case bu: (List[Pool[Budget]], Pool[Budget]) => this.copy(budgets = poolList.filter(_ == pool))
      case go: (List[Pool[Goal]], Pool[Goal]) => this.copy(goals = poolList.filter(_ == pool))
      case de: (List[Pool[Debt]], Pool[Debt]) => this.copy(debts = poolList.filter(_ == pool))
      case other: (_, _) => throw new RuntimeException("Type mismatch")
    }
  }
  def accountDebit(): Double = calculatePoolListTotal(cash) + calculatePoolListTotal(sights) + calculatePoolListTotal(savings) +
    calculatePoolListTotal(investments) + calculatePoolListTotal(budgets) + calculatePoolListTotal(goals)
  def accountCredit(): Double = calculatePoolListTotal(debts)
  def accountNet(): Double = accountDebit() - accountCredit()
  def poolsTotal[A <: Financials](list: List[Pool[A]]): Double = calculatePoolListTotal(list)

  def listAllPools(): String = {
    s"Cash: ${listSpecificPools(cash)}\n Sights: ${listSpecificPools(sights)}\n Savings: ${listSpecificPools(savings)}\n " +
      s"Investments: ${listSpecificPools(investments)}\n + Budgets: ${listSpecificPools(budgets)}\n " +
      s"Goals: ${listSpecificPools(goals)}\n Debts: ${listSpecificPools(debts)}\n"
  }
  def listSpecificPools[A <: Financials](pools: List[Pool[A]]): String = pools.mkString(",")

  def listAllFinancials(): String = {
    s"Cash: ${listPoolListFinancials(cash)}\n Sights: ${listPoolListFinancials(sights)}\n Savings: ${listPoolListFinancials(savings)}\n " +
      s"Investments: ${listPoolListFinancials(investments)}\n + Budgets: ${listPoolListFinancials(budgets)}\n " +
      s"Goals: ${listPoolListFinancials(goals)}\n Debts: ${listPoolListFinancials(debts)}\n"
  }
  def listSpecificFinancials[A <: Financials](list: List[Pool[A]]): String = listPoolListFinancials(list)

  def describeAllFinancials(): String = {
    s"Cash: ${describePoolListFinancials(cash)}\n Sights: ${describePoolListFinancials(sights)}\n Savings: ${describePoolListFinancials(savings)}\n " +
      s"Investments: ${describePoolListFinancials(investments)}\n + Budgets: ${describePoolListFinancials(budgets)}\n " +
      s"Goals: ${describePoolListFinancials(goals)}\n Debts: ${describePoolListFinancials(debts)}\n"
  }
  def describeSpecificFinancials(list: List[Pool[Savings]]): String = describePoolListFinancials(list)
  def apply(): String = s"Type: Account\n Name: $name\n Description: $description\n Unallocated: $unallocated\n Cash: $cash\n " +
    s"Sights: $sights\n Savings: $savings\n Investment: $investments\n Budgets: $budgets\n Goals: $goals\n Debts: $debts\n"

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

