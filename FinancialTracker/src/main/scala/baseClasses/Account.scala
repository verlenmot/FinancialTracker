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
  def accountDebit(): Map[String, Double] = recursiveAddMaps(List(calculatePoolListTotal(cash),calculatePoolListTotal(sights),
    calculatePoolListTotal(savings), calculatePoolListTotal(investments), calculatePoolListTotal(budgets), calculatePoolListTotal(goals)))
  def accountCredit(): Map[String, Double] = calculatePoolListTotal(debts)
  def accountNet(): Map[String, Double] = subtractMaps(accountDebit(), accountCredit())
  def poolsTotal[A <: Financials](list: List[Pool[A]]): Map[String, Double] = calculatePoolListTotal(list)

  def test(): Map[String, Double] = calculatePoolListTotal(cash)

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
    s"\n---Financial Overview---\n\nCash:\n\n${describePoolListFinancials(cash)}Sights:\n\n${describePoolListFinancials(sights)}Savings:\n\n${describePoolListFinancials(savings)}" +
      s"Investments:\n\n${describePoolListFinancials(investments)}Budgets:\n\n${describePoolListFinancials(budgets)}" +
      s"Goals:\n\n${describePoolListFinancials(goals)}Debts:\n\n${describePoolListFinancials(debts)}"
  }
  def describeSpecificFinancials[A <: Financials](list: List[Pool[A]]): String = describePoolListFinancials(list)
  def apply(): String = s"Type: Account\n Name: $name\n Description: $description\n Unallocated: $unallocated\n Cash: $cash\n " +
    s"Sights: $sights\n Savings: $savings\n Investment: $investments\n Budgets: $budgets\n Goals: $goals\n Debts: $debts\n"

  private def calculatePoolListTotal[A <: Financials](list: List[Pool[A]]): Map[String, Double] = {
    @tailrec
    def calculatePoolListTotalTail(n: Int = 0, accumulator: Map[String, Double] = Map.empty): Map[String, Double] = {
      if (n == list.length) accumulator
      else calculatePoolListTotalTail(n + 1, addMaps(accumulator, list(n).poolTotal))
    }
    calculatePoolListTotalTail()
  }

  private def addMaps(map1: Map[String, Double], map2: Map[String, Double]): Map[String, Double] = {
    (map1.keySet ++ map2.keySet).map {
        key =>
          key -> (map1.getOrElse(key, 0.0) + map2.getOrElse(key, 0.0))
      }.toMap
    }

  private def recursiveAddMaps(list: List[Map[String, Double]]): Map[String, Double] = {
    @tailrec
    def recursiveAddMapsTail(n: Int = 0, accumulator: Map[String, Double] = Map.empty): Map[String, Double] = {
      if (n == list.length) accumulator
      else recursiveAddMapsTail(n + 1, addMaps(accumulator, list(n)))
    }
    recursiveAddMapsTail()
  }

  private def subtractMaps(map1: Map[String, Double], map2: Map[String, Double]): Map[String, Double] = {
    val negativeMap = map2.map {
      case (key, value) => key -> -value
    }
    map1.map {
      case (key, value1) =>
        key -> (value1 + negativeMap.getOrElse(key, 0.00))
    }
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
      else describePoolListFinancialsTail(n + 1, accumulator + s"${list(n).describeFinancials()}\n")
    }
    describePoolListFinancialsTail()
  }
}

