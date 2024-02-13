package baseClasses

import com.github.nscala_time.time.Imports._

import scala.reflect.ClassTag

case class Account(private val name: String, private val description: String, private val staging: Double,
                   private val savings: Array[Pool[Savings]], private val investments: Array[Pool[Investment]],
                   private val budgets: Array[Pool[Budget]], private val goals: Array[Pool[Goal]],
                   private val debts: Array[Pool[Debt]], private val active: Boolean) {
  def getName: String = name
  def getDescription: String = description
  def getStaging: Double = staging
  def setName(newName: String): Account = this.copy(name = newName)
  def setDescription(newDescription: String): Account = this.copy(description = newDescription)
  def setStaging(amount: Double): Account = this.copy(staging = amount)
  def addStaging(amount: Double): Account = this.copy(staging = staging + amount)
  def removeStaging(amount: Double): Account = this.copy(staging = staging - amount)
  def accountDebit(): Double = arrayTotal(savings) + arrayTotal(investments) + arrayTotal(budgets) + arrayTotal(goals)
  def accountCredit(): Double = arrayTotal(debts)
  def accountNet(): Double = accountDebit() - accountCredit()
  def listAllPools(): String = {
    s"Savings: ${arrayList(savings)}/n Investments: ${arrayList(investments)}/n Budgets: ${arrayList(budgets)}/n " +
      s"Goals: ${arrayList(goals)}/n Debts: ${arrayList(debts)}/n"
  }
  def listPoolsTotal(array: Array[Pool[Savings]]): Double = arrayTotal(array)
  def listSpecificPools(array: Array[Pool[Savings]]): String = arrayList(array)
  def listAllFinancials(): String = {
    s"Savings: ${arrayFinancials(savings)}/n Investments: ${arrayFinancials(investments)}/n Budgets: ${arrayFinancials(budgets)}/n " +
      s"Goals: ${arrayFinancials(goals)}/n Debts: ${arrayFinancials(debts)}/n"
  }
  def listSpecificFinancials(array: Array[Pool[Savings]]): String = arrayFinancials(array)
  def apply(): String = ???

  private def arrayTotal[A <: Financials](array: Array[Pool[A]]): Double = {
    def arrayTail(n: Int = 0, accumulator: Double = 0): Double = {
      if (n == array.length) accumulator
      else arrayTail(n + 1, accumulator + array(n).total)
    }
    arrayTail()
  }

  private def arrayList[A <: Financials](array: Array[Pool[A]]): String = {
    def arrayTail(n: Int = 0, accumulator: String = ""): String = {
      if (n == array.length) accumulator
      else arrayTail(n + 1, accumulator + s"${array(n).detailedListing()}")
    }
    arrayTail()
  }

  private def arrayFinancials[A <: Financials](array: Array[Pool[A]]): String = {
    def tailFinancials(n: Int = 0, accumulator: String = ""): String = {
      if (n == array.length) accumulator
      else tailFinancials(n + 1, accumulator + s"${array(n).listItems()}")
    }
    tailFinancials()
  }
}

