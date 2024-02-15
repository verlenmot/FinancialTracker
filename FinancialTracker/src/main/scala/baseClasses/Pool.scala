package baseClasses

import scala.annotation.tailrec
import scala.reflect.ClassTag

case class Pool[A <: Financials](private val name: String, private val description: String, private val pool: List[A]) {
  def getName: String = name
  def getDescription: String = description
  def setName(newName: String): Pool[A] = this.copy(name = newName)
  def setDescription(newDescription: String): Pool[A] = this.copy(description = newDescription)
  def addFinancials(item: A): Pool[A] = this.copy(pool = pool :+ item)
  def removeFinancials(item: A): Pool[A] = this.copy(pool = pool.filter(_ == item))
  def poolTotal(): Map[String, Double] = {
    @tailrec
    def poolTotalTail(n: Int = 0, accumulator: Map[String, Double] = Map.empty): Map[String, Double] = {
      if (n == pool.length) accumulator
      else poolTotalTail(n + 1, accumulator.updated(pool(n).getCurrency, accumulator.getOrElse(pool(n).getCurrency, 0) + pool(n).getBalance))
    }
    poolTotalTail()
  }
  def listFinancials(): String = pool.mkString(",")
  def describeFinancials(): String = {
    @tailrec
    def describeFinancialsTail(n: Int = 0, accumulator: String = s"Pool: ${this.getName} - "): String = {
      if (n == pool.length) accumulator
      else describeFinancialsTail(n + 1, accumulator + s"Name: ${pool(n).getName} - Balance: ${pool(n).getCurrency}${pool(n).getBalance}\n")
    }
    describeFinancialsTail()
  }
  def apply(): String = s"Type: ${this.getClass}\n Name: $name\n Description: $description\n Financials: ${this.listFinancials()}"
}