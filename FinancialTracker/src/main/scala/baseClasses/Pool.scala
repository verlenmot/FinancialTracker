package baseClasses

import scala.reflect.ClassTag

case class Pool[A <: Financials: ClassTag](private val name: String, private val description: String,
                                           private val pool: Array[A], private val active: Boolean) {
  def getName: String = name
  def getDescription: String = description
  def setName(newName: String): Pool[A] = this.copy(name = newName)
  def setDescription(newDescription: String): Pool[A] = this.copy(description = newDescription)
  def addItem(item: A): Pool[A] = this.copy(pool = pool :+ item)
  def removeItem(item: A): Pool[A] = this.copy(pool = pool.filter(_ == item))
  def listItems(): String = pool.mkString(",")
  def total(): Double = {
    def tailTotal(n: Int = 0, accumulator: Double = 0): Double = {
      if (n == pool.length) accumulator
      else tailTotal(n + 1, accumulator + pool(n).getBalance)
    }
    tailTotal()
  }
  def detailedListing(): String = {
    def tailDetailed(n: Int = 0, accumulator: String = s"Pool: ${this.getName} - "): String = {
      if (n == pool.length) accumulator
      else tailDetailed(n + 1, accumulator + s"Name: ${pool(n).getName} - Balance: ${pool(n).getBalance}\n")
    }
    tailDetailed()
  }
  def apply(): String = s"Type: ${this.getClass}\n Name: $name\n Description: $description\n Items: ${this.listItems}\n Active: $active"
}