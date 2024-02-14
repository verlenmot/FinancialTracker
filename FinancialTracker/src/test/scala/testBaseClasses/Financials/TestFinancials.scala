package testBaseClasses.Financials

import baseClasses.Financials
import com.github.nscala_time.time.Imports.DateTime
import org.scalatest.flatspec._
import org.scalatest.matchers._

class TestFinancials extends AnyFlatSpec with should.Matchers {
  val testName: String = "financial"
  val testDescription: String = "example"
  val testStartDate: String = "2023-02-13"
  val testBalance: Double = 20
  val testAmount: Double = 10

  val testNewName: String = "newName"
  val testNewDescription: String = "newDescription"
  val testNewStartDate: String = "2023-03-14"
  val testNewBalance: Double = 60

  val financials = new Financials(name = testName, description = testDescription, startDate = DateTime.parse(testStartDate), currentBalance = testBalance)

  // Types
  "Financials" should "create a new instance of Financials using new Financials()" in {
    new Financials(name = testName, description = testDescription, startDate = DateTime.parse(testStartDate), currentBalance = testBalance) shouldBe a [Financials]
  }

  // Getters
  it should "return name using getName" in {
    financials.getName should be (testName)
  }
  it should "return description using getDescription" in {
    financials.getDescription should be (testDescription)
  }
  it should "return start date using getStartDate" in {
    financials.getStartDate should be (DateTime.parse(testStartDate))
  }
  it should "return current balance using getBalance" in {
    financials.getBalance should be (testBalance)
  }

  // Setters
  it should "change name using setName" in {
    financials.setName(testNewName).getName should be (testNewName)
  }
  it should "change description using setDescription" in {
    financials.setDescription(testNewDescription).getDescription should be (testNewDescription)
  }
  it should "change start date using setStartDate" in {
    financials.setStartDate(testNewStartDate).getStartDate should be (DateTime.parse(testNewStartDate))
  }
  it should "change balance using setBalance" in {
    financials.setBalance(testNewBalance).getBalance should be (testNewBalance)
  }
  it should "correctly add balance using addBalance" in {
    financials.addBalance(testAmount).getBalance should be (testBalance + testAmount)
  }
  it should "correctly remove balance using removeBalance" in {
    financials.removeBalance(testAmount).getBalance should be (testBalance - testAmount)
  }
  it should "return a string summary including arguments using apply" in {
    financials.apply() shouldBe a [String]
    financials.apply() should (include (testName) and include(testDescription)
      and include(DateTime.parse(testStartDate).toString) and include(testBalance.toString))
  }
}

