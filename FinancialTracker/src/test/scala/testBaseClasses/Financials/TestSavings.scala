package testBaseClasses.Financials

import baseClasses.{Savings,Financials}
import com.github.nscala_time.time.Imports.DateTime
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class TestSavings extends AnyFlatSpec with should.Matchers {
  val testName: String = "stock"
  val testDescription: String = "example"
  val testStartDate: String = "2023-02-13"
  val testBalance: Double = 20
  val testAmount: Double = 10

  val testNewName: String = "newStock"
  val testNewDescription: String = "newDescription"
  val testNewStartDate: String = "2023-03-14"
  val testNewBalance: Double = 60

  // Instantiation
  "Savings" should "automatically create a companion object Savings" in {
    Savings.toString should be ("Savings")
  }

  it should "create a new instance of Savings using Savings()" in {
    Savings(name = testName, description = testDescription, startDate = DateTime.parse(testStartDate), currentBalance = testBalance) shouldBe a [Savings]
  }

  val savings: Savings = Savings(name = testName, description = testDescription, startDate = DateTime.parse(testStartDate), currentBalance = testBalance)

  // Types
  it should "be a subtype of Financials" in {
    savings shouldBe a[Financials]
  }

  // Getters
  it should "return name using getName" in {
    savings.getName should be (testName)
  }
  it should "return description using getDescription" in {
    savings.getDescription should be (testDescription)
  }
  it should "return start date using getStartDate" in {
    savings.getStartDate should be (DateTime.parse(testStartDate))
  }
  it should "return current balance using getBalance" in {
    savings.getBalance should be (testBalance)
  }

  // Setters
  it should "change name using setName" in {
    savings.setName(testNewName).getName should be (testNewName)
  }
  it should "change description using setDescription" in {
    savings.setDescription(testNewDescription).getDescription should be (testNewDescription)
  }
  it should "change start date using setStartDate" in {
    savings.setStartDate(testNewStartDate).getStartDate should be (DateTime.parse(testNewStartDate))
  }
  it should "change balance using setBalance" in {
    savings.setBalance(testNewBalance).getBalance should be (testNewBalance)
  }
  it should "correctly add balance using addBalance" in {
    savings.addBalance(testAmount).getBalance should be (testBalance + testAmount)
  }
  it should "correctly remove balance using removeBalance" in {
    savings.removeBalance(testAmount).getBalance should be (testBalance - testAmount)
  }
  it should "return a string summary including arguments using apply" in {
    savings.apply() shouldBe a [String]
    savings.apply() should (include (testName) and include(testDescription)
      and include(DateTime.parse(testStartDate).toString) and include(testBalance.toString))
  }}

