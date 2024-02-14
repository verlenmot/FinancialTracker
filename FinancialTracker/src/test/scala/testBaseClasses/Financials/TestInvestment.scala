package testBaseClasses.Financials

import baseClasses.{Investment,Financials}
import com.github.nscala_time.time.Imports.DateTime
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class TestInvestment extends AnyFlatSpec with should.Matchers {
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
  "Investments"  should "automatically create a companion object Investment" in {
    Investment.toString should be ("Investment")
  }

  it should "create a new instance of Investment using Investment()" in {
    Investment(name = testName, description = testDescription, startDate = DateTime.parse(testStartDate), currentBalance = testBalance) shouldBe a [Investment]
  }

  val investment: Investment = Investment(name = testName, description = testDescription, startDate = DateTime.parse(testStartDate), currentBalance = testBalance)

  // Types
   it should "be a subtype of Financials" in {
    investment shouldBe a[Financials]
  }

  // Getters
  it should "return name using getName" in {
    investment.getName should be (testName)
  }
  it should "return description using getDescription" in {
    investment.getDescription should be (testDescription)
  }
  it should "return start date using getStartDate" in {
    investment.getStartDate should be (DateTime.parse(testStartDate))
  }
  it should "return current balance using getBalance" in {
    investment.getBalance should be (testBalance)
  }

  // Setters
  it should "change name using setName" in {
    investment.setName(testNewName).getName should be (testNewName)
  }
  it should "change description using setDescription" in {
    investment.setDescription(testNewDescription).getDescription should be (testNewDescription)
  }
  it should "change start date using setStartDate" in {
    investment.setStartDate(testNewStartDate).getStartDate should be (DateTime.parse(testNewStartDate))
  }
  it should "change balance using setBalance" in {
    investment.setBalance(testNewBalance).getBalance should be (testNewBalance)
  }
  it should "correctly add balance using addBalance" in {
    investment.addBalance(testAmount).getBalance should be (testBalance + testAmount)
  }
  it should "correctly remove balance using removeBalance" in {
    investment.removeBalance(testAmount).getBalance should be (testBalance - testAmount)
  }
  it should "return a string summary including arguments using apply" in {
    investment.apply() shouldBe a [String]
    investment.apply() should (include (testName) and include(testDescription)
      and include(DateTime.parse(testStartDate).toString) and include(testBalance.toString))
}}
