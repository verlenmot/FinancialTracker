package testBaseClasses.Financials

import baseClasses.{Goal,Financials}
import com.github.nscala_time.time.Imports.DateTime
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class TestGoal extends AnyFlatSpec with should.Matchers {
  val testName: String = "stock"
  val testDescription: String = "example"
  val testStartDate: String = "2023-02-13"
  val testBalance: Double = 20
  val testAmount: Double = 10
  val testTarget: Double = 300
  val testEndDate: String = "2023-04-13"
  val testReached: Boolean = false
  val testConsumed: Boolean = true

  val testNewName: String = "newStock"
  val testNewDescription: String = "newDescription"
  val testNewStartDate: String = "2023-03-14"
  val testNewBalance: Double = 60

  // Instantiation
  "Goal" should "automatically create a companion object Goal" in {
    Goal.toString should be ("Goal")
  }

  it should "create a new instance of Goal using Goal()" in {
    Goal(name = testName, description = testDescription, startDate = DateTime.parse(testStartDate), currentBalance = testBalance,
      endDate = DateTime.parse(testEndDate), target = testTarget, reached = testReached, consumed = testConsumed) shouldBe a [Goal]
  }

  val goal: Goal = Goal(name = testName, description = testDescription, startDate = DateTime.parse(testStartDate), currentBalance = testBalance,
    endDate = DateTime.parse(testEndDate), target = testTarget, reached = testReached, consumed = testConsumed)

  // Types
  it should "be a subtype of Financials" in {
    goal shouldBe a [Financials]
  }

  // Getters
  it should "return name using getName" in {
    goal.getName should be (testName)
  }
  it should "return description using getDescription" in {
    goal.getDescription should be (testDescription)
  }
  it should "return start date using getStartDate" in {
    goal.getStartDate should be (DateTime.parse(testStartDate))
  }
  it should "return current balance using getBalance" in {
    goal.getBalance should be (testBalance)
  }
  it should "return end date using getEndDate" in {
    goal.getEndDate should be(DateTime.parse(testEndDate))
  }
  it should "return target balance using getTarget" in {
    goal.getTarget should be(testTarget)
  }
  it should "return whether the goal is reached using isReached" in {
    goal.isReached should be(testReached)
  }
  it should "return whether the goal is consumed using isConsumed" in {
    goal.isConsumed should be(testConsumed)
  }


  // Setters
  it should "change name using setName" in {
    goal.setName(testNewName).getName should be (testNewName)
  }
  it should "change description using setDescription" in {
    goal.setDescription(testNewDescription).getDescription should be (testNewDescription)
  }
  it should "change start date using setStartDate" in {
    goal.setStartDate(testNewStartDate).getStartDate should be (DateTime.parse(testNewStartDate))
  }
  it should "change balance using setBalance" in {
    goal.setBalance(testNewBalance).getBalance should be (testNewBalance)
  }
  it should "add balance using addBalance" in {
    goal.addBalance(testAmount).getBalance should be (testBalance + testAmount)
  }
  it should "remove balance using removeBalance" in {
    goal.removeBalance(testAmount).getBalance should be (testBalance - testAmount)
  }
  it should "return a string summary including arguments using apply" in {
    goal.apply() shouldBe a [String]
    goal.apply() should (include (testName) and include(testDescription)
      and include(DateTime.parse(testStartDate).toString) and include(testBalance.toString))
  }

  // Goal-specific methods

}

