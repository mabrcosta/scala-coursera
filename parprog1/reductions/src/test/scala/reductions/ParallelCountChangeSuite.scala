package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelCountChange._

@RunWith(classOf[JUnitRunner])
class ParallelCountChangeSuite extends FunSuite {

  /**
    * Sequencial
    */

  test("countChange should return 0 for money < 0") {
    def check(money: Int, coins: List[Int]) = 
      assert(countChange(money, coins) == 0,
        s"countChang($money, _) should be 0")

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  test("countChange should return 1 when money == 0") {
    def check(coins: List[Int]) = 
      assert(countChange(0, coins) == 1,
        s"countChang(0, _) should be 1")

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  test("countChange should return 0 for money > 0 and coins = List()") {
    def check(money: Int) = 
      assert(countChange(money, List()) == 0,
        s"countChang($money, List()) should be 0")

    check(1)
    check(Int.MaxValue)
  }

  test("countChange should work when there is only one coin") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

  test("countChange should work for multi-coins") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }


  /**
    * Parallel
    */
  test("parCountChange should return 0 for money < 0") {
    def check(money: Int, coins: List[Int]) =
      assert(parCountChange(money, coins, combinedThreshold(money, coins)) == 0,
        s"countChang($money, _) should be 0")

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  test("parCountChange should return 1 when money == 0") {
    def check(coins: List[Int]) =
      assert(parCountChange(0, coins, combinedThreshold(0, coins)) == 1,
        s"countChang(0, _) should be 1")

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  test("parCountChange should return 0 for money > 0 and coins = List()") {
    def check(money: Int) =
      assert(parCountChange(money, List(), combinedThreshold(money, List())) == 0,
        s"countChang($money, List()) should be 0")

    check(1)
    check(Int.MaxValue)
  }

  test("parCountChange should work when there is only one coin") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChange(money, coins, combinedThreshold(money, coins)) == expected,
        s"countChange($money, $coins) should be $expected")

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

  test("parCountChange should work for multi-coins") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChange(money, coins, combinedThreshold(money, coins)) == expected,
        s"countChange($money, $coins) should be $expected")

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }

  test("parCountChange should invoke the parallel construct 6 times for money == 16, coins == List(1) and moneyThreshold(16)") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChange(money, coins, moneyThreshold(money)) == expected,
        s"countChange($money, $coins) should be $expected")

    check(16, List(1), 1)
  }

  test("moneyThreshold should return false when the money is greater than two-thirds of the starting money") {
    def check(money: Int, coins: List[Int], startingMoney: Int, expected: Boolean) =
      assert(moneyThreshold(startingMoney)(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(3, List(1), 3, false)
    check(2, List(1), 3, true)
    check(10, List(1), 15, true)
    check(11, List(1), 15, false)
    check(9, List(1), 15, true)
    check(11, List(1), 16, false)
    check(10, List(1), 16, true)
  }

  test("parCountChange with totalCoinsThreshold should produce correct result when there is only one coin and the amount is less than the value of the coin") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChange(money, coins, totalCoinsThreshold(coins.size)) == expected,
        s"countChange($money, $coins) should be $expected")

    check(1, List(2), 0)
  }
}
