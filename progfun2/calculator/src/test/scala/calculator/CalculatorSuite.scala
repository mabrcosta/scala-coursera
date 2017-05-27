package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("colorForRemainingCharsCount with a variable signal") {
    val signal = Var(52)
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(signal)
    assert(resultGreen1() == "green")

    signal.update(10)
    assert(resultGreen1() == "orange")
  }

  test("calculator_a=7+b_b=1") {
    val mapVal = Map[String, Signal[Expr]]("a" -> Signal(Plus(Literal(7), Ref("b"))), "b" -> Signal(Literal(1)))
    val expected = Map("a" -> Signal(8.0), "b" -> Signal(1.0))
    val result = Calculator.computeValues(mapVal)
    assert(result.size == expected.size)
    for ((r, e) <- result zip expected) yield  {
      assert(r._1 == e._1)
      assert(r._2() == e._2())
    }
  }

  test("calculator_CyclicRef_a=7+b_b=1+a") {
    val mapVal = Map[String, Signal[Expr]]("a" -> Signal(Plus(Literal(7), Ref("b"))), "b" -> Signal(Plus(Literal(1), Ref("a"))))
    val expected = Map("a" -> Signal(Double.NaN), "b" -> Signal(Double.NaN))
    val result = Calculator.computeValues(mapVal)
    assert(result.size == expected.size)
    for ((r, e) <- result zip expected) yield  {
      assert(r._1 == e._1)
      assert(r._2().isNaN)
      assert(e._2().isNaN)
    }
  }

}
