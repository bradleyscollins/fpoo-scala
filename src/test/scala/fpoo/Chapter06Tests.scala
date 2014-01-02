package fpoo

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks
import fpoo.Chapter06._

class FactorialSpec extends UnitSpec with TableDrivenPropertyChecks {
  "Chapter 6, Exercise 1: factorial" should "compute the factorial, i.e., n!, of a given integer n >= 0" in {
    val factorials = Table(
      ("n", "fact"),
      (0,   1),
      (1,   1),
      (2,   2),
      (3,   6),
      (4,  24),
      (5, 120)
    )

    forAll (factorials) { (n: Int, fact: Int) =>
      whenever (n >= 0) {
        factorial(n) should be (fact)
      }
    }
  }
}

class Factorial2Spec extends UnitSpec with TableDrivenPropertyChecks {
  "Chapter 6, Exercise 2: factorial2" should "compute the factorial, i.e., n!, of a given integer n >= 0" in {
    val factorials = Table(
      ("n", "fact"),
      (0,   1),
      (1,   1),
      (2,   2),
      (3,   6),
      (4,  24),
      (5, 120)
    )

    forAll (factorials) { (n: Int, fact: Int) =>
      whenever (n >= 0) {
        factorial2(n) should be (fact)
      }
    }
  }
}

class SumSequenceSpec extends UnitSpec {
  "Chapter 6, Exercise 3: sumSequence" should "sum all of the elements in a sequence starting with an initial value" in {
    sumSequence(Seq(2, 4, 6, 8), 0) should be (20)
  }
  it should "return 0 if the sequence is empty" in {
    sumSequence(Seq[Int](), 0) should be (0)
  }
}

class ProdSequenceSpec extends UnitSpec {
  "Chapter 6, Exercise 4: prodSequence" should "multiply all of the elements in a sequence starting with an initial value" in {
    prodSequence(Seq(2, 4, 6, 8), 1) should be (384)
  }
  it should "return 1 if the sequence is empty" in {
    prodSequence(Seq[Int](), 1) should be (1)
  }
}

class ReduceSequenceSpec extends UnitSpec {
  val op = (a: Int, b: Int) => a * b
  "Chapter 6, Exercise 4a: reduceSequence" should "apply an operation all of the elements in a sequence starting with an initial value" in {
    reduceSequence(op, Seq(2, 4, 6, 8), 1) should be (384)
  }
  it should "return init if the sequence is empty" in {
    reduceSequence(op, Seq[Int](), 1) should be (1)
  }
}

class ReduceSequenceVecToMapSpec extends UnitSpec {
  val op = (a: String, b: Map[String, Int]) => b + (a -> 0)
  "Chapter 6, Exercise 5: reduceSequence" should "convert a sequence of strings into a map keyed to those strings with values of zero" in {
    reduceSequence(op, Vector("a", "b", "c"), Map[String, Int]()) should be (Map("a" -> 0, "b" -> 0, "c" -> 0))
  }
  it should "return init if the sequence is empty" in {
    reduceSequence(op, Seq[String](), Map[String, Int]()) should be (Map[String, Int]())
  }
  val op2 = (a: String, b: Map[String, Int]) => b + (a -> (b.size + 1))
  "Chapter 6, Exercise 5a: reduceSequence" should "convert a sequence of strings into a map keyed to those strings with values that increment by 1" in {
    reduceSequence(op2, Vector("a", "b", "c"), Map[String, Int]()) should be (Map("a" -> 1, "b" -> 2, "c" -> 3))
  }
}

