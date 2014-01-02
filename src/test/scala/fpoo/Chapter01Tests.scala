package fpoo

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks
import fpoo.Chapter01._

class SecondSpec extends UnitSpec {
  "Chapter 1, Exercise 1: second" should "return the second item in a given list" in {
    val list = List("Lorem", "ipsum", "dolor", "sit", "amet")
    second(list) should be ("ipsum")
  }
  it should "throw IndexOutOfBoundsException if called on a list with fewer than 2 elements" in {
    val listOf1 = List("sole")
    a [IndexOutOfBoundsException] should be thrownBy {
      second(listOf1)
    } 
  }
}

class ThirdSpec extends UnitSpec {
  "Chapter 1, Exercise 2a: third" should "return the third item in a given list" in {
    val list = List("Lorem", "ipsum", "dolor", "sit", "amet")
    third(list) should be ("dolor")
  }
  it should "throw IndexOutOfBoundsException if called on a list with fewer than 3 elements" in {
    val listOf2 = List("penultimate", "ultimate")
    a [IndexOutOfBoundsException] should be thrownBy {
      third(listOf2)
    } 
  }

  "Chapter 1, Exercise 2b: third2" should "return the third item in a given list" in {
    val list = List("Lorem", "ipsum", "dolor", "sit", "amet")
    third2(list) should be ("dolor")
  }
  it should "throw NoSuchElementException if called on a list with fewer than 3 elements" in {
    val listOf2 = List("penultimate", "ultimate")
    a [NoSuchElementException] should be thrownBy {
      third2(listOf2)
    } 
  }
}

class AddSquaresSpec extends UnitSpec {
  "Chapter 1, Exercise 3: addSquares" should "square each item in a list and sum them" in {
    val list = List(1, 2, 5)
    addSquares(list) should be (30)
  }
  it should "return 0 if called on an empty list" in {
    val emptyList = List[Int]()
    addSquares(emptyList) should be (0)
  }
}

class BizarreFactorialSpec extends UnitSpec with TableDrivenPropertyChecks {
  "Chapter 1, Exercise 4: bizarreFactorial" should "compute the factorial, i.e., n!, of a given integer n >= 0" in {
    val factorials = Table(
      ("n", "factorial"),
      (0,   1),
      (1,   1),
      (2,   2),
      (3,   6),
      (4,  24),
      (5, 120)
    )

    forAll (factorials) { (n: Int, factorial: Int) =>
      whenever (n >= 0) {
        bizarreFactorial(n) should be (factorial)
      }
    }
  }
}

class OtherFunctionsSpec extends UnitSpec {
  "Chapter 1, Exercise 5a: take" should "create a new sequence of the first n elements of an existing sequence" in {
    1 to 10 take 3 should be (List(1, 2, 3))
  }

  "Chapter 1, Exercise 5b: distinct" should "remove duplicates from an existing sequence" in {
    val dupes = Seq(1, 2, 3, 4, 2, 3, 4, 5, 3, 4, 5, 6)
    dupes.distinct should be (1 to 6)
  }

  "Chapter 1, Exercise 5c: ++" should "concatenate two sequences together" in {
    val a = 1 to 3
    val b = 4 to 6
    a ++ b should be (1 to 6)
  }

  "Chapter 1, Exercise 5d: fill" should "create a sequence containing n copies of the same value" in {
    Seq.fill(5)(2) should be (Seq(2, 2, 2, 2, 2))
  }

  "Chapter 1, Exercise 5e: interleave" should "interleave the elements of two sequences together" in {
    val evens = 0 to 8 by 2
    val odds = 1 to 9 by 2
    evens interleave odds should be (0 to 9)
  }

  "Chapter 1, Exercise 5f.i: drop" should "remove the first n items from the sequence" in {
    1 to 10 drop 3 should be (4 to 10)
  }
  "Chapter 1, Exercise 5f.ii: dropRight" should "remove the last n items from the sequence" in {
    1 to 10 dropRight 3 should be (1 to 7)
  }
  
  "Chapter 1, Exercise 5g: flatten" should "turn a sequence of sequences into a sequence containing all of the values of each subsequence" in {
    Seq(1 to 3, 4 to 6, 7 to 9).flatten should be (1 to 9)
  }

  "Chapter 1, Exercise 5h: grouped" should "yield an iterator that turns the given sequence into a sequence of subsequences, each n items long" in {
    (1 to 9 grouped 3).toSeq should be (Seq(1 to 3, 4 to 6, 7 to 9))
  }

  "Chapter 1, Exercise 5i: forall" should "test whether all items in a sequence meet a certain condition" in {
    1 to 9 forall { _ < 10 } should be (true)
  }

  "Chapter 1, Exercise 5j: filterNot" should "remove items meeting a certain criterion from a given sequence" in {
    1 to 10 filterNot { _ % 3 == 0 } should be (Seq(1, 2, 4, 5, 7, 8, 10))
  }
}

class PrefixOfSpec extends UnitSpec {
  "Chapter 1, Exercise 6: prefixOf" should "test whether a sequence consists of the first few elements of another sequence" in {
    (1 to 3) prefixOf (1 to 10) should be (true)
  }
}

class TailsSpec extends UnitSpec {
  val seq = 1 to 4
  val expected = Seq(1 to 4, 2 to 4, 3 to 4, 4 to 4, 4 until 4)
  "Chapter 1, Exercise 7a: tails1" should "return a sequence of successively smaller subsequences of the argument" in {
    seq.tails1 should be (expected)
  }
  "Chapter 1, Exercise 7b: tails2" should "return a sequence of successively smaller subsequences of the argument" in {
    seq.tails2 should be (expected)
  }
  "Chapter 1, Exercise 7c: tails3" should "return a sequence of successively smaller subsequences of the argument" in {
    seq.tails3 should be (expected)
  }
}

