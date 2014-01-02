package fpoo

import scala.annotation.tailrec

object Chapter06 {
  def factorial(n: Int): Int = n match {
    case 0 => 1
    case _ => n * factorial(n - 1)
  }
  def factorial2(n: Int): Int = {
    @tailrec
    def fact_(something: Int, soFar: Int): Int = something match {
      case 0 => soFar
      case _ => fact_(something - 1, something * soFar)
    }
    fact_(n, 1)
  }
  @tailrec
  def sumSequence[T](seq: Seq[T], init: T)(implicit n: Numeric[T]): T = seq match {
    case Seq() => init
    case _ => sumSequence(seq.tail, n.plus(init, seq.head))
  }
  @tailrec
  def prodSequence[T](seq: Seq[T], init: T)(implicit n: Numeric[T]): T = seq match {
    case Seq() => init
    case _ => prodSequence(seq.tail, n.times(init, seq.head))
  }
  @tailrec
  def reduceSequence[A, B](op: (A, B) => B, seq: Seq[A], init: B): B = seq match {
    case Seq() => init
    case _ => reduceSequence(op, seq.tail, op(seq.head, init))
  }
}