package fpoo

object Chapter06 {
  def factorial(n: Int): Int = n match {
    case 0 => 1
    case _ => n * factorial(n - 1)
  }
  def factorial2(n: Int): Int = {
    def fact_(something: Int, soFar: Int): Int = something match {
      case 0 => soFar
      case _ => fact_(something - 1, something * soFar)
    }
    fact_(n, 1)
  }
  def sumSequence[T](seq: Seq[T], init: T)(implicit n: Numeric[T]): T = seq match {
    case Seq() => init
    case _ => n.plus(seq.head, sumSequence(seq.tail, init))
  }
  def prodSequence[T](seq: Seq[T], init: T)(implicit n: Numeric[T]): T = seq match {
    case Seq() => init
    case _ => n.times(seq.head, prodSequence(seq.tail, init))
  }
  def reduceSequence[A, B](op: (A, B) => B, seq: Seq[A], init: B): B = seq match {
    case Seq() => init
    case _ => reduceSequence(op, seq.tail, op(seq.head, init))
  }
}