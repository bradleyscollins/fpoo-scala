package fpoo

object Chapter01 {
  def second[A](list: List[A]): A = list(1)
  def third[A](list: List[A]): A = list(2)
  def third2[A](list: List[A]): A = list.tail.tail.head
  def addSquares[T](list: List[T])(implicit n: Numeric[T]): T = list.map( x => n.times(x, x) ).sum
  def bizarreFactorial(n: Int): Int = n match {
    case x if x < 0 => throw new IllegalArgumentException("Factorial only works for positive integers")
    case 0 => 1
    case _ => 1 to n product
  }
  
  implicit class Ops[A](val seq: Seq[A]) extends AnyVal {
    def interleave(that: Seq[A]): Seq[A] = {
      seq zip that flatMap { t => Seq(t._1, t._2) }
    }
    def prefixOf(that: Seq[A]): Boolean = {
      (that take seq.length) == seq
    }
    def tails2: Seq[Seq[A]] = {
      def tailn(s: Seq[A]): Seq[Seq[A]] = s match {
        case Seq() => Seq(s)
        case _ => s +: tailn(s.tail)
      }
      tailn(seq)
    }
    def tails3: Seq[Seq[A]] = {
      val seqs = Seq.fill(seq.length + 1)(seq)
      val nToDrop = 0 to seq.length
      (seqs, nToDrop).zipped map (_ drop _)
    }
  }
}