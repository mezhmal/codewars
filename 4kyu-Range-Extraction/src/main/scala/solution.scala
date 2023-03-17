import scala.math.pow

case class Example(xs: List[Int], expected: String)

@main def solution(params: String*): Unit =
  val examples: List[Example] = List(
    Example(List(1), "1"),
    Example(List(12, 13, 15, 16, 17), "12,13,15-17"),
    Example(List(-3,-2,-1,2,10,15,16,18,19,20), "-3--1,2,10,15,16,18-20"),
    Example(List(-10, -9, -8, -6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20), "-10--8,-6,-3-1,3-5,7-11,14,15,17-20"),
  )
  for (example <- examples) {
    val result = Kata.solution(example.xs)
    println(s"${if (result == example.expected) '+' else '-'} $result ${if (result == example.expected) '=' else '!'}= ${example.expected}")
  }

object Kata {
  def solution(xs: List[Int]): String =
    var tail = xs
    var accumulator = List[List[Int]]()
    while (tail.nonEmpty) {
      val sequence = ((List(tail.head - 1) ::: tail) zip tail).takeWhile((previous, current) => previous + 1 == current).map(_._2)
      accumulator = accumulator :+ sequence
      tail = tail.drop(sequence.size)
    }
    accumulator.map(_ match {
      case single :: Nil => single.toString
      case first :: second :: Nil => s"$first,$second"
      case head :: tail => s"$head-${tail.last}"
      case _ => "?"
    }).mkString(",")
}
