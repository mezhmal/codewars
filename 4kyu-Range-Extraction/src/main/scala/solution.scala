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
    xs.foldLeft(List[List[Int]]()) {
      case (Nil, value) => List(List(value))
      case (accumulator, value) =>
        if (accumulator.last.last + 1 == value)
          accumulator.dropRight(1) :+ (accumulator.last :+ value)
        else
          accumulator :+ List(value)
    }.map {
      case single :: Nil => single.toString
      case first :: second :: Nil => List(first, second).mkString(",")
      case head :: tail => s"$head-${tail.last}"
      case _ => "?"
    }.mkString(",")
}
